package com.sageserpent.neptunium

import java.io.FileWriter
import java.nio.charset.Charset

import com.sageserpent.americium.seqEnrichment._
import io.github.classgraph.ClassGraph
import org.log4s._
import resource._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.PlainFile
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.SourceReader
import scala.tools.nsc.reporters.AbstractReporter
import scala.util.matching._

object FileProcessor {
  private[this] val logger = getLogger

  private val classGraph = new ClassGraph

  def discoverStructure(pathOfInputFile: String,
                        charsetOfInputFile: String,
                        pathOfOutputFileForYamlResult: String) {
    val sourceFile = new PlainFile(pathOfInputFile)

    val settings = new Settings((message: String) => logger.info(message))

    settings.bootclasspath.append(classGraph.getClasspath) // Voodoo required by the Scala presentation compiler.

    class CapturingReporter(val settings: Settings) extends AbstractReporter {
      override def displayPrompt() {}

      override def display(pos: Position,
                           msg: String,
                           severity: Severity): Unit = {
        severity match {
          case ERROR =>
            capturedMessages += pos -> msg
          case _ =>
        }
      }

      val capturedMessages = ListBuffer.empty[(Position, String)]
    }

    val reporter = new CapturingReporter(settings)

    val sourceReader = new SourceReader(
      Charset.forName(charsetOfInputFile).newDecoder(),
      reporter) // Need to do this to handle the likes of ScalaZ with its funky lambda characters. Hmm.

    val presentationCompiler = new Global(settings, reporter)
    val overallTree: presentationCompiler.Tree =
      presentationCompiler.parseTree(
        new BatchSourceFile(pathOfInputFile, sourceReader.read(sourceFile)))
    val parsingErrorsDetected = reporter.hasErrors

    trait InterestingTreeData {
      val name: String
      val typeName: String
    }

    case class DefTreeData(override val name: String)
        extends InterestingTreeData {
      override val typeName = "def"
    }

    case class ClassTreeData(override val name: String)
        extends InterestingTreeData {
      override val typeName = "class"
    }

    case class ModuleTreeData(override val name: String)
        extends InterestingTreeData {
      override val typeName = "module"
    }

    case class PackageTreeData(override val name: String)
        extends InterestingTreeData {
      override val typeName = "package"
    }

    case class PositionTree(position: Position,
                            children: Seq[PositionTree],
                            interestingTreeData: Option[InterestingTreeData]) {
      require(position.isOpaqueRange)
      require(children.isEmpty || (children zip children.tail forall {
        case (predecessor, successor) =>
          predecessor.position.precedes(successor.position)
      }))
      require(
        children.isEmpty || (position.start <= children.head.position.start && children.last.position.end <= position.end))

      def isLeaf = children.isEmpty

      def isInteresting = interestingTreeData.isDefined

      def hasOnlyOneChildTree = 1 == children.size

      lazy val hasInterestingSubtrees: Boolean = isInteresting || children
        .exists(_.hasInterestingSubtrees)

      def transform(transformer: PositionTree => PositionTree): PositionTree = {
        val transformedChildren = children.map(_.transform(transformer))
        transformer(this.copy(children = transformedChildren))
      }
    }

    class PositionTreeBuilder extends presentationCompiler.Traverser {
      val emptyPositionTreeQueue =
        scala.collection.immutable.Queue.empty[PositionTree]
      var positionTreeQueue = emptyPositionTreeQueue

      override def traverse(tree: presentationCompiler.Tree) = {
        if (tree.pos.isOpaqueRange) {
          val interestingTreeData =
            PartialFunction.condOpt(tree) {
              case presentationCompiler.DefDef(_, name, _, _, _, _) =>
                DefTreeData(name.toString)
              case presentationCompiler.ClassDef(_, name, _, _) =>
                ClassTreeData(name.toString)
              case presentationCompiler.ModuleDef(_, name, _) =>
                ModuleTreeData(name.toString)
              case presentationCompiler.PackageDef(pid, _) =>
                PackageTreeData(pid.toString)
            }

          val stackedPositionTreeQueue = positionTreeQueue

          positionTreeQueue = emptyPositionTreeQueue
          super.traverse(tree)
          positionTreeQueue = stackedPositionTreeQueue.enqueue(
            PositionTree(tree.pos,
                         positionTreeQueue.sortWith((lhs, rhs) =>
                           lhs.position.precedes(rhs.position)),
                         interestingTreeData))
        } else super.traverse(tree)
      }
    }

    val positionTreeBuilder = new PositionTreeBuilder()

    positionTreeBuilder.traverse(overallTree)

    val positionTree = positionTreeBuilder.positionTreeQueue.head

    def simplifyTreePreservingInterestingBits(
        rootLevelPositionTree: PositionTree): PositionTree = {
      if (rootLevelPositionTree.isLeaf) rootLevelPositionTree
      else {
        val childTreesWithoutBoringOutliers = rootLevelPositionTree.children
          .dropWhile(!_.hasInterestingSubtrees)
          .reverse
          .dropWhile(!_.hasInterestingSubtrees)
          .reverse

        val simplifiedChildTrees =
          childTreesWithoutBoringOutliers groupWhile {
            case (predecessor, successor) =>
              !(predecessor.isInteresting || successor.isInteresting)
          } map {
            case Seq(singleton) => singleton
            case groupOfBoringTrees =>
              assert(groupOfBoringTrees.forall(!_.isInteresting))
              val fusedPosition = groupOfBoringTrees.head.position.pos
                .withEnd(groupOfBoringTrees.last.position.pos.end)
              val mergedGrandchildTrees =
                groupOfBoringTrees.flatMap(_.children)
              PositionTree(fusedPosition, mergedGrandchildTrees, None)
          }
        val simplifiedTree = rootLevelPositionTree.copy(
          children =
            if (simplifiedChildTrees.exists(_.hasInterestingSubtrees))
              simplifiedChildTrees
            else Seq.empty)
        simplifiedTree
      }
    }

    def squashTree(rootLevelPositionTree: PositionTree): PositionTree = {
      if (rootLevelPositionTree.isLeaf) rootLevelPositionTree
      else {
        val onlyChildTree = rootLevelPositionTree.children.head
        if (rootLevelPositionTree.hasOnlyOneChildTree && !onlyChildTree.isInteresting) {
          rootLevelPositionTree.copy(children = onlyChildTree.children)
        } else rootLevelPositionTree
      }
    }

    val source = overallTree.pos.source

    def absorbMissingStartOfDeclarations(rootLevelPositionTree: PositionTree) =
      if (rootLevelPositionTree.isLeaf) {
        rootLevelPositionTree
      } else {
        import rootLevelPositionTree.children
        val adjustedPositionTrees = children zip children.tail map {
          case (predecessor, successor: PositionTree) =>
            def adjustSuccessor(regex: Regex) = {
              val slice = source.content
                .slice(predecessor.position.pos.end,
                       successor.position.pos.start)
                .toString
              regex.findFirstMatchIn(slice) match {
                case Some(hit) =>
                  successor.copy(
                    position = successor.position.withStart(hit.start))
                case None => successor
              }
            }

            successor match {
              case PositionTree(_, _, Some(DefTreeData(_))) =>
                adjustSuccessor("""\s*def\s*$""".r)
              case PositionTree(_, _, Some(ClassTreeData(_))) =>
                adjustSuccessor("""\s*((abstract|case)s*+)?class\s*$""".r)
              case _ => successor
            }
        }
        val adjustedChildren = children.head +: adjustedPositionTrees.toList
        rootLevelPositionTree.copy(children = adjustedChildren)
      }

    def adjustChildPositionsToCoverTheSourceContiguously(
        rootLevelPositionTree: PositionTree) =
      if (rootLevelPositionTree.isLeaf) {
        rootLevelPositionTree
      } else {
        import rootLevelPositionTree.children
        val adjustedPositionTrees = children zip children.tail map {
          case (predecessor, successor) =>
            predecessor.copy(
              position =
                predecessor.position.withEnd(successor.position.pos.start))
        }
        val adjustedChildren = adjustedPositionTrees.toList :+ children.last
        rootLevelPositionTree.copy(children = adjustedChildren)
      }

    val positionTreeWithInternalAdjustments = positionTree.transform(
      simplifyTreePreservingInterestingBits _ andThen squashTree andThen absorbMissingStartOfDeclarations andThen adjustChildPositionsToCoverTheSourceContiguously)

    val startOfSource      = 0
    val onePastEndOfSource = source.length

    val positionTreeCoveringEntireSource =
      positionTreeWithInternalAdjustments.copy(
        position = Position
          .range(source, startOfSource, startOfSource, onePastEndOfSource))

    val yamlFrom: PositionTree => String = {
      case PositionTree(rootPosition, childrenOfRoot, _) =>
        def lineAndColumnFor(position: Position,
                             offsetFrom: Position => Int) = {
          val offset = offsetFrom(position)
          if (offset < position.source.length) {
            val zeroRelativeLine = position.source.offsetToLine(offset)
            val line             = 1 + zeroRelativeLine
            // Semantic Merge uses one-relative line numbers...
            val offsetOfStartOfLine =
              position.source.lineToOffset(zeroRelativeLine)
            val column = offset - offsetOfStartOfLine // ... but zero-relative column numbers.
            line -> column
          } else {
            val zeroRelativeLine = 1 + position.source.offsetToLine(
              position.source.length - 1)
            val line = 1 + zeroRelativeLine
            // Semantic Merge uses one-relative line numbers...
            val column = 0
            line -> column // ... but zero-relative column numbers.
          }
        }

        def yamlForLineSpan(position: Position) = {
          // Semantic Merge uses []-intervals (closed - closed) for line spans,
          // so we have to decrement the end position which is really one past
          // the end ; 'Position' models a [)-interval (closed, open).
          val (startLine, startColumn) = lineAndColumnFor(position, _.start)
          val (endLine, endColumn)     = lineAndColumnFor(position, _.end - 1)
          s"{start: [$startLine,$startColumn], end: [$endLine,$endColumn]}"
        }

        def yamlForCharacterSpan(position: Position) =
          // Semantic Merge uses []-intervals (closed - closed) for character
          // spans, so we have to decrement the end position which is really
          // one past the end; 'Position' models a [)-interval (closed, open).
          s"[${position.start}, ${position.end - 1}]"

        val yamlForEmptyCharacterSpan = "[0, -1]"

        def indent(indentationLevel: Int)(line: String) =
          " " * indentationLevel + line

        val indentPieces = (_: Iterable[String]).map(indent(2))

        def joinPiecesOnSeparateLines(pieces: Iterable[String]) =
          String.join("\n", pieces.asJava)

        def yamlForSubpieces(yamlForSubpiece: PositionTree => Iterable[String],
                             pieces: Iterable[PositionTree]): Iterable[String] =
          pieces.flatMap(yamlForSubpiece andThen indentPieces)

        def decompose(interestingTreeData: Option[InterestingTreeData]) = {
          interestingTreeData.fold("code"       -> "")(interestingTreeDataPayload =>
            interestingTreeDataPayload.typeName -> interestingTreeDataPayload.name)
        }

        def yamlForSection(section: PositionTree): Iterable[String] = {
          def yamlForContainer(position: Position,
                               children: Iterable[PositionTree],
                               interestingTreeData: Option[InterestingTreeData])
            : Iterable[String] = {
            require(children.nonEmpty)
            val startOfFirstChild     = children.head.position.pos.start
            val onePastEndOfLastChild = children.last.position.pos.end
            val headerSpan            = position.withEnd(startOfFirstChild)
            val footerSpan            = position.withStart(onePastEndOfLastChild)
            val (typeName, name)      = decompose(interestingTreeData)
            Iterable(
              s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(position)}",
              s"  headerSpan : ${yamlForCharacterSpan(headerSpan)}",
              s"  footerSpan : ${yamlForCharacterSpan(footerSpan)}"
            ) ++ (if (children.nonEmpty)
                    Iterable("  children :") ++ yamlForSubpieces(yamlForSection,
                                                                 children)
                  else
                    Iterable.empty[String])
          }

          def yamlForTerminal(terminalPosition: Position,
                              interestingTreeData: Option[InterestingTreeData])
            : Iterable[String] = {
            val (typeName, name) = decompose(interestingTreeData)
            Iterable(
              s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(terminalPosition)}",
              s"  span : ${yamlForCharacterSpan(terminalPosition)}"
            )
          }

          section match {
            case PositionTree(position, Seq(), interestingTreeData) =>
              yamlForTerminal(position, interestingTreeData)
            case PositionTree(position, children, interestingTreeData) =>
              yamlForContainer(position, children, interestingTreeData)
          }
        }

        val yamlForError: ((Position, String)) => Iterable[String] = {
          case ((position: Position), (message: String)) =>
            val (startLine, startColumn) = lineAndColumnFor(position, _.start)
            Iterable(s"- location: [$startLine,$startColumn]",
                     s"""  message: "$message"""")
        }

        val pieces: Iterable[String] = Iterable(
          "---",
          "type : file",
          s"name : $pathOfInputFile",
          s"locationSpan : ${yamlForLineSpan(rootPosition.pos)}",
          s"footerSpan : $yamlForEmptyCharacterSpan",
          s"parsingErrorsDetected : $parsingErrorsDetected"
        ) ++
          (if (childrenOfRoot.nonEmpty)
             Iterable("children :") ++ yamlForSubpieces(yamlForSection,
                                                        childrenOfRoot)
           else
             Iterable.empty[String]) ++
          (if (parsingErrorsDetected)
             Iterable("parsingErrors :") ++ (reporter.capturedMessages flatMap (yamlForError andThen indentPieces))
           else
             Iterable.empty)

        joinPiecesOnSeparateLines(pieces)
    }

    val yaml = yamlFrom(positionTreeCoveringEntireSource)

    for {
      writer <- managed(new FileWriter(pathOfOutputFileForYamlResult))
    } {
      writer.write(yaml)
      writer.flush()
    }
  }
}
