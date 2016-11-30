package com.sageserpent.neptunium


import java.io.{FileOutputStream, FileWriter, OutputStreamWriter}
import java.nio.file.Path

import org.log4s._
import resource._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.PlainFile
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.AbstractReporter


object FileProcessor {
  private[this] val logger = getLogger

  def discoverStructure(jarProvidingThisCode: Path)(pathOfInputFile: String, pathOfOutputFileForYamlResult: String) {
    val sourceFile = new PlainFile(pathOfInputFile)

    val settings = new Settings((message: String) => logger.info(message))

    val classPath = jarProvidingThisCode.toString

    settings.bootclasspath.append(classPath) // Voodoo required by the Scala presentation compiler.

    class CapturingReporter(val settings: Settings) extends AbstractReporter {
      override def displayPrompt() {}

      override def display(pos: Position, msg: String, severity: Severity): Unit = {
        severity match {
          case ERROR =>
            capturedMessages += pos -> msg
          case _ =>
        }
      }

      val capturedMessages = ListBuffer.empty[(Position, String)]
    }

    val reporter = new CapturingReporter(settings)

    val presentationCompiler = new Global(settings, reporter)
    val overallTree: presentationCompiler.Tree = presentationCompiler.parseTree(new BatchSourceFile(sourceFile))
    val parsingErrorsDetected = reporter.hasErrors

    val sourceText = String.copyValueOf(overallTree.pos.source.content)

    case class InterestingTreeData(typeName: String, name: String)

    case class PositionTree(position: Position, children: Seq[PositionTree], interestingTreeData: Option[InterestingTreeData]) {
      require(position.isOpaqueRange)
      require(children.isEmpty || (children zip children.tail forall { case (predecessor, successor) => predecessor.position.precedes(successor.position) }))

      def isLeaf = children.isEmpty

      def transform(transformer: PositionTree => PositionTree): PositionTree = {
        val transformedChildren = children.map(_.transform(transformer))
        transformer(this.copy(children = transformedChildren))
      }
    }

    class PositionTreeBuilder extends presentationCompiler.Traverser {
      val emptyPositionTreeQueue = scala.collection.immutable.Queue.empty[PositionTree]
      var positionTreeQueue = emptyPositionTreeQueue

      override def traverse(tree: presentationCompiler.Tree) = {
        if (tree.pos.isOpaqueRange) {
          val interestingTreeData =
            PartialFunction.condOpt(tree) {
              case presentationCompiler.DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
                InterestingTreeData("Def", name.toString)
              case presentationCompiler.ClassDef(mods, name, tparams, impl) =>
                InterestingTreeData("Class", name.toString)
              case presentationCompiler.ModuleDef(mods, name, impl) =>
                InterestingTreeData("Module", name.toString)
              case presentationCompiler.PackageDef(pid, stats) =>
                InterestingTreeData("Package", pid.toString)
            }

          val stackedPositionTreeQueue = positionTreeQueue

          positionTreeQueue = emptyPositionTreeQueue
          super.traverse(tree)
          positionTreeQueue = stackedPositionTreeQueue.enqueue(PositionTree(tree.pos, positionTreeQueue.sortWith(((lhs, rhs) => lhs.position.precedes(rhs.position))), interestingTreeData))
        }
        else super.traverse(tree)
      }
    }

    val positionTreeBuilder = new PositionTreeBuilder()

    positionTreeBuilder.traverse(overallTree)

    val positionTree = positionTreeBuilder.positionTreeQueue.head

    def squashTreePreservingInterestingBits(rootLevelPositionTree: PositionTree): PositionTree =
      if (rootLevelPositionTree.isLeaf) {
        rootLevelPositionTree
      } else {
        val squashedRootLevelPositionTree = rootLevelPositionTree.copy(children = rootLevelPositionTree.children flatMap {
          case interestingChildPositionTree@PositionTree(_, _, Some(_)) => Seq(interestingChildPositionTree)
          case PositionTree(_, grandChildrenOfRootLevelPositionTree, None) => grandChildrenOfRootLevelPositionTree
        })
        assert(squashedRootLevelPositionTree.children.forall(_.interestingTreeData.isDefined))
        squashedRootLevelPositionTree
      }

    val squashedPositionTree = positionTree.transform(squashTreePreservingInterestingBits)

    def adjustChildPositionsToCoverTheSourceContiguously(rootLevelPositionTree: PositionTree) =
      if (rootLevelPositionTree.isLeaf) {
        rootLevelPositionTree
      } else {
        import rootLevelPositionTree.children
        val pairsOfPositionTreeAndOnePastItsEndAfterAdjustment = children zip children.tail map { case (predecessor, successor) => predecessor -> successor.position.pos.start }
        val adjustedPositionTrees = pairsOfPositionTreeAndOnePastItsEndAfterAdjustment.map { case (positionTree, onePastTheEndAfterAdjustment) => positionTree.copy(position = positionTree.position.withEnd(onePastTheEndAfterAdjustment)) }
        val adjustedChildren = adjustedPositionTrees.toList :+ children.last
        rootLevelPositionTree.copy(children = adjustedChildren)
      }

    val positionTreeWithInternalAdjustments = squashedPositionTree.transform(adjustChildPositionsToCoverTheSourceContiguously)


    val source = overallTree.pos.source

    val startOfSource = 0
    val onePastEndOfSource = source.length

    val fragmentToPadOutFromStartOfSource = positionTreeWithInternalAdjustments match {
      case PositionTree(position, children, _) if children.nonEmpty =>
        val startOfPositionTree = children.head.position.start
        if (startOfPositionTree > startOfSource)
          Some(PositionTree(Position.range(source, startOfSource, startOfSource, startOfPositionTree), Seq.empty, None))
        else
          None
      case _ => None
    }

    val fragmentToPadOutToEndOfSource = positionTreeWithInternalAdjustments match {
      case PositionTree(position, children, _) if children.nonEmpty =>
        val onePastEndOfPositionTree = children.last.position.end
        if (onePastEndOfPositionTree < onePastEndOfSource)
          Some(PositionTree(Position.range(source, onePastEndOfPositionTree, onePastEndOfPositionTree, onePastEndOfSource), Seq.empty, None))
        else
          None
      case _ => None
    }

    val positionTreeCoveringEntireSource = fragmentToPadOutFromStartOfSource -> fragmentToPadOutToEndOfSource match {
      case (Some(fragmentToPadOutFromStartOfSource), Some(fragmentToPadOutToEndOfSource)) =>
        positionTreeWithInternalAdjustments.copy(children = fragmentToPadOutFromStartOfSource +: positionTreeWithInternalAdjustments.children.toList :+ fragmentToPadOutToEndOfSource)
      case (Some(fragmentToPadOutFromStartOfSource), None) =>
        positionTreeWithInternalAdjustments.copy(children = fragmentToPadOutFromStartOfSource +: positionTreeWithInternalAdjustments.children.toList)
      case (None, Some(fragmentToPadOutToEndOfSource)) =>
        positionTreeWithInternalAdjustments.copy(children = positionTreeWithInternalAdjustments.children.toList :+ fragmentToPadOutToEndOfSource)
      case (None, None) => positionTreeWithInternalAdjustments
    }

    val yamlFrom: PositionTree => String = {
      case PositionTree(rootPosition, childrenOfRoot, _) =>
        def lineAndColumnFor(position: Position, offsetFrom: Position => Int) = {
          val offset = offsetFrom(position)
          if (offset < position.source.length) {
            val zeroRelativeLine = position.source.offsetToLine(offset)
            val line = 1 + zeroRelativeLine
            // Semantic Merge uses one-relative line numbers...
            val offsetOfStartOfLine = position.source.lineToOffset(zeroRelativeLine)
            val column = offset - offsetOfStartOfLine // ... but zero-relative column numbers.
            line -> column
          } else {
            val zeroRelativeLine = 1 + position.source.offsetToLine(position.source.length - 1)
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
          val (endLine, endColumn) = lineAndColumnFor(position, (_.end - 1))
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

        def yamlForSubpieces(yamlForSubpiece: PositionTree => Iterable[String], pieces: Iterable[PositionTree]): Iterable[String] =
          pieces.flatMap(yamlForSubpiece andThen indentPieces)

        val fallbackForLackOfInterestingTreeData = InterestingTreeData("Code", "")

        def yamlForSection(section: PositionTree): Iterable[String] = {
          def yamlForContainer(position: Position, children: Iterable[PositionTree], interestingTreeData: Option[InterestingTreeData]): Iterable[String] = {
            require(children.nonEmpty)
            val startOfFirstChild = children.head.position.pos.start
            val onePastEndOfLastChild = children.last.position.pos.end
            val headerSpan = position.withEnd(startOfFirstChild)
            val footerSpan = position.withStart(onePastEndOfLastChild)
            val InterestingTreeData(typeName, name) = interestingTreeData.getOrElse(fallbackForLackOfInterestingTreeData)
            Iterable(s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(position)}",
              s"  headerSpan : ${yamlForCharacterSpan(headerSpan)}",
              s"  footerSpan : ${yamlForCharacterSpan(footerSpan)}") ++ (
              if (children.nonEmpty)
                Iterable("  children :") ++ yamlForSubpieces(yamlForSection, children)
              else
                Iterable.empty[String])
          }

          def yamlForTerminal(terminalPosition: Position, interestingTreeData: Option[InterestingTreeData]): Iterable[String] = {
            val InterestingTreeData(typeName, name) = interestingTreeData.getOrElse(fallbackForLackOfInterestingTreeData)
            Iterable(s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(terminalPosition)}",
              s"  span : ${yamlForCharacterSpan(terminalPosition)}")
          }

          section match {
            case PositionTree(position, Seq(), interestingTreeData) => yamlForTerminal(position, interestingTreeData)
            case PositionTree(position, children, interestingTreeData) => yamlForContainer(position, children, interestingTreeData)
          }
        }

        val yamlForError: ((Position, String)) => Iterable[String] = {
          case ((position: Position), (message: String)) =>
            val (startLine, startColumn) = lineAndColumnFor(position, _.start)
            Iterable(s"- location: [$startLine,$startColumn]",
              s"""  message: "$message"""")
        }

        val pieces: Iterable[String] = Iterable("---",
          "type : file",
          s"name : $pathOfInputFile",
          s"locationSpan : ${yamlForLineSpan(rootPosition.pos)}",
          s"footerSpan : $yamlForEmptyCharacterSpan",
          s"parsingErrorsDetected : $parsingErrorsDetected") ++
          (if (childrenOfRoot.nonEmpty)
            Iterable("children :") ++ yamlForSubpieces(yamlForSection, childrenOfRoot)
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
