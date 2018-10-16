package com.sageserpent.neptunium

import java.io.FileWriter
import java.nio.charset.Charset
import java.nio.file.Paths

import com.sageserpent.americium.seqEnrichment._
import com.sageserpent.neptunium.FileProcessor2._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.yaml.syntax._
import org.log4s._
import resource._

import scala.meta.{Defn, _}
import scala.meta.inputs.Input
import scala.util.matching._

object FileProcessor {
  private[this] val logger = getLogger

  implicit class PositionSyntax(position: Position) {
    def withStart(start: Int): Position = position match {
      case Position.None     => Position.Range(position.input, start, start)
      case _: Position.Range => Position.Range(position.input, start, position.end)
    }
    def withEnd(end: Int): Position = position match {
      case Position.None     => Position.Range(position.input, end, end)
      case _: Position.Range => Position.Range(position.input, position.start, end)
    }
  }

  def discoverStructure(pathOfInputFile: String, charsetOfInputFile: String, pathOfOutputFileForYamlResult: String) {

    val input = Input.File(Paths.get(pathOfInputFile), Charset.forName(charsetOfInputFile))

    val lineMapping: LineMapping = {
      case (line: OneRelativeLineNumber, offset: ZeroRelativeOffset) => AccessWorkaround.offsetFrom(input)(line, offset)
    }

    import lineMapping._

    def positionWithEndDecrementedByOneCharacter(position: Position): Position =
      Position.Range(position.input, position.start, position.end - 1)

    def locationSpanFrom(position: Position): LocationSpan = {
      // Semantic Merge uses []-intervals (closed - closed) for line spans,
      // so we have to decrement the end position which is really one past
      // the end ; 'Position' models a [)-interval (closed, open).
      // The line indices have be bumped from zero-relative to one-relative as well.

      if (0 == position.end) LocationSpan(1 + position.startLine -> position.startColumn, 1 -> -1)
      else {
        val adjustedPosition = positionWithEndDecrementedByOneCharacter(position)

        LocationSpan(
          1 + adjustedPosition.startLine -> adjustedPosition.startColumn,
          1 + adjustedPosition.endLine   -> adjustedPosition.endColumn
        )
      }
    }

    def fileFromError(error: Parsed.Error): File = {
      val locationSpanOfEntireSource = locationSpanFrom(
        scala.meta.inputs.Position.Range(error.pos.input, 0, input.chars.size))

      File(
        "file",
        pathOfInputFile,
        locationSpanOfEntireSource,
        Span.floatingEmptySpan,
        Seq.empty,
        true,
        Seq(ParsingError(locationSpanFrom(error.pos).start, error.message))
      )
    }

    def fileFromSource(source: Source): File = {
      trait InterestingTreeData {
        val name: String
        val typeName: String
      }

      case class DefTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "def"
      }

      case class ClassTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "class"
      }

      case class ModuleTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "module"
      }

      case class PackageTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "package"
      }

      case class PositionTree(
          position: Position,
          children: Seq[PositionTree],
          interestingTreeData: Option[InterestingTreeData]
      ) {
        require(children.isEmpty || (children zip children.tail forall {
          case (predecessor, successor) =>
            predecessor.position.end <= successor.position.start
        }))
        require(
          children.isEmpty || (position.start <= children.head.position.start && children.last.position.end <= position.end)
        )

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

      val positionTree = {
        val emptyPositionTreeQueue =
          scala.collection.immutable.Queue.empty[PositionTree]

        var positionTreeQueue = emptyPositionTreeQueue

        val traverser: Traverser = new Traverser {
          override def apply(tree: Tree): Unit = if (tree.pos.end > tree.pos.start) {
            val interestingTreeData =
              PartialFunction.condOpt(tree) {
                case Defn.Def(_, name, _, _, _, _) =>
                  DefTreeData(name.value)
                case Defn.Class(_, name, _, _, _) =>
                  ClassTreeData(name.value)
                case Defn.Object(_, name, _) =>
                  ModuleTreeData(name.value)
                case Pkg(name, _) =>
                  PackageTreeData(name.toString)
              }

            val stackedPositionTreeQueue = positionTreeQueue

            positionTreeQueue = emptyPositionTreeQueue
            super.apply(tree)
            positionTreeQueue = stackedPositionTreeQueue.enqueue(
              PositionTree(
                tree.pos,
                positionTreeQueue.sortWith((lhs, rhs) => lhs.position.end <= rhs.position.start),
                interestingTreeData
              )
            )

          }
        }

        traverser.apply(source)

        positionTreeQueue.headOption.getOrElse(PositionTree(Position.Range(input, 0, 0), Seq.empty, None))
      }

      def simplifyTreePreservingInterestingBits(rootLevelPositionTree: PositionTree): PositionTree = {
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
                val fusedPosition = groupOfBoringTrees.head.position.withEnd(groupOfBoringTrees.last.position.end)
                val mergedGrandchildTrees =
                  groupOfBoringTrees.flatMap(_.children)
                PositionTree(fusedPosition, mergedGrandchildTrees, None)
            }
          val simplifiedTree = rootLevelPositionTree.copy(
            children =
              if (simplifiedChildTrees.exists(_.hasInterestingSubtrees))
                simplifiedChildTrees
              else Seq.empty
          )
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

      def absorbMissingStartOfDeclarations(rootLevelPositionTree: PositionTree) =
        if (rootLevelPositionTree.isLeaf) {
          rootLevelPositionTree
        } else {
          import rootLevelPositionTree.children
          val adjustedPositionTrees = children zip children.tail map {
            case (predecessor, successor: PositionTree) =>
              def adjustSuccessor(regex: Regex) = {
                val slice = source.pos.text
                  .slice(predecessor.position.end, successor.position.start)
                  .toString
                regex.findFirstMatchIn(slice) match {
                  case Some(hit) =>
                    successor.copy(position = successor.position.withStart(hit.start))
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

      def adjustChildPositionsToCoverTheSourceContiguously(rootLevelPositionTree: PositionTree) =
        if (rootLevelPositionTree.isLeaf) {
          rootLevelPositionTree
        } else {
          import rootLevelPositionTree.children
          val adjustedPositionTrees = children zip children.tail map {
            case (predecessor, successor) =>
              predecessor.copy(position = predecessor.position.withEnd(successor.position.start))
          }
          val adjustedChildren = adjustedPositionTrees.toList :+ children.last
          rootLevelPositionTree.copy(children = adjustedChildren)
        }

      val positionTreeWithInternalAdjustments = positionTree
        .transform(simplifyTreePreservingInterestingBits)
        .transform(squashTree)
        .transform(absorbMissingStartOfDeclarations)
        .transform(adjustChildPositionsToCoverTheSourceContiguously)

      positionTreeWithInternalAdjustments match {
        case PositionTree(rootPosition, childrenOfRoot, _) =>
          def spanFrom(position: Position): Span =
            // Semantic Merge uses []-intervals (closed - closed) for character
            // spans, so we have to decrement the end position which is really
            // one past the end; 'Position' models a [)-interval (closed, open).
            Span(position.start, position.end - 1)

          def decompose(interestingTreeData: Option[InterestingTreeData]) = {
            interestingTreeData.fold("code"                                     -> "")(
              interestingTreeDataPayload => interestingTreeDataPayload.typeName -> interestingTreeDataPayload.name
            )
          }

          def containerFrom(section: PositionTree): Container = {
            def declarationFrom(section: PositionTree): Declaration = {
              section match {
                case PositionTree(position, Seq(), interestingTreeData) =>
                  val (typeName, name) = decompose(interestingTreeData)
                  new Terminal(typeName, name, locationSpanFrom(position), spanFrom(position))
                case _ =>
                  containerFrom(section)
              }
            }

            section match {
              case PositionTree(position, children, interestingTreeData) =>
                val (headerSpan, footerSpan) = if (children.nonEmpty) {
                  val startOfFirstChild     = children.head.position.start
                  val onePastEndOfLastChild = children.last.position.end
                  spanFrom(position.withEnd(startOfFirstChild)) -> spanFrom(position.withStart(onePastEndOfLastChild))
                } else {
                  spanFrom(position) -> Span.floatingEmptySpan
                }

                val (typeName, name) = decompose(interestingTreeData)

                Container(typeName,
                          name,
                          locationSpanFrom(position),
                          headerSpan,
                          footerSpan,
                          children map declarationFrom)
            }
          }

          val errorFrom: Tuple2[Position, String] => ParsingError = {
            case (position: Position, message: String) =>
              ParsingError(1 + position.startLine -> position.startColumn, message)
          }

          File(
            "file",
            pathOfInputFile,
            if (childrenOfRoot.nonEmpty)
              LocationSpan(locationSpanFrom(childrenOfRoot.head.position).start,
                           locationSpanFrom(childrenOfRoot.last.position).end)
            else locationSpanFrom(rootPosition),
            Span.floatingEmptySpan,
            childrenOfRoot map containerFrom,
            false,
            Seq.empty
          )
      }
    }

    val yaml =
      input.parse[Source].fold(fileFromError, fileFromSource).asJson.asYaml.spaces4

    for {
      writer <- managed(new FileWriter(pathOfOutputFileForYamlResult))
    } {
      writer.write(yaml)
      writer.flush()
    }
  }
}
