package com.sageserpent.neptunium

import java.io.FileWriter
import java.nio.charset.Charset
import java.nio.file.Paths

import com.sageserpent.americium.seqEnrichment._
import com.sageserpent.neptunium.YamlModel._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.yaml.syntax._
import resource._

import scala.meta.Pat.Var
import scala.meta.inputs.Input
import scala.meta.{Defn, _}
import scala.util.matching.Regex

object FileProcessor {
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

  val edgeWhitespaceWithCoreTextRegex: Regex = """(?s)\A\s*(\S(?:.*\S)?)\s*\z""".r

  val upToAndIncludingTheFirstLinebreakRegex: Regex = """(?sm)\A.+?^""".r

  def discoverStructure(pathOfInputFile: String, charsetOfInputFile: String, pathOfOutputFileForYamlResult: String) {
    val input = Input.File(Paths.get(pathOfInputFile), Charset.forName(charsetOfInputFile))

    val lineMapping: LineMapping = new LineMapping {
      override def offsetFrom(lineAndOffSet: (OneRelativeLineNumber, ZeroRelativeOffset)): ZeroRelativeCharacterIndex =
        AccessWorkaround.offsetFrom(input)(lineAndOffSet._1, lineAndOffSet._2)
      override val numberOfCharacters: ZeroRelativeOffset = input.chars.length
    }

    import lineMapping._

    def positionWithEndDecrementedByOneCharacter(position: Position): Position =
      Position.Range(position.input, position.start, position.end - 1)

    def locationSpanFrom(position: Position): LocationSpan = {
      // Semantic Merge uses []-intervals (closed - closed) for line spans,
      // so we have to decrement the end position which is really one past
      // the end ; 'Position' models a [)-interval (closed, open).
      // The line indices have been bumped from zero-relative to one-relative as well.

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
        scala.meta.inputs.Position.Range(error.pos.input, 0, numberOfCharacters)
      )

      File(
        "file",
        pathOfInputFile,
        locationSpanOfEntireSource,
        spanOf(locationSpanOfEntireSource),
        Seq.empty,
        parsingErrorsDetected = true,
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

      case class ValTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "val"
      }

      case class ClassTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "class"
      }

      case class ModuleTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "module"
      }

      case class TypeTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "type"
      }

      case class ExpressionTreeData(override val name: String) extends InterestingTreeData {
        override val typeName = "expression"
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

        def isLeaf: Boolean = children.isEmpty

        def isInteresting: Boolean = interestingTreeData.isDefined

        def hasOnlyOneChildTree: Boolean = 1 == children.size

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

        var valsAndExpressionsAreImportant = true

        val traverser: Traverser = new Traverser {
          override def apply(tree: Tree): Unit = if (tree.pos.end > tree.pos.start) {
            val interestingTreeData =
              PartialFunction.condOpt(tree) {
                case Decl.Def(_, name, _, _, _) =>
                  DefTreeData(name.value)
                case Defn.Def(_, name, _, _, _, _) =>
                  DefTreeData(name.value)
                case Decl.Val(_, List(Var(name)), _) if valsAndExpressionsAreImportant =>
                  ValTreeData(name.value)
                case Defn.Val(_, List(Var(name)), _, _) if valsAndExpressionsAreImportant =>
                  ValTreeData(name.value)
                case Term.ApplyInfix(_, name, _, _) if valsAndExpressionsAreImportant =>
                  ExpressionTreeData(name.value)
                case Defn.Class(_, name, _, _, _) =>
                  ClassTreeData(name.value)
                case Defn.Trait(_, name, _, _, _) =>
                  ClassTreeData(name.value)
                case Defn.Object(_, name, _) =>
                  ModuleTreeData(name.value)
                case Defn.Type(_, name, _, _) =>
                  TypeTreeData(name.value)
              }

            val stackedPositionTreeQueue              = positionTreeQueue
            val stackedValsAndExpressionsAreImportant = valsAndExpressionsAreImportant

            positionTreeQueue = emptyPositionTreeQueue
            valsAndExpressionsAreImportant = interestingTreeData.fold(valsAndExpressionsAreImportant) {
              case ClassTreeData(_)      => true
              case ModuleTreeData(_)     => true
              case DefTreeData(_)        => false
              case ValTreeData(_)        => false
              case ExpressionTreeData(_) => false
              case _                     => valsAndExpressionsAreImportant
            }

            super.apply(tree)

            positionTreeQueue = stackedPositionTreeQueue.enqueue(
              PositionTree(
                tree.pos,
                positionTreeQueue.sortWith((lhs, rhs) => lhs.position.end <= rhs.position.start),
                interestingTreeData
              )
            )
            valsAndExpressionsAreImportant = stackedValsAndExpressionsAreImportant
          }
        }

        traverser.apply(source)

        positionTreeQueue.headOption.getOrElse(PositionTree(source.pos, Seq.empty, None))
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
        else
          rootLevelPositionTree.children match {
            case Seq(onlyChild) if !onlyChild.isInteresting => rootLevelPositionTree.copy(children = onlyChild.children)
            case _                                          => rootLevelPositionTree
          }
      }

      def loseEdgeWhitespace(rootLevelPositionTree: PositionTree): PositionTree =
        if (rootLevelPositionTree.isLeaf) {
          rootLevelPositionTree
        } else {
          import rootLevelPositionTree.children
          val adjustedChildren = children flatMap (child =>
            if (child.isInteresting) {
              edgeWhitespaceWithCoreTextRegex.findFirstMatchIn(child.position.text) match {
                case Some(hit) =>
                  val coreTextStart   = hit.start(1)
                  val onePastCoreText = hit.end(1)
                  Some(
                    child.copy(
                      position = child.position
                        .withStart(child.position.start + coreTextStart)
                        .withEnd(child.position.start + onePastCoreText)))
                case None =>
                  // This is the case where the entire text is just whitespace. This isn't expected to occur
                  // for an interesting construct, but is put here to avoid a compiler warning about incomplete
                  // pattern matching.
                  None
              }
            } else Some(child))

          rootLevelPositionTree.copy(children = adjustedChildren)
        }

      def adjustChildPositionsToCoverTheSourceContiguously(rootLevelPositionTree: PositionTree): PositionTree =
        if (rootLevelPositionTree.isLeaf || rootLevelPositionTree.hasOnlyOneChildTree) {
          rootLevelPositionTree
        } else {
          import rootLevelPositionTree.children
          val adjustedSuccessorChildren = children zip children.tail map {
            case (predecessor, successor) =>
              if (successor.isInteresting) {
                val gapText = source.pos.text.slice(predecessor.position.end, successor.position.start)
                upToAndIncludingTheFirstLinebreakRegex.findFirstMatchIn(gapText) match {
                  case Some(hit) =>
                    val onePastTheFirstNewlineInTheGap = hit.end
                    successor.copy(position =
                      successor.position.withStart(predecessor.position.end + onePastTheFirstNewlineInTheGap))
                  case None => successor.copy(position = successor.position.withStart(predecessor.position.end))
                }
              } else { successor }
          }

          val adjustedPredecessorChildren = (children.head +: adjustedSuccessorChildren) zip adjustedSuccessorChildren map {
            case (predecessor, successor) =>
              predecessor.copy(position = predecessor.position.withEnd(successor.position.start))
          }

          rootLevelPositionTree.copy(children = adjustedPredecessorChildren :+ adjustedSuccessorChildren.last)
        }

      val positionTreeWithInternalAdjustments = positionTree
        .transform(simplifyTreePreservingInterestingBits)
        .transform(squashTree)
        .transform(loseEdgeWhitespace)
        .transform(adjustChildPositionsToCoverTheSourceContiguously) match {
        case rootLevelPositionTree =>
          rootLevelPositionTree.copy(
            position = source.pos,
            children = rootLevelPositionTree.children match {
              case empty @ Seq()  => empty
              case Seq(singleton) => Seq(singleton.copy(position = source.pos))
              case twoOrMoreChildren =>
                val (Seq(head), remainder) = twoOrMoreChildren.splitAt(1)
                val (flanked, Seq(last))   = remainder.splitAt(remainder.size - 1)
                head.copy(position = head.position.withStart(source.pos.start)) +: flanked :+
                  last.copy(position = last.position.withEnd(source.pos.end))
            }
          )
      }

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

          def declarationFrom(section: PositionTree): Declaration = {
            section match {
              case PositionTree(position, Seq(), interestingTreeData) =>
                val (typeName, name) = decompose(interestingTreeData)
                new Terminal(typeName, name, locationSpanFrom(position), spanFrom(position))
              case PositionTree(position, children, interestingTreeData) =>
                val (headerSpan, footerSpan) = if (children.nonEmpty) {
                  val startOfFirstChild     = children.head.position.start
                  val onePastEndOfLastChild = children.last.position.end
                  spanFrom(position.withEnd(startOfFirstChild)) -> spanFrom(position.withStart(onePastEndOfLastChild))
                } else {
                  spanFrom(position) -> Span.floatingEmptySpan
                }

                val (typeName, name) = decompose(interestingTreeData)

                Container(
                  typeName,
                  name,
                  locationSpanFrom(position),
                  headerSpan,
                  footerSpan,
                  children map declarationFrom
                )
            }
          }

          File(
            "file",
            pathOfInputFile,
            if (childrenOfRoot.nonEmpty)
              LocationSpan(
                locationSpanFrom(childrenOfRoot.head.position).start,
                locationSpanFrom(childrenOfRoot.last.position).end
              )
            else locationSpanFrom(rootPosition),
            Span.floatingEmptySpan,
            childrenOfRoot map declarationFrom,
            parsingErrorsDetected = false,
            Seq.empty
          )
      }
    }

    val yaml =
      (if (pathOfInputFile.endsWith(".sbt") || pathOfInputFile.endsWith(".sc")) {
         dialects.Sbt0137(input).parse[Source]
       } else { input.parse[Source] })
        .fold(fileFromError, fileFromSource)
        .asJson
        .asYaml
        .spaces4

    for {
      writer <- managed(new FileWriter(pathOfOutputFileForYamlResult))
    } {
      writer.write(yaml)
      writer.flush()
    }
  }
}
