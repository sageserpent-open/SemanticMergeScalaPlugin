package com.sageSerpent.neptunium


import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.PlainFile
import scala.tools.nsc.Settings

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.AbstractReporter


object FileProcessor {
  def discoverStructure(pathOfInputFile: String, pathOfOutputFileForYamlResult: String) {
    val sourceFile = new PlainFile(pathOfInputFile)

    val settings = new Settings()

    val classPath = System.getProperty("java.class.path")

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

    val presentationCompiler: Global = new Global(settings, reporter)
    val overallTree: presentationCompiler.Tree = presentationCompiler.parseTree(new BatchSourceFile(sourceFile))
    val parsingErrorsDetected = reporter.hasErrors

    val startOfFileCharacterIndex = 0

    val onePastEndOfFileCharacterIndex = overallTree.pos.source.length

    case class PositionTree(position: Position, children: Iterable[PositionTree]) {
      def transform(transformer: PositionTree => PositionTree): PositionTree = {
        val transformedChildren = children.map(_.transform(transformer))
        transformer(this.copy(children = transformedChildren))
      }
    }

    class PositionTreeBuilder extends presentationCompiler.Traverser {
      var positionTreeStack = scala.collection.immutable.Queue[PositionTree]()

      override def traverse(tree: presentationCompiler.Tree) = {
        if (tree.pos.isRange) {
          val preservedSpanTreeStack = positionTreeStack
          try {
            positionTreeStack = scala.collection.immutable.Queue.empty
            super.traverse(tree)
          } finally {
            positionTreeStack = preservedSpanTreeStack.enqueue(PositionTree(tree.pos, positionTreeStack))
          }
        }
      }
    }

    val positionTreeBuilder = new PositionTreeBuilder()

    positionTreeBuilder.traverse(overallTree)

    val positionTree = positionTreeBuilder.positionTreeStack.head

    val adjustChildPositionsToCoverTheSourceContiguously: PositionTree => PositionTree = {
      case positionTree@PositionTree(position, Seq()) => positionTree
      case PositionTree(position, children) => {
        {
          val pairsOfPositionTreeAndOnePastItsEndAfterAdjustment = children.sliding(2).filter(2 == _.size).map { case Seq(predecessor, successor) => predecessor -> successor.position.pos.start }
          val adjustedPositionTrees = pairsOfPositionTreeAndOnePastItsEndAfterAdjustment.map { case (positionTree, onePastTheEndAfterAdjustment) => positionTree.copy(position = positionTree.position.withEnd(onePastTheEndAfterAdjustment)) }
          val adjustedChildren = adjustedPositionTrees.toList :+ children.last
          PositionTree(position, adjustedChildren)
        }
      }
    }

    val adjustPositionToBeConsistentWithItsChildren: PositionTree => PositionTree = {
      case positionTree@PositionTree(position, Seq()) => positionTree
      case PositionTree(position, children) => {
        val startOfFirstChild = children.head.position.pos.start
        val onePastEndOfLastChild = children.last.position.pos.end
        val positionConsistentWithChildren = position.withStart(Ordering[Int].min(position.start, startOfFirstChild)).withEnd(Ordering[Int].max(position.end, onePastEndOfLastChild))
        PositionTree(positionConsistentWithChildren, children)
      }
    }

    val positionTreeWithInternalAdjustments = positionTree.transform(adjustChildPositionsToCoverTheSourceContiguously andThen adjustPositionToBeConsistentWithItsChildren)

    val yamlFrom: PositionTree => String = {
      case PositionTree(rootPosition, childrenOfRoot) =>
        def lineAndColumnFor(position: Position, offsetFrom: Position => Int) = {
          val offset = offsetFrom(position)
          val zeroRelativeLine = position.source.offsetToLine(offset)
          val line = 1 + zeroRelativeLine // Semantic Merge uses one-relative line numbers...
          val offsetOfStartOfLine = position.source.lineToOffset(zeroRelativeLine)
          val column = offset - offsetOfStartOfLine // ... but zero-relative column numbers.
          line -> column
        }
        def yamlForLineSpan(position: Position) = {
          // Semantic Merge uses [)-intervals (closed - open) for line spans
          // (but see below about character spans). The examples from Codice
          // show that if the line span ends just after a linebreak, then it
          // either points one past the end on the same line, or points to the
          // zeroeth character position on the next line, this depends on whether
          // the construct is contained within one line or spans several lines.

          val (startLine, startColumn) = lineAndColumnFor(position, _.start)
          val (endLine, endColumn) = lineAndColumnFor(position, _.end)
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

        def yamlForSection(section: PositionTree): Iterable[String] = {
          def yamlForContainer(position: Position, children: Iterable[PositionTree]): Iterable[String] = {
            require(children.nonEmpty)
            val typeName = "Section - TODO"
            val name = "TODO"
            val startOfFirstChild = children.head.position.pos.start
            val onePastEndOfLastChild = children.last.position.pos.end
            val headerSpan = position.withEnd(startOfFirstChild)
            val footerSpan = position.withStart(onePastEndOfLastChild)
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

          def yamlForTerminal(terminalPosition: Position): Iterable[String] = {
            val typeName = "Terminal - TODO"
            val name = "TODO"
            Iterable(s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(terminalPosition)}",
              s"  span : ${yamlForCharacterSpan(terminalPosition)}")
          }

          section match {
            case PositionTree(position, Seq()) => yamlForTerminal(position)
            case PositionTree(position, children) => yamlForContainer(position, children)
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
          //s"footerSpan : ${yamlForCharacterSpan(rootPosition.pos.withStart(startOfFileCharacterIndex).withEnd(onePastEndOfFileCharacterIndex))}",
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

    val yaml = yamlFrom(positionTreeWithInternalAdjustments)

    scala.reflect.io.File(pathOfOutputFileForYamlResult).writeAll(yaml)
  }
}
