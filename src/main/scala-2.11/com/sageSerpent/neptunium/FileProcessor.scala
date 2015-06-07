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
    val tree: presentationCompiler.Tree = presentationCompiler.parseTree(new BatchSourceFile(sourceFile))
    val parsingErrorsDetected = reporter.hasErrors

    case class SpanTree(tree: presentationCompiler.Tree, children: Iterable[SpanTree]) {
      def content = tree.pos.source.content.slice(tree.pos.start, tree.pos.end)

      def transform(transformer: SpanTree => SpanTree) = {
        val transformedChildren = children.map(transformer)
        transformer(this.copy(children = transformedChildren))
      }

      def yaml = {
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
        def yamlForSubpieces(yamlForSubpiece: SpanTree => Iterable[String], pieces: Iterable[SpanTree]): Iterable[String] =
          pieces.flatMap(yamlForSubpiece andThen indentPieces)

        def yamlForSection(section: SpanTree): Iterable[String] = {
          def yamlForContainer(container: presentationCompiler.Tree, children: Iterable[SpanTree]): Iterable[String] = {
            require(children.nonEmpty)
            val typeName = "Section - TODO"
            val name = "TODO"
            val containerPosition = container.pos
            val headerSpan = containerPosition.withEnd(children.head.tree.pos.start)
            val footerSpan = containerPosition.withStart(children.last.tree.pos.end)
            Iterable(s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(containerPosition)}",
              s"  headerSpan : ${yamlForCharacterSpan(headerSpan)}",
              s"  footerSpan : ${yamlForCharacterSpan(footerSpan)}") ++ (
              if (children.nonEmpty)
                Iterable("  children :") ++ yamlForSubpieces(yamlForSection, children)
              else
                Iterable.empty[String])
          }

          def yamlForTerminal(terminal: presentationCompiler.Tree): Iterable[String] = {
            val typeName = "Terminal - TODO"
            val name = "TODO"
            val terminalPosition = terminal.pos
            Iterable(s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(terminalPosition)}",
              s"  span : ${yamlForCharacterSpan(terminalPosition)}")
          }

          section match {
            case SpanTree(tree, Seq()) => yamlForTerminal(tree)
            case SpanTree(tree, children) => yamlForContainer(tree, children)
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
          s"locationSpan : ${yamlForLineSpan(tree.pos)}",
          s"footerSpan : $yamlForEmptyCharacterSpan",
          s"parsingErrorsDetected : $parsingErrorsDetected") ++
          (if (children.nonEmpty)
            Iterable("children :") ++ yamlForSubpieces(yamlForSection, children)
          else
            Iterable.empty[String]) ++
          (if (parsingErrorsDetected)
            Iterable("parsingErrors :") ++ (reporter.capturedMessages flatMap (yamlForError andThen indentPieces))
          else
            Iterable.empty)

        joinPiecesOnSeparateLines(pieces)
      }
    }

    class SpanTreeBuilder extends presentationCompiler.Traverser {
      var spanTreeStack = scala.collection.immutable.Queue[SpanTree]()

      override def traverse(tree: presentationCompiler.Tree) = {
        if (tree.pos.isRange) {
          val preservedSpanTreeStack = spanTreeStack
          try {
            spanTreeStack = scala.collection.immutable.Queue.empty
            super.traverse(tree)
          } finally {
            spanTreeStack = preservedSpanTreeStack.enqueue(SpanTree(tree, spanTreeStack))
          }
        }
      }
    }

    val spanTreeBuilder = new SpanTreeBuilder()

    spanTreeBuilder.traverse(tree)

    val spanTree = spanTreeBuilder.spanTreeStack.head

    def adjustSpansToCoverTheSourceContiguously(siblingSpanTrees: Iterable[SpanTree]) = {
      val pairsOfSpanTreeAndOnePastItsEndAfterAdjustment = siblingSpanTrees.sliding(2).filter(2 == _.size).map { case Seq(predecessor, successor) => predecessor -> successor.tree.pos.start }
      val adjustedSpanTrees = pairsOfSpanTreeAndOnePastItsEndAfterAdjustment.map { case (spanTree, onePastTheEndAfterAdjustment) => spanTree.copy(tree = {
        val adjustedTree = spanTree.tree.shallowDuplicate
        adjustedTree.pos = adjustedTree.pos.withEnd(onePastTheEndAfterAdjustment)
        adjustedTree
      })
      }
      adjustedSpanTrees.toList :+ siblingSpanTrees.last
    }

    val spanTreeWithInternalAdjustments = spanTree.transform {case SpanTree(tree, children) => SpanTree(tree, adjustSpansToCoverTheSourceContiguously(children))}

    val startOfFileCharacterIndex = 0

    val onePastEndOfFileCharacterIndex = tree.pos.source.length

    val yaml = spanTreeWithInternalAdjustments.yaml

    scala.reflect.io.File(pathOfOutputFileForYamlResult).writeAll(yaml)
  }
}
