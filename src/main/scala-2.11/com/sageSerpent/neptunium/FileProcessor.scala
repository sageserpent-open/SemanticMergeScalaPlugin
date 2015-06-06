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

    case class SpanTree(tree: presentationCompiler.Tree, children: Seq[SpanTree]) {
      def content = tree.pos.source.content.slice(tree.pos.start, tree.pos.end)

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
          val (startLine, startColumn) = lineAndColumnFor(position, _.start)
          val (endLine, endColumn) = lineAndColumnFor(position, _.end)
          s"{start: [$startLine,$startColumn], end: [$endLine,$endColumn]}"
        }
        def yamlForCharacterSpan(position: Position) =
          s"[${position.start}, ${position.end - 1}]" // Semantic Merge uses []-intervals (closed - closed) for character
        // spans, so we have to decrement the end position which is really
        // one past the end; 'Position' models a [)-interval (closed, open).
        val yamlForEmptyCharacterSpan = "[0, -1]"
        def indent(indentationLevel: Int)(line: String) =
          " " * indentationLevel + line
        val indentPieces = (_: Iterable[String]).map(indent(2))
        def joinPiecesOnSeparateLines(pieces: Iterable[String]) =
          String.join("\n", pieces.asJava)
        def yamlForSubpieces(yamlForSubpiece: SpanTree => Iterable[String], pieces: Iterable[SpanTree]): Iterable[String] =
          pieces.flatMap(yamlForSubpiece andThen indentPieces)

        def yamlForSection(section: SpanTree): Iterable[String] = {
          def yamlForContainer(containerPosition: Position, children: Iterable[SpanTree]): Iterable[String] = {
            require(children.nonEmpty)
            val typeName = "Section - TODO"
            val name = "Don't know"
            val headerSpan = containerPosition.withEnd(children.head.tree.pos.start)
            val footerSpan = containerPosition.withStart(children.last.tree.pos.start)
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

          def yamlForTerminal(terminalPosition: Position): Iterable[String] = {
            val typeName = "Terminal - TODO"
            val name = "Don't know"
            Iterable(s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(terminalPosition)}",
              s"  span : ${yamlForCharacterSpan(terminalPosition)}")
          }

          section match {
            case SpanTree(tree, Seq()) => yamlForTerminal(tree.pos)
            case SpanTree(tree, children) => yamlForContainer(tree.pos, children)
          }
        }

        val yamlForError: ((Position, String)) => Iterable[String] = {case ((position: Position), (message: String)) =>
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
      var spanTreeStack = scala.collection.immutable.Stack[SpanTree]()

      override def traverse(tree: presentationCompiler.Tree) = {
        if (tree.pos.isRange) {
          val preservedSpanTreeStack = spanTreeStack
          try {
            spanTreeStack = scala.collection.immutable.Stack.empty
            super.traverse(tree)
          } finally {
            spanTreeStack = preservedSpanTreeStack.push(SpanTree(tree, spanTreeStack))
          }
        }
      }
    }

    val spanTreeBuilder = new SpanTreeBuilder()

    spanTreeBuilder.traverse(tree)

    val spanTree = spanTreeBuilder.spanTreeStack.head

    val yaml = spanTree.yaml

    scala.reflect.io.File(pathOfOutputFileForYamlResult).writeAll(yaml)
  }
}
