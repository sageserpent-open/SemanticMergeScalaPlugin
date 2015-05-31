package com.sageSerpent.neptunium

import scala.collection.JavaConverters._
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.reflect.io.PlainFile
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.ConsoleReporter

object FileProcessor {
  def discoverStructure(pathOfInputFile: String, pathOfOutputFileForYamlResult: String) = {
    val settings = new Settings()

    val classPath = System.getProperty("java.class.path");

    settings.bootclasspath.append(classPath) // Voodoo required by the Scala presentation compiler.

    val sourceFile = new PlainFile(pathOfInputFile)
    val presentationCompiler: Global = new Global(settings, new ConsoleReporter(settings))
    val tree: presentationCompiler.Tree = presentationCompiler.parseTree(new BatchSourceFile(sourceFile))

    case class SpanTree(tree: presentationCompiler.Tree, children: Seq[SpanTree]) {
      def content = tree.pos.source.content.slice(tree.pos.start, tree.pos.end)

      def yaml = {
        def yamlForLineSpan(position: Position) = {
          def lineAndColumnFor(offset: Int) = {
            val line = 1 + position.source.offsetToLine(offset) // Semantic Merge uses one-relative line numbers...
            val column = position.source.lineToOffset(line)     // ... but zero-relative column numbers.
            line -> column
          }
          val (startLine, startColumn) = lineAndColumnFor(position.start)
          val (endLine, endColumn) = lineAndColumnFor(position.end)
          s"{{start: [$startLine,$startColumn], end: [$endLine,$endColumn]}}"
        }
        def yamlForCharacterSpan(position: Position) =
          s"[${position.start}, ${position.end}]"
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
            val typeName = "Section - TODO"
            val name = "Don't know"
            val headerSpan = containerPosition
            val footerSpan = containerPosition
            Iterable(s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(containerPosition)}",
              s"  headerSpan : ${yamlForCharacterSpan(headerSpan)}",
              s"  footerSpan : ${yamlForCharacterSpan(footerSpan)}") ++ (
              if (!children.isEmpty)
                Iterable("  children :") ++ yamlForSubpieces(yamlForSection _, children)
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

        val pieces: Iterable[String] = {
          val parsingErrorsDetected = false
          Iterable("---",
            "type : file",
            s"name : ${pathOfInputFile}",
            s"locationSpan : ${yamlForLineSpan(tree.pos)}",
            s"footerSpan : ${yamlForEmptyCharacterSpan}",
            s"parsingErrorsDetected : ${parsingErrorsDetected}") ++
            (if (!children.isEmpty)
              Iterable("  children :") ++ yamlForSubpieces(yamlForSection _, children)
            else
              Iterable.empty[String]) ++
            (if (parsingErrorsDetected)
              Seq("parsingErrors :")
            else
              Seq.empty)
        }

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
