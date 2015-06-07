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

    case class SpanTree(treePosition: Position, children: Iterable[SpanTree]) {
      def transform(transformer: SpanTree => SpanTree): SpanTree = {
        val transformedChildren = children.map(_.transform(transformer))
        transformer(this.copy(children = transformedChildren))
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
            spanTreeStack = preservedSpanTreeStack.enqueue(SpanTree(tree.pos, spanTreeStack))
          }
        }
      }
    }

    val spanTreeBuilder = new SpanTreeBuilder()

    spanTreeBuilder.traverse(overallTree)

    val spanTree = spanTreeBuilder.spanTreeStack.head

    def adjustSpansToCoverTheSourceContiguously(siblingSpanTrees: Iterable[SpanTree]) = {
      if (siblingSpanTrees.nonEmpty) {
        val pairsOfSpanTreeAndOnePastItsEndAfterAdjustment = siblingSpanTrees.sliding(2).filter(2 == _.size).map { case Seq(predecessor, successor) => predecessor -> successor.treePosition.pos.start }
        val adjustedSpanTrees = pairsOfSpanTreeAndOnePastItsEndAfterAdjustment.map { case (spanTree, onePastTheEndAfterAdjustment) => spanTree.copy(treePosition = spanTree.treePosition.withEnd(onePastTheEndAfterAdjustment))
        }
        adjustedSpanTrees.toList :+ siblingSpanTrees.last
      } else {siblingSpanTrees}
    }

    val spanTreeWithInternalAdjustments = spanTree.transform {case SpanTree(treePosition, children) => SpanTree(treePosition, adjustSpansToCoverTheSourceContiguously(children))}

    def yamlFrom: SpanTree => String = { case SpanTree(rootPosition, childrenOfRoot) =>
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
        def yamlForContainer(containerPosition: Position, children: Iterable[SpanTree]): Iterable[String] = {
          require(children.nonEmpty)
          val typeName = "Section - TODO"
          val name = "TODO"
          val startOfFirstChild = children.head.treePosition.pos.start
          val onePastEndOfLastChild = children.last.treePosition.pos.end
          val containerPositionConsistentWithChildren = containerPosition.withStart(Ordering[Int].min(containerPosition.start, startOfFirstChild)).withEnd(Ordering[Int].max(containerPosition.end, onePastEndOfLastChild))
          val headerSpan = containerPositionConsistentWithChildren.withEnd(startOfFirstChild)
          val footerSpan = containerPositionConsistentWithChildren.withStart(onePastEndOfLastChild)
          Iterable(s"- type : $typeName",
            s"  name : $name",
            s"  locationSpan : ${yamlForLineSpan(containerPositionConsistentWithChildren)}",
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
          case SpanTree(treePosition, Seq()) => yamlForTerminal(treePosition)
          case SpanTree(treePosition, children) => yamlForContainer(treePosition, children)
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
        //s"footerSpan : ${yamlForCharacterSpan(tree.pos.withStart(startOfFileCharacterIndex).withEnd(onePastEndOfFileCharacterIndex))}",
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

    val yaml = yamlFrom(spanTreeWithInternalAdjustments)

    scala.reflect.io.File(pathOfOutputFileForYamlResult).writeAll(yaml)
  }
}
