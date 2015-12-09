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

    val sourceText = String.copyValueOf(overallTree.pos.source.content)

    case class PositionTree(position: Position, children: Seq[PositionTree], typeName: String, name: String) {
      require(position.isOpaqueRange)
      require(children.sliding(2).filter(2 == _.size).forall { case Seq(predecessor, successor) => predecessor.position.precedes(successor.position) })

      def transform(transformer: PositionTree => PositionTree): PositionTree = {
        val transformedChildren = children.map(_.transform(transformer))
        transformer(this.copy(children = transformedChildren))
      }
    }

    class PositionTreeBuilder extends presentationCompiler.Traverser {
      val emptyPositionTreeQueue = scala.collection.immutable.Queue.empty[PositionTree]
      var positionTreeQueue = emptyPositionTreeQueue

      override def traverse(tree: presentationCompiler.Tree) = {
        val informationFromInterestingTree = if (tree.pos.isOpaqueRange) {
          PartialFunction.condOpt(tree) {
/*            case presentationCompiler.ValDef(mods, name, tpt, rhs) =>
              "Val" -> name.toString*/
            case presentationCompiler.DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
              "Def" -> name.toString
/*            case presentationCompiler.Block(stats, expr) =>
              "Block" -> ""
            case presentationCompiler.If(cond, thenp, elsep) =>
              "If" -> ""
            case presentationCompiler.CaseDef(pat, guard, body) =>
              "Case" -> ""
            case presentationCompiler.Function(vparams, body) =>
              "Function" -> ""
            case presentationCompiler.Match(selector, cases) =>
              "Match" -> ""*/
            case presentationCompiler.ClassDef(mods, name, tparams, impl) =>
              "Class" -> name.toString
            case presentationCompiler.ModuleDef(mods, name, impl) =>
              "Module" -> name.toString
            case presentationCompiler.TypeDef(mods, name, tparams, rhs) =>
              "Type" -> name.toString
            case presentationCompiler.PackageDef(pid, stats) =>
              "Package" -> pid.toString
          }
        } else None
        informationFromInterestingTree match {
          case Some((typeName, name)) =>
            val stackedPositionTreeQueue = positionTreeQueue

            positionTreeQueue = emptyPositionTreeQueue
            super.traverse(tree)
            positionTreeQueue = stackedPositionTreeQueue.enqueue(PositionTree(tree.pos, positionTreeQueue.sortWith(((lhs, rhs) => lhs.position.precedes(rhs.position))), typeName, name))

          case None => super.traverse(tree)
        }
      }
    }

    val positionTreeBuilder = new PositionTreeBuilder()

    positionTreeBuilder.traverse(overallTree)

    val positionTree = positionTreeBuilder.positionTreeQueue.head

    val adjustChildPositionsToCoverTheSourceContiguously: PositionTree => PositionTree = {
      case positionTree@PositionTree(position, Seq(), _, _) => positionTree
      case positionTree@PositionTree(position, children, _, _) =>
        val pairsOfPositionTreeAndOnePastItsEndAfterAdjustment = children.sliding(2).filter(2 == _.size).map { case Seq(predecessor, successor) => predecessor -> successor.position.pos.start }
        val adjustedPositionTrees = pairsOfPositionTreeAndOnePastItsEndAfterAdjustment.map { case (positionTree, onePastTheEndAfterAdjustment) => positionTree.copy(position = positionTree.position.withEnd(onePastTheEndAfterAdjustment)) }
        val adjustedChildren = adjustedPositionTrees.toList :+ children.last
        positionTree.copy(children = adjustedChildren)
    }

    val positionTreeWithInternalAdjustments = positionTree.transform(adjustChildPositionsToCoverTheSourceContiguously)


    val source = overallTree.pos.source

    val startOfSource = 0
    val onePastEndOfSource = source.length

    val fragmentToPadOutFromStartOfSource = positionTreeWithInternalAdjustments match {
      case PositionTree(position, children, _, _) =>
        val startOfPositionTree = children.head.position.start
        if (startOfPositionTree > startOfSource)
          Some(PositionTree(Position.range(source, startOfSource, startOfSource, startOfPositionTree), Seq.empty, "", ""))
        else
          None
    }

    val fragmentToPadOutToEndOfSource = positionTreeWithInternalAdjustments match {
      case PositionTree(position, children, _, _) =>
        val onePastEndOfPositionTree = children.last.position.end
        if (onePastEndOfPositionTree < onePastEndOfSource)
          Some(PositionTree(Position.range(source, onePastEndOfPositionTree, onePastEndOfPositionTree, onePastEndOfSource), Seq.empty, "", ""))
        else
          None
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
      case PositionTree(rootPosition, childrenOfRoot, _, _) =>
        def lineAndColumnFor(position: Position, offsetFrom: Position => Int) = {
          val offset = offsetFrom(position)
          if (offset < position.source.length) {
            val zeroRelativeLine = position.source.offsetToLine(offset)
            val line = 1 + zeroRelativeLine // Semantic Merge uses one-relative line numbers...
            val offsetOfStartOfLine = position.source.lineToOffset(zeroRelativeLine)
            val column = offset - offsetOfStartOfLine // ... but zero-relative column numbers.
            line -> column
          } else {
            val zeroRelativeLine = 1 + position.source.offsetToLine(position.source.length - 1)
            val line = 1 + zeroRelativeLine // Semantic Merge uses one-relative line numbers...
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

        def yamlForSection(section: PositionTree): Iterable[String] = {
          def yamlForContainer(position: Position, children: Iterable[PositionTree], typeName: String, name: String): Iterable[String] = {
            require(children.nonEmpty)
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

          def yamlForTerminal(terminalPosition: Position, typeName: String, name: String): Iterable[String] = {
            Iterable(s"- type : $typeName",
              s"  name : $name",
              s"  locationSpan : ${yamlForLineSpan(terminalPosition)}",
              s"  span : ${yamlForCharacterSpan(terminalPosition)}")
          }

          section match {
            case PositionTree(position, Seq(), typeName, name) => yamlForTerminal(position, typeName, name)
            case PositionTree(position, children, typeName, name) => yamlForContainer(position, children, typeName, name)
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

    scala.reflect.io.File(pathOfOutputFileForYamlResult).writeAll(yaml)
  }
}
