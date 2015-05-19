/**
 * Created by Gerard on 17/05/2015.
 */

package com.sageSerpent.neptunium

import java.nio.file.Paths

import scala.reflect.internal.util.Position
import scala.reflect.io.PlainFile
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters._
import scala.tools.nsc.util.BatchSourceFile


object Main extends App {

  System.out.println("Starting....")

  val settings = new Settings()

  val classPath = System.getProperty("java.class.path");

  settings.bootclasspath.append(classPath)

  val currentWorkingDirectory = Paths.get(".").toAbsolutePath()

  val sourceFile = new PlainFile( """C:\Users\Gerard\OneDrive\Documents\Neptunium\src\main\scala-2.11\SomeThingToParse.scala""")
  val presentationCompiler: Global = new Global(settings, new ConsoleReporter(settings))
  val tree: presentationCompiler.Tree = presentationCompiler.parseTree(new BatchSourceFile(sourceFile))

  val positions: List[presentationCompiler.Position] = tree.collect(PartialFunction(_.pos))

  val snippets = tree.collect(PartialFunction(_.toString()))


  for ((position, snippet) <- positions zip snippets) {
    System.out.println(position)
    System.out.println(snippet)
    System.out.println("--------------------")
  }
  val spanTreeBuilder = new SpanTreeBuilder()

  case class SpanTree(position: Position, children: Seq[SpanTree]) {
  }

  class SpanTreeBuilder extends presentationCompiler.Traverser {
    var spanTreeStack = scala.collection.immutable.Stack[SpanTree]()

    override def traverse(tree: presentationCompiler.Tree) = {
      val preservedSpanTreeStack = spanTreeStack
      try {
        spanTreeStack = scala.collection.immutable.Stack.empty
        super.traverse(tree)
      } finally {
        spanTreeStack = preservedSpanTreeStack.push(SpanTree(tree.pos, spanTreeStack))
      }
    }
  }

  spanTreeBuilder.traverse(tree)

  System.out.println(spanTreeBuilder.spanTreeStack.head)

  System.out.println("Made it here.")
}
