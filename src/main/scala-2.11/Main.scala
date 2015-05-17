/**
 * Created by Gerard on 17/05/2015.
 */

package com.sageSerpent.neptunium

import java.nio.file.Paths

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
  val tree = presentationCompiler.parseTree(new BatchSourceFile(sourceFile))
  for (thing <- tree.children map (_.toString())) {
    System.out.println(thing)
  }

  System.out.println("Made it here.")
}
