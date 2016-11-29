/**
  * Created by Gerard on 17/05/2015.
  */

package com.sageserpent.neptunium

import java.io.File
import java.nio.file.{Files, Path}

import resource._

import scala.util.{Success, Failure, Try}
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.{-\/, \/-}
import org.log4s._

import scala.collection.JavaConversions._

object Main extends App {
  private[this] val logger = getLogger

  val jarWithNonPossiblyNonStandardExtensionProvidingThisCode = new File(Main.getClass.getProtectionDomain.getCodeSource.getLocation.getPath).toPath

  def removeJunk(path: Path): Unit = {
    logger.info(s"Removing: $path.")
    Try(path.toFile.delete()) match {
      case Success(true) => logger.info(s"Removed: '$path' successfully.")
      case Success(false) => logger.error(s"Failed to remove '$path' - no reason given.")
      case Failure(error) => logger.error(error)(s"Failed to remove '$path'.")
    }
  }

  {
    val numberOfArguments = args.length

    val numberOfArgumentsExpected = 2

    numberOfArguments match {
      case _@count if numberOfArgumentsExpected == count =>
  for {
    locationOfLibraryJar <- makeManagedResource(temporaryDirectory)(removeJunk)(List.empty)
    libraryJar <- makeManagedResource(temporaryLibraryJar(locationOfLibraryJar))(removeJunk)(List.empty)
  } {
          new ProcessBuilder(List("java", "-jar", jarWithNonPossiblyNonStandardExtensionProvidingThisCode.toString) ++ args :+ libraryJar.toString).inheritIO.start().waitFor()
        }
      case _@count if 1 + numberOfArgumentsExpected == count =>
        val theOnlyModeHandled = "shell"

        val mode = args(0)

        if (!theOnlyModeHandled.equalsIgnoreCase(mode)) {
          throw new Error(
            s"ERROR, expect mode: $theOnlyModeHandled, requested mode was: $mode.")
        }

        val acknowledgementFilePath = args(1)

        val acknowledgementOfBeingInitialisedBackToSemanticMerge = "READY"

        scala.reflect.io.File(acknowledgementFilePath).writeAll(acknowledgementOfBeingInitialisedBackToSemanticMerge)

        val endOfInputSentinelFromSemanticMerge = "end"

        val loggingSink = Process.constant((line: String) => Task {
          logger.info(line)
        }).toSource

        val pathsOfFiles = io.linesR(System.in) observe loggingSink takeWhile (!endOfInputSentinelFromSemanticMerge.equalsIgnoreCase(_))

        val pairsOfPathOfFileToBeProcessedAndItsResultFile = pathsOfFiles.chunk(2).takeWhile(2 == _.length)

        val libraryPath = new File(args(2)).toPath

        val statuses = pairsOfPathOfFileToBeProcessedAndItsResultFile.flatMap { case Vector(pathOfFileToBeProcessed, pathOfResultFile) => Process eval Task {
          FileProcessor.discoverStructure(libraryPath)(pathOfFileToBeProcessed, pathOfResultFile)
        }.attempt
        } |> process1.lift { case \/-(()) => "OK"
        case -\/(error) =>
          logger.error(error)("Caught exception thrown by FileProcessor.")
          "KO"
        }
        val endToEndProcessing = statuses to io.stdOutLines

        endToEndProcessing.run.run

    logger.info("Plugin exiting.")
      case _ =>
        throw new Error(
          s"ERROR: expected $numberOfArgumentsExpected arguments, got $numberOfArguments.")
  }
  }

  private def temporaryDirectory = {
    val tempDirectory = Files.createTempDirectory("SemanticMergeScalaPlugin")
    tempDirectory.toFile.setWritable(true)
    tempDirectory
  }

  private def temporaryLibraryJar(locationOfLibraryJar: Path) = {
    val libraryJar = Files.copy(jarWithNonPossiblyNonStandardExtensionProvidingThisCode, locationOfLibraryJar.resolve("SemanticMergeScalaPlugin.jar"))
    libraryJar.toFile.setReadable(true)
    libraryJar.toFile.setWritable(true)
    libraryJar.toFile.setExecutable(true)
    libraryJar
  }
}
