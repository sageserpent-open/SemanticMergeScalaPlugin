/**
  * Created by Gerard on 17/05/2015.
  */

package com.sageSerpent.neptunium

import java.io.File
import java.nio.file.{Files, Path}

import resource._

import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.{-\/, \/-}
import org.log4s._

object Main extends App {
  private [this] val logger = getLogger

  val jarWithNonPossiblyNonStandardExtensionProvidingThisCode = new File(Main.getClass.getProtectionDomain.getCodeSource.getLocation.getPath).toPath

  def removeJunk(path: Path): Unit = path.toFile.deleteOnExit()

  for {
    locationOfLinkAlias <- makeManagedResource(Files.createTempDirectory("SemanticMergeScalaPlugin"))(removeJunk _)(List.empty)
    linkAlias = linkAliasIn(locationOfLinkAlias)
    aliasedJarLocation <- makeManagedResource(Files.createLink(linkAlias, jarWithNonPossiblyNonStandardExtensionProvidingThisCode))((removeJunk _))(List.empty)
  } {
    val numberOfArguments = args.length

    val numberOfArgumentsExpected = 2
    if (numberOfArgumentsExpected != numberOfArguments) {
      throw new Error(
        s"ERROR: expected $numberOfArgumentsExpected arguments, got $numberOfArguments.")
    }
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

    val loggingSink = Process.constant((line: String) => Task{logger.info(line)}).toSource

    val pathsOfFiles = io.stdInLines observe loggingSink takeWhile (!endOfInputSentinelFromSemanticMerge.equalsIgnoreCase(_))

    val pairsOfPathOfFileToBeProcessedAndItsResultFile = pathsOfFiles.chunk(2).takeWhile(2 == _.length)
    val statuses = pairsOfPathOfFileToBeProcessedAndItsResultFile.flatMap { case Vector(pathOfFileToBeProcessed, pathOfResultFile) => Process eval Task {
      FileProcessor.discoverStructure(linkAlias)(pathOfFileToBeProcessed, pathOfResultFile)
    }.attempt
    } |> process1.lift { case \/-(()) => "OK"
    case -\/(error) => {
      logger.error(error)("Caught exception.")
      "KO"
    }
    }
    val endToEndProcessing = statuses to io.stdOutLines

    endToEndProcessing.run.run

    println("Done.")
  }

  def linkAliasIn(locationOfLinkAlias: Path): Path = {
    val linkAlias = locationOfLinkAlias.resolve("SemanticMergeScalaPlugin.jar")
    linkAlias.toFile.setReadable(true)
    linkAlias.toFile.setWritable(false)
    linkAlias.toFile.setExecutable(true)
    linkAlias
  }
}
