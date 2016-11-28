/**
  * Created by Gerard on 17/05/2015.
  */

package com.sageSerpent.neptunium

import java.io.File

import org.log4s._

import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.{-\/, \/-}

object Subprocess extends App {
  private[this] val logger = getLogger

  {
    val numberOfArguments = args.length

    val numberOfArgumentsExpected = 3
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

    val loggingSink = Process.constant((line: String) => Task {
      logger.info(line)
    }).toSource

    val pathsOfFiles = io.linesR(System.in) observe loggingSink takeWhile (!endOfInputSentinelFromSemanticMerge.equalsIgnoreCase(_))

    val pairsOfPathOfFileToBeProcessedAndItsResultFile = pathsOfFiles.chunk(2).takeWhile(2 == _.length)

    val linkAlias = new File(args(2)).toPath

    val statuses = pairsOfPathOfFileToBeProcessedAndItsResultFile.flatMap { case Vector(pathOfFileToBeProcessed, pathOfResultFile) => Process eval Task {
      FileProcessor.discoverStructure(linkAlias)(pathOfFileToBeProcessed, pathOfResultFile)
    }.attempt
    } |> process1.lift { case \/-(()) => "OK"
    case -\/(error) =>
      logger.error(error)("Caught exception thrown by FileProcessor.")
      "KO"
    }
    val endToEndProcessing = statuses to io.stdOutLines

    endToEndProcessing.run.run

    logger.info("Subprocess exiting.")
  }
}
