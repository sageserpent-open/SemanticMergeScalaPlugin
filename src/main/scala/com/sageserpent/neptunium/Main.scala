/**
  * Created by Gerard on 17/05/2015.
  */
package com.sageserpent.neptunium

import java.io.FileWriter

import org.log4s._
import resource._
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.{-\/, \/-}

object Main extends App {
  private[this] val logger = getLogger

  private val numberOfLinesPerParsingRequest = 3

  {
    val numberOfArguments = args.length

    val numberOfArgumentsExpected = 2

    numberOfArguments match {
      case _ @count if numberOfArgumentsExpected == count =>
        val theOnlyModeHandled = "shell"

        val mode = args(0)

        if (!theOnlyModeHandled.equalsIgnoreCase(mode)) {
          throw new Error(
            s"ERROR, expect mode: $theOnlyModeHandled, requested mode was: $mode.")
        }

        val acknowledgementFilePath = args(1)

        val acknowledgementOfBeingInitialisedBackToSemanticMerge = "READY"

        for (writer <- managed(new FileWriter(acknowledgementFilePath))) {
          writer.write(
            acknowledgementOfBeingInitialisedBackToSemanticMerge,
            0,
            acknowledgementOfBeingInitialisedBackToSemanticMerge.length)
        }

        val endOfInputSentinelFromSemanticMerge = "end"

        val loggingSink = Process
          .constant((line: String) =>
            Task {
              logger.info(line)
          })
          .toSource

        val pathsOfFiles = io.linesR(System.in) observe loggingSink takeWhile (!endOfInputSentinelFromSemanticMerge
          .equalsIgnoreCase(_))

        val pairsOfPathOfFileToBeProcessedAndItsResultFile = pathsOfFiles
          .chunk(numberOfLinesPerParsingRequest)
          .takeWhile(numberOfLinesPerParsingRequest == _.length)

        val statuses = pairsOfPathOfFileToBeProcessedAndItsResultFile.flatMap {
          case Vector(pathOfFileToBeProcessed,
                      charsetOfFileToBeProcessed,
                      pathOfResultFile) =>
            Process eval Task {
              FileProcessor.discoverStructure(
                pathOfFileToBeProcessed,
                charsetOfFileToBeProcessed,
                pathOfResultFile)
            }.attempt
        } |> process1.lift {
          case \/-(()) => "OK"
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
}
