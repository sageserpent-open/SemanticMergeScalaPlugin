/**
 * Created by Gerard on 17/05/2015.
 */

package com.sageSerpent.neptunium

import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.{-\/, \/-}

object Main extends App {

  /*  val numberOfArguments = args.length

    val numberOfArgumentsExpected = 2
    if (numberOfArgumentsExpected != numberOfArguments) {
      throw new Error(
        s"ERROR: expected $numberOfArgumentsExpected arguments, got $numberOfArguments.")
    }


    var mode = args(0)

    val theOnlyModeHandled = "shell"

    if (!theOnlyModeHandled.equalsIgnoreCase(mode)) {
      throw new Error(
        s"ERROR, expect mode: $theOnlyModeHandled, requested mode was: $mode.")
    }

    val acknowledgementFilePath = args(1)

    for (acknowledgementFileWriter <- managed(new FileWriter(acknowledgementFilePath))) {
      var readyAcknowledgementToSemanticMerge = "READY"
      acknowledgementFileWriter.write(readyAcknowledgementToSemanticMerge)
    }*/

  val pathsOfFiles = io.stdInLines takeWhile (!endOfInputSentinelFromSemanticMerge.equalsIgnoreCase(_))
  val pairsOfPathOfFileToBeProcessedAndItsResultFile = pathsOfFiles.chunk(2).takeWhile(2 == _.length)
  val statuses = pairsOfPathOfFileToBeProcessedAndItsResultFile.flatMap { case Vector(pathOfFileToBeProcessed, pathOfResultFile) => Process eval Task {
    FileProcessor.discoverStructure(pathOfFileToBeProcessed, pathOfResultFile)
  }.attempt
  } |> process1.lift { case \/-(_) => "OK"
  case -\/(_) => "KO"
  }
  val endToEndProcessing = statuses to io.stdOutLines
  private val endOfInputSentinelFromSemanticMerge = "end"


  endToEndProcessing.run.run
}
