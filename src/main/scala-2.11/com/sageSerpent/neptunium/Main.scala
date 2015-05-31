/**
 * Created by Gerard on 17/05/2015.
 */

package com.sageSerpent.neptunium

import java.io.FileWriter

import resource.managed

import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.{-\/, \/-}

object Main extends App {

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

  val endOfInputSentinelFromSemanticMerge = "end"

  val pathsOfFiles = io.stdInLines takeWhile (!endOfInputSentinelFromSemanticMerge.equalsIgnoreCase(_))

  for (acknowledgementFileWriter <- managed(new FileWriter(acknowledgementFilePath))) {
    var readyAcknowledgementToSemanticMerge = "READY"
    acknowledgementFileWriter.write(readyAcknowledgementToSemanticMerge)
  }
  val pairsOfPathOfFileToBeProcessedAndItsResultFile = pathsOfFiles.chunk(2).takeWhile(2 == _.length)
  val statuses = pairsOfPathOfFileToBeProcessedAndItsResultFile.flatMap { case Vector(pathOfFileToBeProcessed, pathOfResultFile) => Process eval Task {
    FileProcessor.discoverStructure(pathOfFileToBeProcessed, pathOfResultFile)
  }.attempt
  } |> process1.lift { case \/-(_) => "OK"
  case -\/(_) => "KO"
  }
  val endToEndProcessing = statuses to io.stdOutLines

  endToEndProcessing.run.run
}
