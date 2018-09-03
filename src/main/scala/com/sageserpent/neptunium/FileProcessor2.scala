package com.sageserpent.neptunium
import scala.meta.parsers.Parsed
import scala.meta.Source

object FileProcessor2 {
  def discoverStructure(pathOfInputFile: String,
                        charsetOfInputFile: String,
                        pathOfOutputFileForYamlResult: String): Unit = ???

  case class File()

  case class ParsingErrors()

  sealed trait Declaration

  case class Container() extends Declaration

  case class Node() extends Declaration

  def structureOf(parsedSource: Parsed[Source]): File = ???
}
