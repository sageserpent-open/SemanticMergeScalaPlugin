package com.sageserpent.neptunium
import scala.meta.parsers.Parsed
import scala.meta.Source

object FileProcessor2 {
  def discoverStructure(
      pathOfInputFile: String,
      charsetOfInputFile: String,
      pathOfOutputFileForYamlResult: String
  ): Unit = ???

  case class File(
      `type`: String,
      name: String,
      locationSpan: LocationSpan,
      footerSpan: Span,
      children: Seq[Container],
      parsingErrorsDetected: Boolean,
      parsingError: Seq[ParsingError]
  )

  case class ParsingError(location: LineAndOffSet, message: String)

  sealed trait Declaration

  case class Container(
      `type`: String,
      name: String,
      locationSpan: LocationSpan,
      headerSpan: Span,
      footerSpan: Span,
      children: Seq[Container]
  ) extends Declaration

  case class Node(`type`: String, name: String, locationSpan: LocationSpan, span: Span) extends Declaration

  type OneRelativeLineNumber = Int

  type ZeroRelativeOffset = Int

  type LineAndOffSet = (OneRelativeLineNumber, ZeroRelativeOffset)

  type ZeroRelativeCharacterIndex = Int

  case class LocationSpan(start: LineAndOffSet, end: LineAndOffSet)

  type Span = (ZeroRelativeCharacterIndex, ZeroRelativeCharacterIndex)

  def structureOf(parsedSource: Parsed[Source]): File = ???
}
