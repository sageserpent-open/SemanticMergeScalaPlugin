package com.sageserpent.neptunium
import io.circe.{Decoder, Encoder}

import scala.meta.parsers.Parsed
import scala.meta.Source

object FileProcessor2 {
  def discoverStructure(
      pathOfInputFile: String,
      charsetOfInputFile: String,
      pathOfOutputFileForYamlResult: String
  ): Unit = ???

  trait LineMapping {
    def offsetFrom(lineAndOffSet: LineAndOffSet): ZeroRelativeCharacterIndex
    def spanOf(locationSpan: LocationSpan): Span = Span(offsetFrom(locationSpan.start), offsetFrom(locationSpan.end))
  }

  trait Parent {
    def locationSpan: LocationSpan
    def childSpans: Seq[Span]
  }

  trait ParentContracts extends Parent {
    this: LineMapping =>
    require(!childSpans.isEmpty)
    require(Span(childSpans.head.start, childSpans.last.end) == spanOf(locationSpan))
    require(childSpans zip childSpans.tail forall {
      case (predecessor, successor) =>
        predecessor abuts successor
    })
  }

  abstract case class File(
      `type`: String,
      name: String,
      locationSpan: LocationSpan,
      footerSpan: Span,
      children: Seq[Container],
      parsingErrorsDetected: Boolean,
      parsingError: Seq[ParsingError]
  ) extends Parent {
    this: LineMapping with ParentContracts =>
    require(Span.floatingEmptySpan == footerSpan || (spanOf(locationSpan) abuts footerSpan))

    def childSpans: Seq[Span] = children.map(child => spanOf(child.locationSpan))
  }

  case class ParsingError(location: LineAndOffSet, message: String)

  trait Declaration {
    def locationSpan: LocationSpan
  }

  trait DeclarationContracts extends Declaration {
    this: LineMapping =>
    require(!spanOf(locationSpan).isEmpty)
  }

  abstract case class Container(
      `type`: String,
      name: String,
      locationSpan: LocationSpan,
      headerSpan: Span,
      footerSpan: Span,
      children: Seq[Container]
  ) extends Declaration
      with Parent {
    this: LineMapping with ParentContracts with DeclarationContracts =>
    def childSpans: Seq[Span] = headerSpan +: children.map(child => spanOf(child.locationSpan)) :+ footerSpan
  }

  abstract case class Terminal(`type`: String, name: String, locationSpan: LocationSpan, span: Span)
      extends Declaration {
    this: LineMapping with DeclarationContracts =>
    require(spanOf(locationSpan) == span)
  }

  type OneRelativeLineNumber = Int

  type ZeroRelativeOffset = Int

  type LineAndOffSet = (OneRelativeLineNumber, ZeroRelativeOffset)

  type ZeroRelativeCharacterIndex = Int

  case class LocationSpan(start: LineAndOffSet, end: LineAndOffSet)

  object Span {
    val floatingEmptySpan = Span(0, -1)

    implicit val encoder: Encoder[Span] =
      implicitly[Encoder[(ZeroRelativeCharacterIndex, ZeroRelativeCharacterIndex)]].contramap(span =>
        span.start -> span.end)

    implicit val decoder: Decoder[Span] =
      implicitly[Decoder[(ZeroRelativeCharacterIndex, ZeroRelativeCharacterIndex)]].emap {
        case (start, end) => Right(Span(start, end))
      }
  }

  case class Span(start: ZeroRelativeCharacterIndex, end: ZeroRelativeCharacterIndex) {
    // TODO - will need to do custom serialization into YAML by converting to a pair.
    require(start <= 1 + end) // A span denotes a closed-closed interval, so an empty interval has the end one before the start.

    def isEmpty: Boolean = this.start == 1 + this.end

    def abuts(another: Span): Boolean = 1 + this.end == another.start
  }

  def structureOf(parsedSource: Parsed[Source]): File = ???
}
