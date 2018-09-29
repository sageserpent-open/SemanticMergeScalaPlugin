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

  trait LineMapping {
    def offsetFrom(lineAndOffSet: LineAndOffSet): ZeroRelativeCharacterIndex
    def spanOf(locationSpan: LocationSpan): Span = Span(offsetFrom(locationSpan.start), offsetFrom(locationSpan.end))

    trait Compound {
      def locationSpan: LocationSpan
      def childSpans: Seq[Span]

      require(childSpans.isEmpty || Span(childSpans.head.start, childSpans.last.end) == spanOf(locationSpan))
      require(childSpans.isEmpty || (childSpans zip childSpans.tail forall {
        case (predecessor, successor) =>
          predecessor abuts successor
      }))
    }

    case class File(
        `type`: String,
        name: String,
        locationSpan: LocationSpan,
        footerSpan: Span,
        children: Seq[Container],
        parsingErrorsDetected: Boolean,
        parsingError: Seq[ParsingError]
    ) extends Compound {
      require(Span.floatingEmptySpan == footerSpan || (spanOf(locationSpan) abuts footerSpan))

      def childSpans: Seq[Span] = children.map(child => spanOf(child.locationSpan))
    }

    case class ParsingError(location: LineAndOffSet, message: String)

    sealed trait Declaration {
      def locationSpan: LocationSpan
      require(!spanOf(locationSpan).isEmpty)
    }

    case class Container(
        `type`: String,
        name: String,
        locationSpan: LocationSpan,
        headerSpan: Span,
        footerSpan: Span,
        children: Seq[Declaration]
    ) extends Declaration
        with Compound {
      def childSpans: Seq[Span] = headerSpan +: children.map(child => spanOf(child.locationSpan)) :+ footerSpan
    }

    case class Terminal(`type`: String, name: String, locationSpan: LocationSpan, span: Span) extends Declaration {
      require(spanOf(locationSpan) == span)
    }
  }

  def structureOf(parsedSource: Parsed[Source]): LineMapping#File = ???

  {
    val lineMapping: LineMapping = ???
    import io.circe.generic.auto._
    implicit val parsingErrorEncoder = implicitly[Encoder[lineMapping.ParsingError]]
    implicit val terminalEncoder     = implicitly[Encoder[lineMapping.Terminal]]
    implicit val containerEncoder    = implicitly[Encoder[lineMapping.Container]]
  }
}
