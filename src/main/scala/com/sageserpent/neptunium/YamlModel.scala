package com.sageserpent.neptunium
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}

import scala.meta.Source
import scala.meta.parsers.Parsed

object YamlModel {
  type OneRelativeLineNumber = Int

  type ZeroRelativeOffset = Int

  type LineAndOffset = (OneRelativeLineNumber, ZeroRelativeOffset)

  type ZeroRelativeCharacterIndex = Int

  case class LocationSpan(start: LineAndOffset, end: LineAndOffset)

  object Span {
    val floatingEmptySpan = Span(0, -1)

    implicit val encoder: Encoder[Span] =
      implicitly[Encoder[(ZeroRelativeCharacterIndex, ZeroRelativeCharacterIndex)]]
        .contramap(span => span.start -> span.end)

    implicit val decoder: Decoder[Span] =
      implicitly[Decoder[(ZeroRelativeCharacterIndex, ZeroRelativeCharacterIndex)]].emap {
        case (start, end) => Right(Span(start, end))
      }
  }

  case class Span(start: ZeroRelativeCharacterIndex, end: ZeroRelativeCharacterIndex) {
    // TODO - will need to do custom serialization into YAML by converting to a pair.
    require(start <= 1 + end) // A span denotes a closed-closed interval, so an empty interval has the end one before the start.

    def isEmpty: Boolean = this.start == 1 + this.end

    def abuts(another: Span): Boolean =
      Span.floatingEmptySpan == this || Span.floatingEmptySpan == another || 1 + this.end == another.start

    def mergeWithAbutting(another: Span) = {
      require(this abuts another)

      if (Span.floatingEmptySpan == this) another
      else if (Span.floatingEmptySpan == another) this
      else this.copy(end = another.end)
    }
  }

  trait LineMapping {
    def offsetFrom(lineAndOffSet: LineAndOffset): ZeroRelativeCharacterIndex
    def spanOf(locationSpan: LocationSpan): Span = Span(offsetFrom(locationSpan.start), offsetFrom(locationSpan.end))
    val numberOfCharacters: Int

    trait Compound {
      def locationSpan: LocationSpan
      def childSpans: Seq[Span]

      require(childSpans zip childSpans.tail forall {
        case (predecessor, successor) =>
          predecessor abuts successor
      })

      require(childSpans.isEmpty || childSpans.reduce(_ mergeWithAbutting _) == spanOf(locationSpan))
    }

    case class File(
        `type`: String,
        name: String,
        locationSpan: LocationSpan,
        footerSpan: Span,
        children: Seq[Declaration],
        parsingErrorsDetected: Boolean,
        parsingErrors: Seq[ParsingError]
    ) extends Compound {
      def childSpans: Seq[Span] = children.map(child => spanOf(child.locationSpan)) :+ footerSpan
    }

    case class ParsingError(location: LineAndOffset, message: String)

    object Declaration {
      implicit val encoder: Encoder[Declaration] = {
        case container: Container => container.asJson
        case terminal: Terminal   => terminal.asJson
      }
    }

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

    object Terminal {
      def apply(`type`: String, name: String, locationSpan: LocationSpan): Terminal =
        Terminal(`type`, name, locationSpan, spanOf(locationSpan))
    }

    case class Terminal(`type`: String, name: String, locationSpan: LocationSpan, span: Span) extends Declaration {
      require(spanOf(locationSpan) == span)
    }
  }

  def structureOf(parsedSource: Parsed[Source]): LineMapping#File = ???
}
