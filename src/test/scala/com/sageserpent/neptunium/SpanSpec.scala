package com.sageserpent.neptunium

import com.sageserpent.neptunium.FileProcessor2.Span
import io.circe.generic.auto._
import io.circe.syntax._
import org.scalatest.{FlatSpec, Matchers}

class SpanSpec extends FlatSpec with Matchers {
  "A span" should "be rendered in YAML as if it were a plain tuple." in {
    val exampleSpan      = Span(0, 1)
    val Span(start, end) = exampleSpan
    val equivalentTuple  = start -> end

    val jsonFromSpan = exampleSpan.asJson

    val reconstitutedTuple = jsonFromSpan.as[(Int, Int)]

    reconstitutedTuple should be(Right(equivalentTuple))
  }

  it should "be reconstituted from YAML as if it were a plain tuple" in {
    val exampleSpan      = Span(0, 1)
    val Span(start, end) = exampleSpan
    val equivalentTuple  = start -> end

    val jsonFromTuple = equivalentTuple.asJson

    val reconstitutedSpan = jsonFromTuple.as[Span]

    reconstitutedSpan should be(Right(exampleSpan))
  }
}
