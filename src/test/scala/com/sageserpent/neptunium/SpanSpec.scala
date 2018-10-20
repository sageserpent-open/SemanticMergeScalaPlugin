package com.sageserpent.neptunium

import com.sageserpent.neptunium.YamlModel.Span
import io.circe.syntax._
import org.scalacheck.{Gen, Shrink, ShrinkLowPriority}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SpanSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with ShrinkLowPriority {
  implicit val noShrink: Shrink[Span] = shrinkAny

  val maximumIndex = 50
  val spanGenerator =
    for {
      start <- Gen.chooseNum(0, maximumIndex)
      end   <- Gen.chooseNum(start, maximumIndex)
    } yield Span(start, end)

  "A span" should "be rendered in YAML as if it were a plain tuple." in {
    forAll(spanGenerator, MinSuccessful(200)) {
      case exampleSpan @ Span(start, end) =>
        val equivalentTuple = start -> end

        val jsonFromSpan = exampleSpan.asJson

        val reconstitutedTuple = jsonFromSpan.as[(Int, Int)]

        reconstitutedTuple should be(Right(equivalentTuple))
    }
  }

  it should "be reconstituted from YAML as if it were a plain tuple" in {
    forAll(spanGenerator, MinSuccessful(200)) {
      case exampleSpan @ Span(start, end) =>
        val equivalentTuple = start -> end

        val jsonFromTuple = equivalentTuple.asJson

        val reconstitutedSpan = jsonFromTuple.as[Span]

        reconstitutedSpan should be(Right(exampleSpan))
    }
  }
}
