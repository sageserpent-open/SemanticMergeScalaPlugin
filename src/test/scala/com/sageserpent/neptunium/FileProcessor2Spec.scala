package com.sageserpent.neptunium

import com.sageserpent.neptunium.FileProcessor2._
import org.scalatest.{FlatSpec, Matchers}

class FileProcessor2Spec extends FlatSpec with Matchers {
  val lineMapping = null.asInstanceOf[LineMapping]
  import lineMapping.{Container, File, ParsingError, Terminal}
  val aFile = null.asInstanceOf[File]

  "A file" should "have a type" in {
    "aFile.`type`: String" should compile
  }

  it should "have a name" in {
    "aFile.name: String" should compile
  }

  it should "have a locationSpan" in {
    "aFile.locationSpan: LocationSpan" should compile
  }

  it should "have a footerSpan" in {
    "aFile.footerSpan: Span" should compile
  }

  it should "have optional children" in {
    "aFile.children: Seq[Container]" should compile
  }

  it should "have optional parsing errors" in {
    "aFile.parsingErrorsDetected: Boolean" should compile
    "aFile.parsingErrors: Seq[ParsingError]" should compile
  }

  it should "be serialized as JSON" in {
    """import io.circe.generic.auto._
      |import io.circe.syntax._
      |aFile.asJson""".stripMargin should compile
  }

  val aContainer = null.asInstanceOf[Container]

  "A container" should "have a type" in {
    "aContainer.`type`: String" should compile
  }

  it should "have a name" in {
    "aContainer.name: String" should compile
  }

  it should "have a locationSpan" in {
    "aContainer.locationSpan: LocationSpan" should compile
  }

  it should "have a headerSpan" in {
    "aContainer.headerSpan: Span" should compile
  }

  it should "have a footerSpan" in {
    "aContainer.footerSpan: Span" should compile
  }

  it should "have optional children" in {
    """import lineMapping.Declaration
      |aContainer.children: Seq[Declaration]""".stripMargin should compile
  }

  it should "be serialized as JSON" in {
    """import io.circe.generic.auto._
      |import io.circe.syntax._
      |aContainer.asJson""".stripMargin should compile
  }

  val aTerminal = null.asInstanceOf[Terminal]

  "A node" should "have a type" in {
    "aTerminal.`type`: String" should compile
  }

  it should "have a name" in {
    "aTerminal.name: String" should compile
  }

  it should "have a locationSpan" in {
    "aTerminal.locationSpan: LocationSpan" should compile
  }

  it should "have a span" in {
    "aTerminal.span: Span" should compile
  }

  it should "be serialized as JSON" in {
    """import io.circe.generic.auto._
      |import io.circe.syntax._
      |aTerminal.asJson""".stripMargin should compile
  }

  val aParsingError = null.asInstanceOf[ParsingError]

  "A parsing error" should "have a location " in {
    "aParsingError.location: LineAndOffSet" should compile
  }

  it should "have a message" in {
    "aParsingError.message: String" should compile
  }

  it should "be serialized as JSON" in {
    """import io.circe.generic.auto._
      |import io.circe.syntax._
      |aParsingError.asJson""".stripMargin should compile
  }
}
