package com.sageserpent.neptunium

import com.sageserpent.neptunium.YamlModel._
import org.scalatest.{FlatSpec, Matchers}

class YamlModelSpec extends FlatSpec with Matchers {
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
    """import lineMapping.Declaration
      |val one: Seq[Declaration] = aFile.children
      |val two: File = aFile.copy(children = Seq.empty[Declaration])""".stripMargin should compile
  }

  it should "have optional parsing errors" in {
    """val one: Boolean = aFile.parsingErrorsDetected
      |val two: Seq[ParsingError] = aFile.parsingErrors""".stripMargin should compile
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
      |val one: Seq[Declaration] = aContainer.children
      |val two: Container = aContainer.copy(children = Seq.empty[Declaration])""".stripMargin should compile
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
    "aParsingError.location: LineAndOffset" should compile
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
