import com.sageserpent.neptunium.FileProcessor2._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class FileProcessor2Spec extends FlatSpec with Matchers with MockFactory {
  val aFile = mock[File]

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
    "aFile.parsingError: Seq[ParsingError]" should compile
  }

  val aContainer = mock[Container]

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
    "aContainer.children: Seq[Declaration]" should compile
  }

  val aNode = mock[Node]

  "A node" should "have a type" in {
    "aNode.`type`: String" should compile
  }

  it should "have a name" in {
    "aNode.name: String" should compile
  }

  it should "have a locationSpan" in {
    "aNode.locationSpan: LocationSpan" should compile
  }

  it should "have a span" in {
    "aNode.span: Span" should compile
  }

  val aParsingError = mock[ParsingError]

  "A parsing error" should "have a location " in {
    "aParsingError.location: LineAndOffSet" should compile
  }

  it should "have a message" in {
    "aParsingError.message: String" should compile
  }
}
