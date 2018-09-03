import com.sageserpent.neptunium.FileProcessor2.File
import org.scalatest.{FlatSpec, Matchers}

class FileProcessor2Spec extends FlatSpec with Matchers {
  val aFileInstance = File()

  "A file instance" should "have a type" in {
    "aFileInstance.type: String" should compile
  }

  it should "have a name" in {}

  it should "have a locationSpan" in {}
  it should "have a footerSpan" in {}

  it should "have optional children" in {}

  it should "have optional parsing errors" in {}
}
