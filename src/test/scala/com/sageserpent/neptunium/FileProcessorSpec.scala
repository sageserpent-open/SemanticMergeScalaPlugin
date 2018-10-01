package com.sageserpent.neptunium
import java.nio.charset.Charset
import java.nio.file.Files
import scala.collection.JavaConverters._

import org.scalatest.{FlatSpec, Matchers}
import resource._

class FileProcessorSpec extends FlatSpec with Matchers {
  val charset = Charset.defaultCharset()

  private def temporarySourceScalaFile = {
    makeManagedResource(Files.createTempFile("source", ".scala").toFile)(_.delete())(List.empty)
  }

  private def temporaryOutputYamlFile = {
    makeManagedResource(Files.createTempFile("output", ".yml").toFile)(_.delete())(List.empty)
  }

  "a file processor" should "cope with empty input" in {
    for {
      sourceFile <- temporarySourceScalaFile
      outputFile <- temporaryOutputYamlFile
    } {
      val emptySource = ""

      Files.write(sourceFile.toPath, emptySource.getBytes(charset))

      FileProcessor.discoverStructure(sourceFile.getAbsolutePath, charset.name, outputFile.getAbsolutePath)

      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith("type: file")
    }
  }

  it should "cope with a trivial source that contains no classes or objects" in {
    val objectConstructName = "IsThatAllThereIs"

    for {
      sourceFile <- temporarySourceScalaFile
      outputFile <- temporaryOutputYamlFile
    } {
      val trivialSource =
        s"""
          |object $objectConstructName {
          |}
        """.stripMargin

      Files.write(sourceFile.toPath, trivialSource.getBytes(charset))

      FileProcessor.discoverStructure(sourceFile.getAbsolutePath, charset.name, outputFile.getAbsolutePath)

      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith("type: file")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include("type: module")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include(s"name: $objectConstructName")
    }
  }

  it should "cope with source code errors" in {
    val objectConstructName = "IsThatAllThereIs"

    for {
      sourceFile <- temporarySourceScalaFile
      outputFile <- temporaryOutputYamlFile
    } {
      val trivialSource =
        s"""
           |flobject $objectConstructName {
           |}
        """.stripMargin

      Files.write(sourceFile.toPath, trivialSource.getBytes(charset))

      FileProcessor.discoverStructure(sourceFile.getAbsolutePath, charset.name, outputFile.getAbsolutePath)

      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith("type: file")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include("parsingErrorsDetected: true")
    }
  }
}
