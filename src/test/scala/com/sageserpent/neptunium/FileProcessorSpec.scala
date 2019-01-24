package com.sageserpent.neptunium
import java.io.File
import java.nio.charset.Charset
import java.nio.file.Files

import scala.collection.JavaConverters._
import org.scalatest.{FlatSpec, Matchers}
import resource._

import scala.util.matching.Regex

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
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith(s"name: ${sourceFile.getAbsolutePath}")
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
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith(s"name: ${sourceFile.getAbsolutePath}")
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
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith(s"name: ${sourceFile.getAbsolutePath}")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include("parsingErrorsDetected: true")
    }
  }

  it should "not pollute the YAML with discriminators" in {
    val objectConstructName = "IsThatAllThereIs"

    for {
      sourceFile <- temporarySourceScalaFile
      outputFile <- temporaryOutputYamlFile
    } {
      val trivialSource =
        s"""
           |object $objectConstructName {
           |  def foo() = 2
           |}
        """.stripMargin

      Files.write(sourceFile.toPath, trivialSource.getBytes(charset))

      FileProcessor.discoverStructure(sourceFile.getAbsolutePath, charset.name, outputFile.getAbsolutePath)

      no(Files.readAllLines(outputFile.toPath).asScala) should include("Container")
      no(Files.readAllLines(outputFile.toPath).asScala) should include("Terminal")
    }
  }

  it should "cope with real world Scala code" in {
    for {
      outputFile <- temporaryOutputYamlFile
    } {
      val scalaFilename = "bigLumpOfScala.scala"
      val sourceFileUrl = getClass.getResource(scalaFilename)

      FileProcessor.discoverStructure(new File(sourceFileUrl.toURI).getAbsolutePath,
                                      charset.name,
                                      outputFile.getAbsolutePath)

      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith("type: file")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith regex s"""${Regex.quote("name: ")}.*$scalaFilename""".r
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include(s"name: WorldBehaviours")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include(
        s"name: WorldSpecUsingWorldReferenceImplementation")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include(
        s"name: WorldSpecUsingWorldEfficientInMemoryImplementation")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include(
        s"name: WorldSpecUsingWorldRedisBasedImplementation")
    }
  }

  it should "cope with real world SBT code" in {
    for {
      outputFile <- temporaryOutputYamlFile
    } {
      val scalaFilename = "dollopOf.sbt"
      val sourceFileUrl = getClass.getResource(scalaFilename)

      FileProcessor.discoverStructure(new File(sourceFileUrl.toURI).getAbsolutePath,
                                      charset.name,
                                      outputFile.getAbsolutePath)

      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith("type: file")
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should startWith regex s"""${Regex.quote("name: ")}.*$scalaFilename""".r
      exactly(1, Files.readAllLines(outputFile.toPath).asScala) should include(s"name: neptunium")
    }
  }
}
