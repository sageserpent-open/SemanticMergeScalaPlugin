/**
  * Created by Gerard on 17/05/2015.
  */

package com.sageSerpent.neptunium

import java.io.File
import java.nio.file.{Files, Path}

import org.log4s._
import resource._

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConversions._

object Main extends App {
  private[this] val logger = getLogger

  val jarWithNonPossiblyNonStandardExtensionProvidingThisCode = new File(Main.getClass.getProtectionDomain.getCodeSource.getLocation.getPath).toPath

  def removeJunk(path: Path): Unit = {
    logger.info(s"Removing: $path.")
    Try(path.toFile.delete()) match {
      case Success(true) => logger.info(s"Removed: '$path' successfully.")
      case Success(false) => logger.error(s"Failed to remove '$path' - no reason given.")
      case Failure(error) => logger.error(error)(s"Failed to remove '$path'.")
    }
  }

  for {
    locationOfLibraryJar <- makeManagedResource(temporaryDirectory)(removeJunk)(List.empty)
    libraryJar <- makeManagedResource(temporaryLibraryJar(locationOfLibraryJar))(removeJunk)(List.empty)
  } {
    new ProcessBuilder(List("java", "-cp", jarWithNonPossiblyNonStandardExtensionProvidingThisCode.toString, Subprocess.getClass.getName.stripSuffix("$")) ++ args :+ libraryJar.toString).inheritIO.start().waitFor()

    logger.info("Plugin exiting.")
  }

  private def temporaryDirectory = {
    val tempDirectory = Files.createTempDirectory("SemanticMergeScalaPlugin")
    tempDirectory.toFile.setWritable(true)
    tempDirectory
  }

  private def temporaryLibraryJar(locationOfLibraryJar: Path) = {
    val libraryJar = Files.copy(jarWithNonPossiblyNonStandardExtensionProvidingThisCode, locationOfLibraryJar.resolve("SemanticMergeScalaPlugin.jar"))
    libraryJar.toFile.setReadable(true)
    libraryJar.toFile.setWritable(true)
    libraryJar.toFile.setExecutable(true)
    libraryJar
  }
}
