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
      case Success(true) => logger.info(s"Removed: $path successfully.")
      case Success(false) => logger.error(s"Failed to remove $path - no reason given.")
      case Failure(error) => logger.error(error)(s"Failed to remove $path.")
    }
  }

  for {
    locationOfLinkAlias <- makeManagedResource(Files.createTempDirectory("SemanticMergeScalaPlugin"))(removeJunk)(List.empty)
    linkAlias = linkAliasIn(locationOfLinkAlias)
    _ <- makeManagedResource(Files.createLink(linkAlias, jarWithNonPossiblyNonStandardExtensionProvidingThisCode))(removeJunk)(List.empty)
  } {
    new ProcessBuilder(List("java", "-cp", jarWithNonPossiblyNonStandardExtensionProvidingThisCode.toString, "com.sageSerpent.neptunium.Subprocess") ++ args :+ linkAlias.toString).inheritIO.start().wait()

    logger.info("Plugin exiting.")
  }

  def linkAliasIn(locationOfLinkAlias: Path): Path = {
    val linkAlias = locationOfLinkAlias.resolve("SemanticMergeScalaPlugin.jar")
    linkAlias.toFile.setReadable(true)
    linkAlias.toFile.setWritable(true)
    linkAlias.toFile.setExecutable(true)
    linkAlias
  }
}
