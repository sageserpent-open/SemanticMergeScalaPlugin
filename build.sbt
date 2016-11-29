import sbt.IO

val allInOneCommandScript = taskKey[File]("Creates a self-contained Windows command script.")

lazy val neptunium = (project in file(".")).settings(
  mainClass := Some("com.sageserpent.neptunium.Main"),

  assemblyJarName in assembly := "neptunium.jar",

  name := "Neptunium",
  version := "1.0",

  scalaVersion := "2.11.7",

  libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.7",
  libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.7",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7",

  libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.8",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0",

  libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4",

  libraryDependencies += "org.log4s" %% "log4s" % "1.2.1",

  libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.5",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.5",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.5",

    allInOneCommandScript := {
    val jarFile = assembly.value
    val stubFile = baseDirectory.value / "neptuniumStub.cmd"
    val allInOneCommandScriptFile = target.value / "neptunium.cmd"
    IO.append(allInOneCommandScriptFile, IO.readBytes(stubFile))
    IO.append(allInOneCommandScriptFile, IO.readBytes(jarFile))
    allInOneCommandScriptFile
  }
)


