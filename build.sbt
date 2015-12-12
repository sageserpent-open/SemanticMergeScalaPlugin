import sbt.IO

val allInOneCommandScript = taskKey[File]("Creates a self-contained Windows command script.")

lazy val neptunium = (project in file(".")).settings(
  mainClass := Some("com.sageSerpent.neptunium.Main"),

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

  allInOneCommandScript := {
    val jarFile = assembly.value
    val stubFile = baseDirectory.value / "neptuniumStub.cmd"
    val allInOneCommandScriptFile = target.value / "neptunium.cmd"
    IO.append(allInOneCommandScriptFile, IO.readBytes(stubFile))
    IO.append(allInOneCommandScriptFile, IO.readBytes(jarFile))
    allInOneCommandScriptFile
  }
)


