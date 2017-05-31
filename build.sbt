import sbt.IO

val allInOneCommandScript = taskKey[File]("Creates a self-contained Windows command script.")

lazy val neptunium = (project in file(".")).settings(
  mainClass := Some("com.sageserpent.neptunium.Main"),

  assemblyJarName in assembly := "neptunium.jar",

  name := "Neptunium",
  version := "1.0",

  scalaVersion := "2.11.8",

  libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8",
  libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.8",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8",

  libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.8.6",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.7",

  libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0",

  libraryDependencies += "org.log4s" %% "log4s" % "1.3.3",

  libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.7",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.7",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.7",

  libraryDependencies += "com.sageserpent" %% "americium" % "0.1.1",

  resolvers += Resolver.jcenterRepo,

    allInOneCommandScript := {
    val jarFile = assembly.value
    val stubFile = baseDirectory.value / "neptuniumStub.cmd"
    val allInOneCommandScriptFile = target.value / "neptunium.cmd"
    IO.write(allInOneCommandScriptFile, IO.readBytes(stubFile))
    IO.append(allInOneCommandScriptFile, IO.readBytes(jarFile))
    allInOneCommandScriptFile
  }
)


