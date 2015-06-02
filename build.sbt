mainClass := Some("com.sageSerpent.neptunium.Main")

assemblyJarName in assembly := "neptunium.jar"

resolvers += "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"

name := "Neptunium"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.6"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.6"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.6"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.7a"
