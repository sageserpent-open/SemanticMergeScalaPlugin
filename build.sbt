resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val neptunium = (project in file(".")).settings(
  mainClass := Some("com.sageserpent.neptunium.Main"),
  assemblyJarName in assembly := "neptunium.jar",
  name := "Neptunium",
  version := "1.1-SNAPSHOT",
  scalaVersion := "2.12.4",
  libraryDependencies += "org.scala-lang"           % "scala-library"    % "2.12.4",
  libraryDependencies += "org.scalaz.stream"        %% "scalaz-stream"   % "0.8.6",
  libraryDependencies += "org.scalaz"               %% "scalaz-core"     % "7.2.7",
  libraryDependencies += "com.jsuereth"             %% "scala-arm"       % "2.0",
  libraryDependencies += "org.log4s"                %% "log4s"           % "1.3.3",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-api"        % "2.7",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-core"       % "2.7",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.7",
  libraryDependencies += "com.sageserpent"          %% "americium"       % "0.1.5",
  libraryDependencies += "org.scalameta"            %% "scalameta"       % "4.0.0-M10",
  libraryDependencies += "io.circe"                 %% "circe-yaml"      % "0.8.0",
  libraryDependencies += "io.circe"                 %% "circe-core"      % "0.10.0-M2",
  libraryDependencies += "io.circe"                 %% "circe-parser"    % "0.10.0-M2",
  libraryDependencies += "io.circe"                 %% "circe-generic"   % "0.10.0-M2",
  libraryDependencies += "org.scalatest"            %% "scalatest"       % "3.0.5" % "test",
  libraryDependencies += "org.scalamock"            %% "scalamock"       % "4.1.0" % "test",
  libraryDependencies += "org.scalacheck"           %% "scalacheck"      % "1.14.0" % "test",
  libraryDependencies += "com.github.ichoran"       %% "thyme"           % "0.1.2-SNAPSHOT" % "test",
  resolvers += Resolver.jcenterRepo
)
