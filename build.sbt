//enablePlugins(ScalaNativePlugin)

name := "advent-of-code-2024"

version := "0.1"

scalaVersion := "3.5.2"

lazy val versions = new {
  val zio       = "2.1.13"
  val nio       = "2.0.2"
  val fastparse = "3.1.1"
}

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse"    % versions.fastparse,
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "dev.zio"     %% "zio"          % versions.zio,
  "dev.zio"     %% "zio-nio"      % versions.nio,
  "dev.zio"     %% "zio-test"     % versions.zio,
  "dev.zio"     %% "zio-streams"  % versions.zio,
  "dev.zio"     %% "zio-test"     % versions.zio % Test,
  "dev.zio"     %% "zio-test-sbt" % versions.zio % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
