val scala2V = "2.13.11"
val scala3V = "3.3.1"
val scalaV = scala2V

val munitTest = "org.scalameta" %% "munit" % "0.7.29" % Test
def munitFramework = new TestFramework("munit.Framework")

val assertVerboseTest = "com.eed3si9n.expecty" %% "expecty" % "0.16.0" % Test
val enumeratum = "com.beachape" %% "enumeratum" % "1.7.3"
val flatlaf = "com.formdev" % "flatlaf" % "3.2.2"

val curryhoward = "io.chymyst" %% "curryhoward" % "0.3.8"

lazy val root = (project in file(".")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaV),
  name := "ui-root",
).aggregate(
  macros,
  core_elm,
  example_elm,
  backend_awt,
)

lazy val macros = (project in file("macros")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Compile
    )
    case Some((3, _)) => Seq.empty // No need for scala-reflect with Scala 3.
  }) ++ Seq(
    munitTest,
  ),
)

lazy val core_elm = (project in file("core_elm")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    munitTest,
  ),
)

lazy val example_elm = (project in file("example_elm")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    munitTest,
  ),
).dependsOn(core_elm)

lazy val backend_awt = (project in file("backend_awt")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    munitTest,
  ),
).dependsOn(core_elm, macros, example_elm % "test->compile")

lazy val backend_swing = (project in file("backend_swing")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    flatlaf,
    munitTest,
  ),
).dependsOn(core_elm, macros, backend_awt, example_elm % "test->compile")

