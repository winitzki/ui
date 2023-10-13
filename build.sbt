val scala2V = "2.13.11"
val scala3V = "3.3.1"
val scalaV = scala2V

val utest = "com.lihaoyi" %% "utest" % "0.8.1" % Test
def utestFramework = new TestFramework("utest.runner.Framework")

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
  testFrameworks += utestFramework,
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Compile
    )
    case Some((3, _)) => Seq.empty // No need for scala-reflect with Scala 3.
  }) ++ Seq(
    utest,
  ),
)

lazy val core_elm = (project in file("core_elm")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += utestFramework,
  libraryDependencies ++= Seq(
    utest,
  ),
)

lazy val example_elm = (project in file("example_elm")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += utestFramework,
  libraryDependencies ++= Seq(
    utest,
  ),
).dependsOn(core_elm)

lazy val backend_awt = (project in file("backend_awt")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += utestFramework,
  libraryDependencies ++= Seq(
    utest,
  ),
).dependsOn(core_elm, macros, example_elm % "test->compile")

lazy val backend_swing = (project in file("backend_swing")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += utestFramework,
  libraryDependencies ++= Seq(
    utest,
  ),
).dependsOn(core_elm, macros, backend_awt, example_elm % "test->compile")
