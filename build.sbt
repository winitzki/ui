val scala2V = "2.13.11"
val scala3V = "3.3.1"
val scalaV = scala2V

//val utest = "com.lihaoyi" %% "utest" % "0.8.1" % Test
//def utestFramework = new TestFramework("utest.runner.Framework")

val munit = "org.scalameta" %% "munit" % "0.7.29" % Test
def munitFramework = new TestFramework("munit.Framework")

val fastparse = "com.lihaoyi" %% "fastparse" % "3.0.2"
val assertVerbose = "com.eed3si9n.expecty" %% "expecty" % "0.16.0" % Test
val enumeratum = "com.beachape" %% "enumeratum" % "1.7.3"
val flatlaf = "com.formdev" % "flatlaf" % "3.2.2"

val cbor1 = "co.nstant.in" % "cbor" % "0.9"
val cbor2  ="com.upokecenter" % "cbor" % "4.5.2"
val cbor3 = "io.bullet" %% "borer-core" % "1.8.0"

lazy val root = (project in file(".")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaV),
  name := "ui-root",
).aggregate(
  macros,
  core_elm,
  example_elm,
  backend_awt,
  dhall,
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
    munit,
  ),
)

lazy val core_elm = (project in file("core_elm")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    munit,
  ),
)

lazy val example_elm = (project in file("example_elm")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    munit,
  ),
).dependsOn(core_elm)

lazy val backend_awt = (project in file("backend_awt")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    munit,
  ),
).dependsOn(core_elm, macros, example_elm % "test->compile")

lazy val backend_swing = (project in file("backend_swing")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    flatlaf,
    munit,
  ),
).dependsOn(core_elm, macros, backend_awt, example_elm % "test->compile")

lazy val dhall = (project in file("dhall")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scala2V, scala3V),
  testFrameworks += munitFramework,
  libraryDependencies ++= Seq(
    fastparse,
    munit,
    assertVerbose,
    enumeratum,
//    cbor1,
    cbor2,
    cbor3,
  ),
)
