val scalaV = "2.13.11"
val scala3V = "3.3.+"

lazy val root = (project in file(".")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaV, scala3V),
).aggregate(
  macros,
  core_elm,
  backend_awt,
)

lazy val macros = (project in file("macros")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaV, scala3V),
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Compile
    )
    case Some((3, _)) => Seq.empty
  }) ++ Seq(
    "com.lihaoyi" %% "utest" % "latest.integration" % Test,
  ),
  testFrameworks += new TestFramework("utest.runner.Framework"),
)

lazy val core_elm = (project in file("core_elm")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaV, scala3V),
)

lazy val backend_awt = (project in file("backend_awt")).settings(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaV, scala3V),
).dependsOn(core_elm, macros)


