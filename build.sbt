val commonSettings = Seq(
  scalaVersion := "2.11.1")

lazy val quasar = project.in(file("."))
  .settings(commonSettings: _*)
  .dependsOn(quasarMacro)

lazy val quasarMacro = project.in(file("quasar-macro"))
  .settings(name := "quasar-macro")
  .settings(commonSettings: _*)
  .settings(libraryDependencies +=
    "org.scala-lang" % "scala-reflect" % scalaVersion.value)
