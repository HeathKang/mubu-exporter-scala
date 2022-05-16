val scala3Version = "3.1.2"
val scala2Version = "2.13.8"
val http4sVersion = "1.0.0-M32"
val circeVersion = "0.14.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mubu-exporter",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala2Version,

    libraryDependencies += "org.http4s" %% "http4s-ember-client" % http4sVersion,
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.11",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-circe" % http4sVersion,
      // Optional for auto-derivation of JSON codecs
      "io.circe" %% "circe-generic" % circeVersion,
      // Optional for string interpolation to JSON model
      "io.circe" %% "circe-literal" % circeVersion,
      "io.circe" %% "circe-optics" % circeVersion
    )
  )
