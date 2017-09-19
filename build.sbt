organization := "com.github.deaktator"

name := "monads"

description := "Modeling effects other than failure / short circuiting."

scalaVersion := "2.11.11"

lazy val kind_projector_version = "0.9.4"

incOptions := incOptions.value.withNameHashing(true)

javacOptions ++= Seq("-Xlint:unchecked")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xverify"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % kind_projector_version)
