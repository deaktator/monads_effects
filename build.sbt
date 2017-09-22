organization := "com.github.deaktator"

name := "monads"

description := "Modeling effects other than failure / short circuiting."

lazy val _scalaVersion = "2.11.8"
scalaVersion := _scalaVersion

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

// There's a bug, SI-2712 (https://issues.scala-lang.org/browse/SI-2712),
// whereby the Scala compiler has type inference problems when dealing with
// certain type constructors.  A fix to some of this is necessary to get some
// of ideas in this project to work. SI-2712 was back-ported to 2.11.11 and is
// in the 2.12 branch but not in Scala 2.11.
//
// This compiler plugin (written by the same author that contributed the fix to
// the Scala compiler) is necessary to fix the issue in Scala 2.11.8.
//
// When using scala 2.11.11 or scala 2.12, this plugin is unnecessary but the
// following scala compiler optionmust be provided:  "-Ypartial-unification".
//
// To see how this helps, try commenting out the following line and compiling.
//
addCompilerPlugin("com.milessabin" % ("si2712fix-plugin_" + _scalaVersion) % "1.2.0")

libraryDependencies += "org.typelevel" %% "cats-core" % "0.9.0"
