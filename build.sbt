ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "3.2.2"
ThisBuild / organization := "ch.benikm91"

lazy val scalaTestSettings = Seq(
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14" % Test, 
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test,
  libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % Test
)

lazy val spireDependency = Seq(
    libraryDependencies += "org.typelevel" %% "spire" % "0.18.0"
)

lazy val basicSettings = Seq(
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
)

lazy val scalaGradApi = (project in file("./scala-grad-api"))
  .settings(
    name := "scala-grad-api",
    basicSettings,
  )
  
lazy val scalaGradScalarFractionalApi = (project in file("./scala-grad-scalar-fractional-api"))
  .settings(
    name := "scala-grad-scalar-fractional-api",
    basicSettings,
  ).dependsOn(
    scalaGradApi
  )

lazy val scalaGradScalarFractionalForwardMode = (project in file("./scala-grad-scalar-fractional-forward-mode"))
  .settings(
    name := "scala-grad-scalar-fractional-forward-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradScalarFractionalApi
  )
  
lazy val scalaGradScalarFractionalReverseMode = (project in file("./scala-grad-scalar-fractional-reverse-mode"))
  .settings(
    name := "scala-grad-scalar-fractional-reverse-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradScalarFractionalApi
  )

lazy val scalaGradScalarSpireApi = (project in file("./scala-grad-scalar-spire-api"))
  .settings(
    name := "scala-grad-scalar-spire-api",
    basicSettings,
    spireDependency,
  ).dependsOn(
    scalaGradApi
  )
  
lazy val scalaGradScalarSpireForwardMode = (project in file("./scala-grad-scalar-spire-forward-mode"))
  .settings(
    name := "scala-grad-scalar-spire-forward-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradScalarSpireApi
  )
  
lazy val root = (project in file("."))
  .settings(
    name := "scala-grad",
  ).aggregate(
    scalaGradApi,
    // Fractional
    scalaGradScalarFractionalApi,
    scalaGradScalarFractionalForwardMode,
    scalaGradScalarFractionalReverseMode,
    // Spire Numeric
    scalaGradScalarSpireApi,
    scalaGradScalarSpireForwardMode,
  )