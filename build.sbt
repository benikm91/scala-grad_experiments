ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "3.3.0-RC3"
ThisBuild / organization := "ch.benikm91"

lazy val scalaTestSettings = Seq(
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14" % Test, 
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test,
  libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % Test
)

lazy val spireDependency = Seq(
    libraryDependencies += "org.typelevel" %% "spire" % "0.18.0"
)

lazy val breezeDependency = Seq(
  libraryDependencies += "org.scalanlp" %% "breeze" % "2.1.0"
)

lazy val basicSettings = Seq(
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",
)

lazy val scalaGradApi = (project in file("./scala-grad-api"))
  .settings(
    name := "scala-grad-api",
    basicSettings,
    spireDependency,
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

lazy val scalaGradScalarBreezeApi = (project in file("./scala-grad-scalar-breeze-api"))
  .settings(
    name := "scala-grad-scalar-breeze-api",
    basicSettings,
    breezeDependency,
  ).dependsOn(
    scalaGradApi
  )
  
lazy val scalaGradScalarBreezeForwardMode = (project in file("./scala-grad-scalar-breeze-forward-mode"))
  .settings(
    name := "scala-grad-scalar-breeze-forward-mode",
    basicSettings,
    breezeDependency,
  ).dependsOn(
    scalaGradScalarBreezeApi,
    scalaGradScalarFractionalForwardMode,
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
    // Breeze
    scalaGradScalarBreezeApi,
    scalaGradScalarBreezeForwardMode,
  )