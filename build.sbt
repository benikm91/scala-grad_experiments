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
    Compile / resourceDirectory := baseDirectory.value / "res",
    Test / scalaSource := baseDirectory.value / "test",
)

// Provides basic API
lazy val scalaGradApi = (project in file("./scala-grad-api"))
  .settings(
    name := "scala-grad-api",
    basicSettings,
    scalaTestSettings,
  )
  
// Implements API with Numerical Differentitation
lazy val scalaGradNumericalDifferentiation = (project in file("./scala-grad-numerical-differentiation"))
  .settings(
    name := "scala-grad-numerical-differentiation",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test",
  )

// Implements API with forward-mode of Automatic Differentitation
lazy val scalaGradAutoForwardMode = (project in file("./scala-grad-auto-forward-mode"))
  .settings(
    name := "scala-grad-auto-forward-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test",
  )

// Implements API with reverse-mode of Automatic Differentitation
lazy val scalaGradAutoReverseMode = (project in file("./scala-grad-auto-reverse-mode"))
  .settings(
    name := "scala-grad-auto-reverse-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test",
  )

// Extends forward-mode to work with scala.math.fractional
lazy val scalaGradScalarFractionalForwardMode = (project in file("./scala-grad-scalar-fractional-forward-mode"))
  .settings(
    name := "scala-grad-scalar-fractional-forward-mode",
    basicSettings,
  ).dependsOn(
    scalaGradAutoForwardMode,
    scalaGradNumericalDifferentiation % "test->compile;test->test",
  )
  
// Extends reverse-mode to work with scala.math.fractional
lazy val scalaGradScalarFractionalReverseMode = (project in file("./scala-grad-scalar-fractional-reverse-mode"))
  .settings(
    name := "scala-grad-scalar-fractional-reverse-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradAutoReverseMode,
    scalaGradNumericalDifferentiation % "test->compile;test->test",
  )

// Extends forward-mode to work with spire.math.numeric
lazy val scalaGradScalarSpireForwardMode = (project in file("./scala-grad-scalar-spire-forward-mode"))
  .settings(
    name := "scala-grad-scalar-spire-forward-mode",
    basicSettings,
    scalaTestSettings,
    spireDependency,
  ).dependsOn(
    scalaGradAutoForwardMode,
    scalaGradNumericalDifferentiation % "test->compile;test->test",
  )
  
// Show library usage
lazy val showcaseDeepLearning = (project in file("./showcases/showcase-deep-learning"))
  .settings(
      name := "showcase-deep-learning",
      basicSettings,
    ).dependsOn(
      // Fractional
      scalaGradScalarFractionalForwardMode,
      scalaGradScalarFractionalReverseMode,
      // Spire Numeric
      scalaGradScalarSpireForwardMode,
    )

lazy val root = (project in file("."))
  .settings(
    name := "scala-grad",
  )
  .dependsOn(
    scalaGradApi,
    // Forward
    scalaGradAutoForwardMode,
    // Fractional
    scalaGradScalarFractionalForwardMode,
    scalaGradScalarFractionalReverseMode,
    // Spire Numeric
    scalaGradScalarSpireForwardMode,
    // Showcases
    showcaseDeepLearning,
  )
  .enablePlugins(MdocPlugin)