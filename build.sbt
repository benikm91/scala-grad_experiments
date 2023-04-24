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
lazy val scalaGradAutoFractional = (project in file("./scala-grad-auto-fractional"))
  .settings(
    name := "scala-grad-auto-fractional",
    basicSettings,
  ).dependsOn(
    scalaGradAutoForwardMode,
    scalaGradAutoReverseMode,
    scalaGradNumericalDifferentiation % "test->compile;test->test",
  )
  
// Extends forward-mode to work with spire.math.numeric
lazy val scalaGradAutoSpire = (project in file("./scala-grad-auto-spire"))
  .settings(
    name := "scala-grad-auto-spire",
    basicSettings,
    scalaTestSettings,
    spireDependency,
  ).dependsOn(
    scalaGradAutoForwardMode,
    scalaGradAutoReverseMode,
    scalaGradNumericalDifferentiation % "test->compile;test->test",
  )
  
// Show library usage
lazy val showcaseDeepLearning = (project in file("./showcases/showcase-deep-learning"))
  .settings(
      name := "showcase-deep-learning",
      basicSettings,
      libraryDependencies  ++= Seq(
        "org.scalanlp" %% "breeze" % "2.1.0",
      )
    ).dependsOn(
      scalaGradAutoFractional,
      scalaGradAutoSpire,
    )

lazy val root = (project in file("."))
  .settings(
    name := "scala-grad",
  )
  .aggregate(
    // API
    scalaGradApi,
    // Modes
    scalaGradAutoForwardMode,
    scalaGradAutoReverseMode,
    // Implementations
    scalaGradAutoFractional,
    scalaGradAutoSpire,
    // Showcases
    showcaseDeepLearning,
  )
  .enablePlugins(MdocPlugin)