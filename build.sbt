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

lazy val scalaGradApi = (project in file("./scala-grad-api"))
  .settings(
    name := "scala-grad-api",
    basicSettings,
    scalaTestSettings,
  )
  
lazy val scalaGradScalarFractionalApi = (project in file("./scala-grad-scalar-fractional-api"))
  .settings(
    name := "scala-grad-scalar-fractional-api",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test",
  )

lazy val scalaGradScalarFractionalForwardMode = (project in file("./scala-grad-scalar-fractional-forward-mode"))
  .settings(
    name := "scala-grad-scalar-fractional-forward-mode",
    basicSettings,
  ).dependsOn(
    scalaGradScalarFractionalApi,
    scalaGradScalarFractionalApi % "test->test",
  )
  
lazy val scalaGradScalarFractionalReverseMode = (project in file("./scala-grad-scalar-fractional-reverse-mode"))
  .settings(
    name := "scala-grad-scalar-fractional-reverse-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradScalarFractionalApi % "compile->compile;test->test"
  )

lazy val scalaGradScalarFractionalReverseModeNoLet = (project in file("./scala-grad-scalar-fractional-reverse-mode-no-let"))
  .settings(
    name := "scala-grad-scalar-fractional-reverse-mode-no-let",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradScalarFractionalApi % "compile->compile;test->test"
  )

lazy val scalaGradScalarSpireApi = (project in file("./scala-grad-scalar-spire-api"))
  .settings(
    name := "scala-grad-scalar-spire-api",
    basicSettings,
    spireDependency,
    scalaTestSettings,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test"
  )
  
lazy val scalaGradScalarSpireForwardMode = (project in file("./scala-grad-scalar-spire-forward-mode"))
  .settings(
    name := "scala-grad-scalar-spire-forward-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradScalarSpireApi,
    scalaGradScalarSpireApi % "test->test",
  )
  
lazy val showcaseDeepLearning = (project in file("./showcases/showcase-deep-learning"))
  .settings(
      name := "showcase-deep-learning",
      basicSettings,
    ).dependsOn(
      scalaGradScalarFractionalForwardMode,
      scalaGradScalarFractionalReverseMode,
      scalaGradScalarFractionalReverseModeNoLet,
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
    scalaGradScalarFractionalReverseModeNoLet,
    // Spire Numeric
    scalaGradScalarSpireApi,
    scalaGradScalarSpireForwardMode,
    // Showcases
    showcaseDeepLearning,
  )