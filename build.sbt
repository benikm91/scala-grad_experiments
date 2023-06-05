ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "3.2.2"
ThisBuild / organization := "ch.benikm91"

lazy val scalismoDependency = Seq(
  libraryDependencies ++= Seq(
    "ch.unibas.cs.gravis" %% "scalismo" % "0.91.0",
    "ch.unibas.cs.gravis" %% "scalismo-ui" % "0.91.0"
  )
)

lazy val scaltirDependency = Seq(
      resolvers +=  Resolver.sonatypeRepo("snapshots"), 
      libraryDependencies += "ch.unibas.cs.gravis" %% "scaltair" % "0.1-SNAPSHOT"
)

lazy val scalaTestSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.2.14" % Test, 
    "org.scalatest" %% "scalatest" % "3.2.14" % Test,
    "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % Test
  )
)

lazy val spireDependency = Seq(
    libraryDependencies += "org.typelevel" %% "spire" % "0.18.0"
)

lazy val breezeDependency = Seq(
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "2.1.0",
  )
)

lazy val basicSettings = Seq(
    Compile / scalaSource := baseDirectory.value / "src",
    Compile / resourceDirectory := baseDirectory.value / "res",
    Test / scalaSource := baseDirectory.value / "test",
)

// ScalaGrad API
lazy val scalaGradApi = (project in file("./scala-grad-api"))
  .settings(
    name := "scala-grad-api",
    basicSettings,
    scalaTestSettings,
  )
  
// Numerical Differentitation
lazy val scalaGradNumericalDifferentiation = (project in file("./scala-grad-numerical-differentiation"))
  .settings(
    name := "scala-grad-numerical-differentiation",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test",
  )

// Forward-mode 
lazy val scalaGradAutoForwardMode = (project in file("./scala-grad-auto-forward-mode"))
  .settings(
    name := "scala-grad-auto-forward-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test",
  )

// Reverse-mode
lazy val scalaGradAutoReverseMode = (project in file("./scala-grad-auto-reverse-mode"))
  .settings(
    name := "scala-grad-auto-reverse-mode",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradApi,
    scalaGradApi % "test->test",
  )

// Add derivers for scala.math.fractional
lazy val scalaGradAutoFractional = (project in file("./scala-grad-auto-fractional"))
  .settings(
    name := "scala-grad-auto-fractional",
    basicSettings,
  ).dependsOn(
    scalaGradAutoForwardMode,
    scalaGradAutoReverseMode,
    scalaGradNumericalDifferentiation % "test->compile;test->test",
  )

// Add derivers for spire.math.numeric
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
  
// ScalaGrad Linear Algebra API
lazy val scalaGradLinearAlgebraApi = (project in file("./scala-grad-linear-algebra-api"))
  .settings(
    name := "scala-grad-linear-algebra-api",
    basicSettings,
    breezeDependency,
  ).dependsOn(
    scalaGradApi,
    scalaGradAutoFractional,
    scalaGradAutoSpire,
  )
  
// ScalaGrad Linear Algebra Breeze 
lazy val scalaGradLinearAlgebraAutoBreeze = (project in file("./scala-grad-linear-algebra-auto-breeze"))
  .settings(
    name := "scala-grad-linear-algebra-auto-breeze",
    basicSettings,
    scalaTestSettings,
  ).dependsOn(
    scalaGradLinearAlgebraApi,
  )

// Show library usage
lazy val showcaseDeepLearning = (project in file("./showcases/showcase-deep-learning"))
  .settings(
      name := "showcase-deep-learning",
      basicSettings,
      breezeDependency,
    ).dependsOn(
      scalaGradAutoFractional,
      scalaGradAutoSpire,
      scalaGradLinearAlgebraApi,
      scalaGradLinearAlgebraAutoBreeze
    )

// Show library usage
lazy val showcaseProbabilisticProgramming = (project in file("./showcases/showcase-probablistic-programming"))
  .settings(
      name := "showcase-probablistic-programming",
      basicSettings,
      breezeDependency,
      scalismoDependency,
      scaltirDependency,
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