import Dependencies.*

/* Core settings */
ThisBuild / scalaVersion := "3.6.4"
val groupId = "com.github.gerlichlab"
val projectName = "gerlib"
val rootPkg = s"at.ac.oeaw.imba.gerlich.$projectName"
val gitHubOwner = "gerlichlab"
val gitPubUrl = s"https://github.com/$gitHubOwner/$projectName.git"
val primaryJavaVersion = "11"
val primaryOs = "ubuntu-latest"
val isPrimaryOsAndPrimaryJavaTest = s"runner.os == '$primaryOs' && runner.java-version == '$primaryJavaVersion'"

// Needed for ZARR (jzarr) (?)
ThisBuild / resolvers += "Unidata UCAR" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases/"

/* scalafix */
ThisBuild / semanticdbEnabled := true
ThisBuild / scalafixDependencies ++= Seq(
  "cats", "cats-effect", "fs2"
).map(prj => "org.typelevel" %% s"typelevel-scalafix-${prj}" % "0.2.0")

/* sbt-github-actions settings */
ThisBuild / githubWorkflowOSes := Seq(primaryOs, "ubuntu-20.04", "macos-latest")
ThisBuild / githubWorkflowTargetBranches := Seq("main")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()
ThisBuild / githubWorkflowJavaVersions := Seq(primaryJavaVersion, "17", "19", "21").map(JavaSpec.temurin)
ThisBuild / githubWorkflowBuildPreamble ++= Seq(
  // Account for the absence of sbt in newer versions of the setup-java GitHub Action.
  WorkflowStep.Run(commands = List("brew install sbt"), cond = Some("contains(runner.os, 'macos')")), 
  /* Add linting and formatting checks, but only limit to a single platform + Java combo. */
  WorkflowStep.Sbt(
    List("scalafmtCheckAll"), 
    name = Some("Check formatting with scalafmt"),
    cond = Some(isPrimaryOsAndPrimaryJavaTest),
  ), 
  WorkflowStep.Sbt(
    List("scalafixAll --check"), 
    name = Some("Lint with scalafix"), 
    cond = Some(isPrimaryOsAndPrimaryJavaTest),
  ),
)

ThisBuild / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

ThisBuild / testOptions += Tests.Argument("-oF") // full stack traces

lazy val root = project
  .in(file("."))
  .aggregate(cell, geometry, graph, imaging, io, json, numeric, pan, refinement, roi, testing, zarr)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(
    name := projectName,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion), 
    buildInfoPackage := s"$rootPkg.internal"
  )

lazy val cell = defineModule("cell")(project)
  .dependsOn(numeric)

lazy val geometry = defineModule("geometry")(project)
  .dependsOn(numeric, refinement)
  .settings(
    libraryDependencies ++= Seq(
      squants,
    )
  )

lazy val graph = defineModule("graph")(project)
  .settings(
    libraryDependencies ++= Seq(
      scalaGraphCore,
      catsLaws % Test,
      disciplineScalatest % Test,
    ), 
  )

lazy val io = defineModule("io")(project)
  .dependsOn(cell, geometry, imaging, pan, roi)
  .settings(
    libraryDependencies ++= Seq(
      fs2Csv, 
      fs2IO, 
      os,
    )
  )

lazy val imaging = defineModule("imaging")(project)
  .dependsOn(json, numeric, refinement)
  .settings(
    libraryDependencies ++= Seq(
      iron % Test,
      ironScalacheck % Test,
      uJson, 
      uPickle,
    )
  )

lazy val json = defineModule("json")(project)
  .dependsOn(geometry, numeric)
  .settings(
    libraryDependencies ++= Seq(
      uJson, 
      uPickle,
    )
  )

lazy val numeric = defineModule("numeric")(project)
  .dependsOn(pan, refinement)
  .settings(
    libraryDependencies ++= Seq(
      iron, 
      ironCats,
      ironScalacheck % Test,
    )
  )

// "pan"-subproject types and functions
lazy val pan = defineModule("pan")(project)
  .settings(
    libraryDependencies ++= Seq(
      catsLaws % Test,
      disciplineScalatest % Test,
      iron,
      ironScalacheck % Test,
    )
  )

lazy val refinement = defineModule("refinement")(project)
  .settings(
    libraryDependencies ++= Seq(
      iron,
      ironScalacheck % Test,
    )
  )

lazy val roi = defineModule("roi")(project)
  .dependsOn(geometry, numeric, zarr)

lazy val testing = defineModule("testing", false)(project)
  .dependsOn(geometry, imaging, io, numeric)
  .settings(libraryDependencies ++= Seq(
    ironScalacheck,
    scalacheck, 
    scalatest % Test,
    scalatestScalacheck % Test,
  ))

lazy val zarr = defineModule("zarr")(project)
  .dependsOn(imaging, numeric)
  .settings(libraryDependencies ++= Seq(jzarr))

lazy val commonSettings = Def.settings(
  compileSettings, 
  metadataSettings,
)

// For subprojects that shouldn't be published (e.g., root)
lazy val noPublishSettings = Def.settings(
  publish / skip := true
)

lazy val compileSettings = Def.settings(
  scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", 
      "utf8",
      "-feature",
      "-language:existentials",
      "-explain",
      // https://contributors.scala-lang.org/t/for-comprehension-requires-withfilter-to-destructure-tuples/5953
      "-source:future", // for tuples in for comprehension; see above link
      "-unchecked",
      "-new-syntax",
      "-rewrite",
      // for scalafix RemoveUnused: https://scalacenter.github.io/scalafix/docs/rules/RemoveUnused.html
      "-Wunused:all", 
      // These unused symbol warnings are expected in the tests related to availability of typeclass instances.
      "-Wconf:msg=unused import&src=./modules/testing/src/test/scala/TestInstanceAvailability.scala:silent",
      "-Werror",
    ),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,
)

lazy val versionNumber = "0.6.0-SNAPSHOT"

lazy val metadataSettings = Def.settings(
  name := projectName,
  description := "Gerlich lab programming utilities, especially for data from imaging or sequencing", 
  version := versionNumber,
  isSnapshot := versionNumber.endsWith("SNAPSHOT"), // Allow publish overwrites: https://github.com/sbt/sbt/issues/1156#issuecomment-43512022
  organization := groupId, 
  organizationName := "Gerlich Group, IMBA, ÖAW",
  homepage := Some(url(s"https://github.com/$gitHubOwner/$projectName")),
  startYear := Some(2023),
  licenses := Seq("Apache2" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(ScmInfo(homepage.value.get, s"scm:git:$gitPubUrl", None)),
  developers := List(
    Developer(
      id = "vreuter",
      name = "Vince Reuter",
      email = "",
      url("https://github.com/vreuter")
    )
  )
)

lazy val testDependencies = Seq(
  scalacheck, 
  scalactic, 
  scalatest, 
  scalatestScalacheck,
)

// The subprojects/modules share similar structure, so DRY.
def defineModule(name: String, addTestDeps: Boolean = true): Project => Project =
  _.in(file(s"modules/$name"))
    .settings(commonSettings)
    .settings(moduleName := s"$projectName-$name")
    .settings(
      libraryDependencies ++= Seq(
        catsCore,
        kittens,
        mouse,
      ) ++ (
        if (addTestDeps) testDependencies.map(_ % Test) else Seq()
      ),
    )
