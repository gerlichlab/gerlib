import Dependencies.*

/* Core settings */
ThisBuild / scalaVersion := "3.5.0"
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

lazy val root = project
  .in(file("."))
  .aggregate(cell, geometry, imaging, io, json, numeric, pan, roi, testing, zarr)
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
  .dependsOn(numeric)

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
  .dependsOn(json, numeric)
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
  .dependsOn(pan)
  .settings(
    libraryDependencies ++= Seq(
      iron, 
      ironCats,
      ironScalacheck % Test,
    )
  )

// "pan"-subproject types and functions
lazy val pan = defineModule("pan")(project)

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
      "-Wunused:all", // for scalafix
    ),
  Compile / console / scalacOptions -= "-Ywarn-unused:imports",
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,
)

lazy val metadataSettings = Def.settings(
  name := projectName,
  description := "Gerlich lab programming utilities, especially for data from imaging or sequencing", 
  version := "0.2-SNAPSHOT",
  organization := groupId, 
  organizationName := "Gerlich Group, IMBA, ÖAW",
  homepage := Some(url(s"https://github.com/$gitHubOwner/$projectName")),
  startYear := Some(2015),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
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
