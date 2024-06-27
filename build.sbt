import Dependencies.*

/* Core settings */
val groupId = "com.github.gerlichlab"
val projectName = "gerlib"
val rootPkg = s"at.ac.oeaw.imba.gerlich.$projectName"
val gitHubOwner = "gerlichlab"
val gitPubUrl = s"https://github.com/$gitHubOwner/$projectName.git"

// Needed for ZARR (jzarr) (?)
ThisBuild / resolvers += "Unidata UCAR" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases/"

/* sbt-github-actions settings */
ThisBuild / githubWorkflowOSes := Seq("ubuntu-latest", "ubuntu-20.04", "macos-latest")
ThisBuild / githubWorkflowTargetBranches := Seq("main")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()
ThisBuild / githubWorkflowJavaVersions := Seq("11", "17", "19", "21").map(JavaSpec.temurin)
// Account for the absence of sbt in newer versions of the setup-java GitHub Action.
ThisBuild / githubWorkflowBuildPreamble ++= Seq(WorkflowStep.Run(commands = List("brew install sbt"), cond = Some("contains(runner.os, 'macos')")))

lazy val root = project
  .in(file("."))
  .aggregate(cell, geometry, imaging, io, numeric, syntax, testing, zarr)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(
    name := "gerlib",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion), 
    buildInfoPackage := s"$rootPkg.internal"
  )

lazy val cell = defineModule("cell")(project)
  .dependsOn(numeric)

lazy val geometry = defineModule("geometry")(project)
  .dependsOn(testing % Test)

lazy val io = defineModule("io")(project)
  .dependsOn(geometry, syntax)
  .settings(
    libraryDependencies ++= Seq(
      fs2Csv, 
      fs2IO, 
      os,
    )
  )

lazy val imaging = defineModule("imaging")(project)
  .dependsOn(numeric)

lazy val numeric = defineModule("numeric")(project)

lazy val syntax = defineModule("syntax")(project)

lazy val testing = defineModule("testing", false)(project)
  .dependsOn(imaging, numeric)
  .settings(libraryDependencies ++= testDependencies)

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
      // https://contributors.scala-lang.org/t/for-comprehension-requires-withfilter-to-destructure-tuples/5953
      "-source:future", // for tuples in for comprehension; see above link
      "-unchecked",
      "-Werror",
    ),
  Compile / console / scalacOptions -= "-Ywarn-unused:imports",
  Test / console / scalacOptions := (Compile / console / scalacOptions).value,
)

lazy val metadataSettings = Def.settings(
  name := projectName,
  description := "Gerlich lab programming utilities, especially for data from imaging or sequencing", 
  version := "0.1.0",
  scalaVersion := "3.4.2",
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
