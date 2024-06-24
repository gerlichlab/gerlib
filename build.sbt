import Dependencies._

ThisBuild / scalaVersion     := "3.4.2"
ThisBuild / version          := "0.0.1"
ThisBuild / organization     := "com.github.gerlichlab"
ThisBuild / organizationName := "Gerlich Group, IMBA, ÖAW"

/* sbt-github-actions settings */
ThisBuild / githubWorkflowOSes := Seq("ubuntu-latest", "ubuntu-20.04", "macos-latest", "windows-latest")
ThisBuild / githubWorkflowTargetBranches := Seq("main")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()
ThisBuild / githubWorkflowJavaVersions := Seq("11", "17", "19", "21").map(JavaSpec.temurin)
// Account for the absence of sbt in newer versions of the setup-java GitHub Action.
ThisBuild / githubWorkflowBuildPreamble ++= Seq(WorkflowStep.Run(commands = List("brew install sbt"), cond = Some("contains(runner.os, 'macos')")))

lazy val root = (project in file("."))
  .settings(
    name := "gerlib",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "utf8",
      "-feature",
      "-language:existentials",
      // https://contributors.scala-lang.org/t/for-comprehension-requires-withfilter-to-destructure-tuples/5953
      "-source:future", // for tuples in for comprehension; see above link
      "-unchecked",
      "-Werror",
    ),
    libraryDependencies ++= Seq(
      catsCore,
      fs2Csv,
      fs2IO,
      mouse,
      os, 
      ) ++ 
      Seq( // only for tests
        scalacheck, 
        scalactic, 
        scalatest, 
        scalatestScalacheck
      ).map(_ % Test), 
  )
