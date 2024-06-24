import sbt._

/** Dependencies for the project */
object Dependencies {
    /* Versions */
    lazy val scalatestVersion = "3.2.18"
    lazy val fs2CsvVersion = "1.11.0"
    
    /* Core libraries */
    lazy val catsCore = "org.typelevel" %% "cats-core" % "2.12.0"
    lazy val fs2Csv = "org.gnieh" %% "fs2-data-csv" % fs2CsvVersion
    lazy val fs2IO = "co.fs2" %% "fs2-io" % "3.10.2"
    lazy val mouse = "org.typelevel" %% "mouse" % "1.3.0"
    lazy val os = "com.lihaoyi" %% "os-lib" % "0.10.2"

    /* Test dependencies */
    lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.18.0"
    lazy val scalactic = "org.scalactic" %% "scalactic" % scalatestVersion
    lazy val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
    lazy val scalatestScalacheck = "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0"

}
