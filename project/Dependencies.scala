import sbt.*

/** Dependencies for the project */
object Dependencies {
    /** Bundle data related to getting ModuleID for an iron subproject. */
    object Iron {
        def moduleId = getModuleID(None)
        def getModuleID(name: String): ModuleID = getModuleID(Some(name))
        private def getModuleID(name: Option[String]): ModuleID = 
            "io.github.iltotore" %% ("iron" ++ name.fold("")("-" ++ _)) % "3.4.1" 
    }

    /** Build ModuleID for a com.lihaoyi JSON-related project. */
    object HaoyiJson {
        def getModuleId(name: String): ModuleID = "com.lihaoyi" %% name % latestVersion
        private def latestVersion = "4.4.3"
    }

    object Cats {
        def getModuleId(name: String): ModuleID = 
            "org.typelevel" %% s"cats-$name" % "2.13.0"
    }

    object PureConfig {
        def getModuleId(name: String): ModuleID = "com.github.pureconfig" %% s"pureconfig-$name" % "0.17.11"
    }

    /* Versions */
    lazy val scalatestVersion = "3.2.20"
    
    /* config dependencies */
    lazy val pureconfigCore = PureConfig.getModuleId("core")
    lazy val pureconfigGeneric = PureConfig.getModuleId("generic-scala3")

    /* core dependencies */
    lazy val catsCore = Cats.getModuleId("core")
    lazy val kittens = "org.typelevel" %% "kittens" % "3.5.0"
    lazy val mouse = "org.typelevel" %% "mouse" % "1.4.0"
    lazy val uJson = HaoyiJson.getModuleId("ujson")
    lazy val uPickle = HaoyiJson.getModuleId("upickle")
    
    /* geometry dependencies */
    lazy val squants = "org.typelevel" %% "squants" % "1.8.3"

    /* graph dependencies */
    lazy val scalaGraphCore = "org.scala-graph" %% "graph-core" % "2.0.3"

    /* numeric dependencies */
    lazy val iron = Iron.moduleId
    lazy val ironCats = Iron.getModuleID("cats")
    lazy val ironScalacheck = Iron.getModuleID("scalacheck")

    /* IO dependencies */
    lazy val fs2Csv = "org.gnieh" %% "fs2-data-csv" % "1.14.1"
    lazy val fs2IO = "co.fs2" %% "fs2-io" % "3.14.0"
    lazy val os = "com.lihaoyi" %% "os-lib" % "0.11.10"

    /** ZARR dependencies */
    // Held at 0.4.2: 0.5.0 is a minor release of a 0.x version, so it may change
    // behaviour, and no test covers the code that reads through it.
    lazy val jzarr = "dev.zarr" % "jzarr" % "0.4.2"

    /* Test dependencies */
    lazy val catsLaws = Cats.getModuleId("laws")
    lazy val disciplineScalatest = "org.typelevel" %% "discipline-scalatest" % "2.3.0"
    lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.20.0"
    lazy val scalactic = "org.scalactic" %% "scalactic" % scalatestVersion
    lazy val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
    lazy val scalatestScalacheck = "org.scalatestplus" %% "scalacheck-1-20" % "3.2.20.0"

}
