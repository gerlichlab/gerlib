import sbt.*

/** Dependencies for the project */
object Dependencies {
    /** Bundle data related to getting ModuleID for an iron subproject. */
    object Iron {
        def moduleId = getModuleID(None)
        def getModuleID(name: String): ModuleID = getModuleID(Some(name))
        private def getModuleID(name: Option[String]): ModuleID = 
            "io.github.iltotore" %% ("iron" ++ name.fold("")("-" ++ _)) % "2.6.0" 
    }

    /** Build ModuleID for a com.lihaoyi JSON-related project. */
    object HaoyiJson {
        def getModuleId(name: String): ModuleID = "com.lihaoyi" %% name % latestVersion
        private def latestVersion = "4.0.0"
    }

    object Cats {
        def getModuleId(name: String): ModuleID = 
            "org.typelevel" %% s"cats-$name" % "2.12.0"
    }

    /* Versions */
    lazy val scalatestVersion = "3.2.19"
    
    /* Core dependencies */
    lazy val catsCore = Cats.getModuleId("core")
    lazy val kittens = "org.typelevel" %% "kittens" % "3.3.0"
    lazy val mouse = "org.typelevel" %% "mouse" % "1.3.2"
    lazy val uJson = HaoyiJson.getModuleId("ujson")
    lazy val uPickle = HaoyiJson.getModuleId("upickle")
    
    /* numeric dependencies */
    lazy val iron = Iron.moduleId
    lazy val ironCats = Iron.getModuleID("cats")
    lazy val ironScalacheck = Iron.getModuleID("scalacheck")

    /* IO dependencies */
    lazy val fs2Csv = "org.gnieh" %% "fs2-data-csv" % "1.11.1"
    lazy val fs2IO = "co.fs2" %% "fs2-io" % "3.10.2"
    lazy val os = "com.lihaoyi" %% "os-lib" % "0.10.3"

    /** ZARR dependencies */
    lazy val jzarr = "dev.zarr" % "jzarr" % "0.4.2"

    /* Test dependencies */
    lazy val catsLaws = Cats.getModuleId("laws")
    lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.18.0"
    lazy val scalactic = "org.scalactic" %% "scalactic" % scalatestVersion
    lazy val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion
    lazy val scalatestScalacheck = "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0"

}
