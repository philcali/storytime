import sbt._

import Keys._

object StoryBuild extends Build {
  // Right now we're the same
  def generalSettings = Defaults.defaultSettings ++ Seq (
    organization := "com.github.philcali",
    version := "0.0.1"
  ) 

  lazy val core = Project (
    "storytime-core",
    file("core"),
    settings = generalSettings ++ Seq (
      libraryDependencies <++= (scalaVersion) { sv => Seq (
        "net.databinder" %% "pamflet-knockoff" % "0.2.5",
        (sv match {
          case v if v.contains("2.8") =>
            "org.scalatest" % "scalatest" % "1.3" % "test"
          case _ =>
            "org.scalatest" %% "scalatest" % "1.6.1" % "test"
        })
      ) }
    )
  )

  lazy val app = Project (
    "storytime-app",
    file("app"),
    settings = generalSettings ++ Seq (
      libraryDependencies <++= (sbtVersion) { sv => Seq (
        "org.scala-tools.sbt" % "launcher-interface_2.8.1" % sv % "provided",
        "net.databinder" %% "dispatch-http" % "0.8.5"
      ) }
    )
  ) dependsOn core

}
