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
      libraryDependencies += "net.databinder" %% "pamflet-knockoff" % "0.2.5"
    )
  )

  lazy val app = Project (
    "storytime-app",
    file("app"),
    settings = generalSettings ++ Seq (
      libraryDependencies <+= (sbtVersion) {
        "org.scala-tools.sbt" % "launcher-interface_2.8.1" % _ % "provided"
      }
    )
  ) dependsOn core

}
