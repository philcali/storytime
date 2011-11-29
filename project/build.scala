import sbt._

import Keys._

object StoryBuild extends Build {
  // Right now we're the same
  def generalSettings = Defaults.defaultSettings ++ Seq (
    organization := "com.github.philcali",
    scalacOptions += "-unchecked",
    version := "0.0.1"
  ) 

  lazy val core = Project (
    "storytime-core",
    file("core"),
    settings = generalSettings ++ Seq (
      crossScalaVersions := Seq("2.8.1", "2.9.1"),

      libraryDependencies <++= (scalaVersion) { sv => Seq (
        "net.databinder" %% "pamflet-knockoff" % "0.3.0",
        "commons-codec" % "commons-codec" % "1.5",
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
        "org.scala-tools.sbt" %% "launcher-interface" % sv % "provided",
        "org.clapper" %% "argot" % "0.3.5",
        "net.databinder" %% "dispatch-http" % "0.8.5"
      ) }
    )
  ) dependsOn core

}
