name := "storytime"

organization := "com.github.philcali"

version := "0.0.1"

scalacOptions += "-unchecked"

libraryDependencies ++= Seq (
  "net.databinder" %% "pamflet-knockoff" % "0.2.5"
)
