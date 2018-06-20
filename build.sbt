name := "sdols"

organization := "org.ddahl"

//version := "1.5"
version := "1.5-SNAPSHOT"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.ddahl" %% "commonsmath" % "1.2-SNAPSHOT",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

retrieveManaged := true

mainClass in (Compile,run) := Some("org.ddahl.sdols.Main")

