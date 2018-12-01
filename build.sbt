name := "sdols"

organization := "org.ddahl"

//version := "1.8"
version := "1.8-SNAPSHOT"

scalaVersion := "2.12.7"

crossScalaVersions := Seq("2.11.12", "2.12.7")

libraryDependencies ++= Seq(
//  "org.ddahl" %% "commonsmath" % "1.3-SNAPSHOT",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

retrieveManaged := true

mainClass in (Compile,run) := Some("org.ddahl.sdols.Main")

