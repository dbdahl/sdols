name := "sdols"

organization := "org.ddahl"

version := "1.1-SNAPSHOT"

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.4")
        
libraryDependencies ++= Seq(
  "org.ddahl" %% "rscala" % "2.4.0",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

retrieveManaged := true

mainClass in (Compile,run) := Some("org.ddahl.sdols.Main")

