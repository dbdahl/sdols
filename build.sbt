name := "sdols"

organization := "org.ddahl"

version := "1.2-SNAPSHOT"

scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.11.12", "2.12.4")
        
libraryDependencies ++= Seq(
  "org.ddahl" %% "rscala" % "2.5.0",
  "org.ddahl" %% "commonsmath" % "1.1",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

retrieveManaged := true

mainClass in (Compile,run) := Some("org.ddahl.sdols.Main")

