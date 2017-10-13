name := "austin"

organization := "org.ddahl"

version := "1.0"

scalaVersion := "2.12.3"

crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.3")
        
libraryDependencies ++= Seq(
  "org.ddahl" %% "rscala" % "2.4.0",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

retrieveManaged := true

mainClass in (Compile,run) := Some("org.ddahl.austin.Main")

