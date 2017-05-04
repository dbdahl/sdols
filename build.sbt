name := "austin"

version := "1.0"

scalaVersion := "2.12.2"
        
libraryDependencies ++= Seq(
  "org.ddahl" %% "rscala" % "2.0.1",
  "org.apache.commons" % "commons-math3" % "3.6.1" withSources(),
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

retrieveManaged := true

mainClass in (Compile,run) := Some("org.ddahl.austin.Main")

