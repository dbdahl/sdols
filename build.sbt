name := "sdols"

organization := "org.ddahl"

version := "1.7.3.4-SNAPSHOT"
//version := "1.7.3.4"

scalaVersion := "2.12.8"

crossScalaVersions := Seq("2.11.12", "2.12.8")

mainClass in (Compile,run) := Some("org.ddahl.sdols.Main")

libraryDependencies ++= Seq(
  "org.ddahl" %% "commonsmath" % "1.2.2.4",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

resolvers += Resolver.bintrayRepo("dahl", "maven")

//publishTo := sonatypePublishTo.value

licenses := List(("Apache-2.0",url("https://www.apache.org/licenses/LICENSE-2.0")))

publishMavenStyle := true

pomExtra := (
  <url>https://github.com/dbdahl/sdols/</url>
  <licenses>
    <license>
      <name>Apache License 2.0</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/dbdahl/sdols/</url>
    <connection>scm:git:https://github.com/dbdahl/sdols.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dbdahl</id>
      <name>David B. Dahl</name>
      <url>https://dahl.byu.edu</url>
    </developer>
  </developers>
)

