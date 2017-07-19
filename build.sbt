name := "memidx"

version := "1.0.3-SNAPSHOT"

organization := "org.phasanix"

crossScalaVersions := Seq ("2.11.11", "2.12.2")

scalaVersion := "2.12.2"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.scala-lang"    %  "scala-compiler"  % scalaVersion.value,
  "org.scalatest"     %% "scalatest"       % "3.2.0-SNAP5" % "test"
)


