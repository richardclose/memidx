name := "memidx"

version := "1.0.4-SNAPSHOT"

organization := "org.phasanix"

// crossScalaVersions := Seq ("2.11.11", "2.12.3")

scalaVersion := "2.13.3"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.scala-lang"    %  "scala-compiler"  % scalaVersion.value,
  "org.scalatest"     %% "scalatest"       % "3.2.3" % "test"
)


