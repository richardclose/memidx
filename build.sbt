name := "memidx"

version := "1.0.4-SNAPSHOT"

organization := "org.phasanix"

scalaVersion := "2.13.5"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.scala-lang"    %  "scala-compiler"  % scalaVersion.value,
  "org.scalatest"     %% "scalatest"       % "3.2.6" % "test"
)


