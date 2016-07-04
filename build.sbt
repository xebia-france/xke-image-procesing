name := "xke-image-processing"

organization := "fr.xebia.scala"

version := "0.1"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
  "org.spire-math" %% "cats" % "0.3.0"
)

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits")

testOptions in Test += Tests.Argument("-oD")

fork in Runtime := true
