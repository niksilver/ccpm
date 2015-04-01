name := "CCPM DSL"

version := "0.1.3"

scalaVersion := "2.11.6"

scalacOptions ++=
	Seq("-deprecation", "-feature",
		"-language:implicitConversions")

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
)

// For Scalatest for Scala 2.11
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

// For ScalaMock
libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"

