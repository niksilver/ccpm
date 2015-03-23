name := "CCPM DSL"

version := "0.1.1"

scalaVersion := "2.11.6"

scalacOptions ++=
	Seq("-deprecation", "-feature",
		"-language:implicitConversions")

// unmanagedJars in Compile <++= baseDirectory map { base =>
	// val jarDirs =
		// (base / "lib" / "build") +++
		// (base / "lib" / "runtime")
	// val jars = jarDirs ** "*.jar"
	// jars.classpath
// }

libraryDependencies ++= Seq(
)

// For Scalatest for Scala 2.11
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

// For ScalaMock
libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"

