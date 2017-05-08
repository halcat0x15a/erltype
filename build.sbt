antlr4Settings

scalaVersion := "2.11.8"

antlr4PackageName in Antlr4 := Some("erltype")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

scalacOptions ++= Seq("-language:higherKinds", "-feature")
