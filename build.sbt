name := "eclat"

version := "0.1"

scalaVersion := "2.13.4"

idePackagePrefix := Some("com.mkobiers.med")

Compile / mainClass := Some("com.mkobiers.med.Main")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

scalacOptions ++= Seq("-deprecation", "-feature")
