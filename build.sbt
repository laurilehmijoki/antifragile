name := "antifragile"

version := "1.0"

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.11.2", "2.10.4")

libraryDependencies += "org.specs2" %% "specs2" % "2.4" % "test"

libraryDependencies += "com.amazonaws" % "aws-java-sdk" % "1.7.7" % "test"