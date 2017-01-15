name := "visualz"
organization := "com.jsuereth"

scalaVersion := "2.12.0"


libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.6" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

