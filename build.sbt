name := "jbulletd"

version := "0.1"

scalaVersion := "2.10.3"

javacOptions ++= Seq("-Xlint:unchecked")
 
scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "java3d" % "vecmath" % "1.3.1"
