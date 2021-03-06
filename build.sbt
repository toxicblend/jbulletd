name := "jbulletd"

version := "0.1"

exportJars := true

scalaVersion := "2.11.4"

javacOptions ++= Seq("-Xlint:unchecked")
 
scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "java3d" % "vecmath" % "1.5.2"
)

resolvers += "Geotoolkit.org" at "http://maven.geotoolkit.org"

// disable javadoc - it fails

sources in doc in Compile := List() 

publishArtifact in packageDoc := false
 
publishMavenStyle := true

publishArtifact in Test := false

publishTo := Some(Resolver.file("file",  baseDirectory.value / "dist" ) )
