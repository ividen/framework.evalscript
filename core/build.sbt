organization := "com.ividen"

name := "framework-evalscript"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value
  , "org.scala-lang" % "scala-reflect" % scalaVersion.value
  , "org.scala-lang" % "scalap" % scalaVersion.value
  ,"org.apache.commons" % "commons-lang3" % "3.0"
)
