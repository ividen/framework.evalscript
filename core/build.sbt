organization := "com.ividen"

name := "framework-evalscript"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value
  , "org.scala-lang" % "scala-reflect" % scalaVersion.value
  , "org.scala-lang" % "scalap" % scalaVersion.value
  , "org.slf4j" % "slf4j-api" % "1.7.7"
  , "org.slf4j" % "jul-to-slf4j" % "1.7.7"
  , "org.apache.commons" % "commons-lang3" % "3.0"
  , "org.ow2.asm" % "asm" % "4.0"
  , "org.ow2.asm" % "asm-util" % "4.0"
  , "org.ow2.asm" % "asm-commons" % "4.0"
  , "commons-codec" % "commons-codec" % "1.10"
)
