name := """framework-evalscript-benchmark"""

version := "1.0-SNAPSHOT"

enablePlugins(JmhPlugin)

libraryDependencies ++= Seq(
   "org.freemarker" % "freemarker" % "2.3.22",
   "org.apache.velocity" % "velocity" %"1.7",
   "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.9.1",
   "jacl" %% "jacl" %  "1.4.1",
   "jacl" %% "tcljava" % "1.4.1"
)



