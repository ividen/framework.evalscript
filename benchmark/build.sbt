name := """framework-evalscript-benchmark"""

version := "1.0-SNAPSHOT"

enablePlugins(JmhPlugin)

unmanagedBase := baseDirectory.value / "unmanaged_libs"

libraryDependencies ++= Seq(
   "org.freemarker" % "freemarker" % "2.3.22"
   ,"org.apache.velocity" % "velocity" %"1.7"
   ,"org.openjdk.jmh" % "jmh-generator-annprocess" % "1.9.1"
)



