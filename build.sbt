scalaVersion := "2.10.4"

lazy val evalscript = project.in(file(".")).aggregate(core,benchmark)

lazy val core = project.in( file("core/"))

lazy val benchmark = project.in( file("benchmark/")).dependsOn(core)


