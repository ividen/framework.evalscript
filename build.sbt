scalaVersion := "2.10.4"

import org.scoverage.coveralls.Imports.CoverallsKeys._

coverallsToken := Some("XvYxCV3pQh8z6JPSBzDAdey7WmCmqAPj0")

lazy val evalscript = project.in(file(".")).aggregate(core,benchmark)

lazy val core = project.in( file("core/"))

lazy val benchmark = project.in( file("benchmark/")).dependsOn(core)


