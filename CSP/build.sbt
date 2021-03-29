name := "CSP"

version := "0.1"

scalaVersion := "2.13.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8", "-Ymacro-annotations")

lazy val ScalaTestVersion = "3.2.5"

libraryDependencies += "org.scalatest" %% "scalatest" % ScalaTestVersion % Test
libraryDependencies += "org.scalatest" %% "scalatest-wordspec" % ScalaTestVersion % Test
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.1"

val quicklens = "com.softwaremill.quicklens" %% "quicklens" % "1.6.1"
libraryDependencies += quicklens