name := "SI"

version := "0.1"

scalaVersion := "2.13.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8", "-Ymacro-annotations")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" %% "scalafx" % "15.0.1-R21"
libraryDependencies += "org.scalafx" %% "scalafxml-core-sfx8" % "0.5"


// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

// Add dependency on JavaFX libraries, OS dependent
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map(m =>
  "org.openjfx" % s"javafx-$m" % "15.0.1" classifier osName
)
