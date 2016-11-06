name := "enocean-pi-akka"

organization := "tbje"

scalaVersion := Version.scala

offline := true // Use when on the move...

libraryDependencies ++= Dependencies.all

//initialCommands in Console := """|import tbje.util._
//                                 |import com.github.nscala_time.time.Imports._""".stripMargin
incOptions := incOptions.value.withNameHashing(true)

initialCommands in (Test, console) := """ammonite.Main().run()"""

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-language:_",
  "-encoding", "UTF-8"
)
