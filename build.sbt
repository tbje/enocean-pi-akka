name := "enocean-pi-akka"

organization := "tbje"

scalaVersion := Version.scala

offline := true // Use when on the move...

libraryDependencies ++= Dependencies.all

initialCommands in console := """|import tbje.enocean.util._
                                 |import tbje.enocean.util.DSL._
                                 |import akka.util.{ ByteString => BS, CompactByteString => CBS }""".stripMargin

incOptions := incOptions.value.withNameHashing(true)

initialCommands in (Test, console) := """ammonite.Main().run()"""

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-language:_",
  "-encoding", "UTF-8"
)
