import sbt._

object Version {
  val flow             = "3.0.3"
  val akka             = "2.4.11"
  val akkaHttp         = "10.0.0"
  val logback          = "1.1.3"
  val nScalaTime       = "2.0.0"
  val scala            = "2.11.8"
  val scalaTest        = "2.2.2"
  val ammonite         = "0.7.8"
  val scalaParsers     = "1.0.4"
}

object Library {
  val flow             = "ch.jodersky"                   %% "flow-core"                 % Version.flow
  val scalaParsers     = "org.scala-lang.modules"        %% "scala-parser-combinators"  % Version.scalaParsers
  val ammonite         = "com.lihaoyi"                   %  "ammonite"                  % Version.ammonite % "test" cross CrossVersion.full
  val akkaActor        = "com.typesafe.akka"             %% "akka-actor"                % Version.akka
  val akkaSlf4j        = "com.typesafe.akka"             %% "akka-slf4j"                % Version.akka
  val akkaHttp         = "com.typesafe.akka"             %% "akka-http"                 % Version.akkaHttp
  val akkaTestkit      = "com.typesafe.akka"             %% "akka-testkit"              % Version.akka
  val logbackClassic   = "ch.qos.logback"                %  "logback-classic"           % Version.logback
  val nScalaTime       = "com.github.nscala-time"        %% "nscala-time"               % Version.nScalaTime
  val scalaTest        = "org.scalatest"                 %% "scalatest"                 % Version.scalaTest
}

object Dependencies {

  import Library._

  private[this] val test = List(
    scalaTest
  ).map(_ % "test")

  private[this] val main = (
    flow ::
    scalaParsers ::
    akkaActor ::
    ammonite ::
    nScalaTime ::
    akkaSlf4j ::
    akkaHttp ::
    logbackClassic ::
    Nil
  )

  val all = main ++ test
}
