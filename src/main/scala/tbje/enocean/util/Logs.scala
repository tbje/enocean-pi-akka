package tbje.enocean.util

import scala.collection.JavaConverters._
import org.slf4j.{ Logger, LoggerFactory }

object Logs {

  def listLogs(): Set[String] = {
    LoggerFactory.getILoggerFactory() match {
      case c: ch.qos.logback.classic.LoggerContext =>
        val loggers = c.getLoggerList().asScala.toSet
        loggers.flatMap {
          case l: ch.qos.logback.classic.Logger => l.iteratorForAppenders().asScala map {
            case appender: ch.qos.logback.core.FileAppender[_] =>
              val p = new java.io.File(appender.getFile()).getAbsolutePath()
              s"See extended log in $p"
          }
        }
      case _ => Set()
    }
  }

}
