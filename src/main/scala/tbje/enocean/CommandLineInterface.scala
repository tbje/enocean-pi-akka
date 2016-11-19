package tbje.enocean

import scala.util.parsing.combinator.RegexParsers

trait CommandLineInterface {

  protected sealed trait Command

  protected object Command {

    case object Quit extends Command

    case object Help extends Command

    case object List extends Command

    case class Set(room: Int, percent: Int) extends Command

    case class Unknown(command: String) extends Command

    def apply(command: String): Command =
      CmdParser.parseAsCommand(command)
  }

  private object CmdParser extends RegexParsers {

    def parseAsCommand(s: String): Command =
      parseAll(parser, s) match {
        case Success(command, _) => command
        case _                   => Command.Unknown(s)
      }

    def quit: Parser[Command.Quit.type] =
      "quit|q".r ^^ (_ => Command.Quit)

    def list: Parser[Command.List.type] =
      "list|l".r ^^ (_ => Command.List)

    def set: Parser[Command.Set] =
      "set|s".r ~> int ~ int ^^ {
        case room ~ pct => Command.Set(room, pct)
      }

    def help: Parser[Command.Help.type] =
      """help|h|\?""".r ^^ (_ => Command.Help)

    def int: Parser[Int] =
      """\d+""".r ^^ (_.toInt)
  }

  private val parser: CmdParser.Parser[Command] =
    CmdParser.quit | CmdParser.help | CmdParser.list | CmdParser.set
}
