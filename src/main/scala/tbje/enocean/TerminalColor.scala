package tbje.enocean.util

object TerminalColor {
  implicit class ColorCtxt(val sc: StringContext) extends AnyVal {
    private def anyColor(color: String, args: Any*): String = {
      val str  = sc.standardInterpolator(x=>x, args)
      color + str + Console.RESET
    }

    def yellow(args: Any*) = anyColor(Console.YELLOW, args: _*)
    def blue(args: Any*) = anyColor(Console.BLUE, args: _*)
    def red(args: Any*) = anyColor(Console.RED, args: _*)
    def cyan(args: Any*) = anyColor(Console.CYAN, args: _*)
    def magenta(args: Any*) = anyColor(Console.MAGENTA, args: _*)
    def green(args: Any*) = anyColor(Console.GREEN, args: _*)
    def white(args: Any*) = anyColor(Console.WHITE, args: _*)
  }
}
