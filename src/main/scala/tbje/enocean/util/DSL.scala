package tbje.enocean.util

import akka.util.{ ByteString => BS }

object DSL {

  implicit class BitCtxt(val sc: StringContext) extends AnyVal {
    def bit(args: Any*): Byte = {
      val str  = sc.parts.mkString("")
      require(str.length() == 8)
      Integer.parseInt(str, 2).toByte
    }
  }

  implicit class ByteStringOps(val a: BS) extends AnyVal {
    def hex: String = "0x" + a.map(x => f"${x.toByte}%02x").mkString(",")
    def hex2: String = a.map(x => f"${x.toByte}%02x").mkString(":").toUpperCase()
    def bits = "|" + a.foldLeft("")((x,y) => x + "76543210|") + a.foldLeft("\n|")(_ + displayBits(_) + "|")
  }

  def displayBits(x: Byte)  =
    (1 to 8).map(bit => x >> (8 - bit) & 1).mkString("")

  implicit class ByteOps(val x:Byte) extends AnyVal {
    def hex: String = f"0x$x%02x"
    def bits = "76543210\n" + displayBits(x)
  }


  implicit class IntOps(val x: Int) extends AnyVal {
    def hex: String = f"0x$x%02x"
    def bits = "76543210\n" + displayBits(x.toByte)
  }

}
