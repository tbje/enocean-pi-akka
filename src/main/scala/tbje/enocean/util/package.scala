package tbje.enocean

import akka.util.{ ByteString => BS, CompactByteString => CBS }


package object util {


  def fromInt(x: Int): BS =
    CBS(x >> 8, x.toByte.toInt)

  def toInt(x: BS): Int = x match {
    case fst +: snd +: tail =>
      fst << 8 + snd
    case fst +: _ => fst.toInt
  }

}
