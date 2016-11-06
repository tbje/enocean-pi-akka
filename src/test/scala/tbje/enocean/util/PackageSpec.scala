package tbje.enocean.util

import akka.util.{ CompactByteString => CBS }
import org.scalatest.{ Matchers, WordSpec }


class PackageSpec extends WordSpec with Matchers {
  val b = CBS(
    0x55,
    0x00, 0x0a, 0x07, 0x01, 0xeb,
    0xa5, 0x32, 0x20, 0x93, 0x08, 0xff, 0xe1, 0x49, 0x80, 0x00,
    0x01, 0xff, 0xff, 0xff, 0xff, 0x31, 0x00,
    0xb0
  )
  "Running to Int" should {
    "yield true for headers part" in {
      b match {
        case _ +: _ +: fst +: snd +: _ =>
          toInt(fst +: snd +: Nil) shouldEqual 12
      }
    }
  }
}
