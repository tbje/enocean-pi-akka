package tbje.enocean.util

import akka.util.{ CompactByteString => CBS }
import org.scalatest.{ Matchers, WordSpec }

class Crc8Spec extends WordSpec with Matchers {
  val b = CBS(
    0x55,
    0x00, 0x0a, 0x07, 0x01, 0xeb,
    0xa5, 0x32, 0x20, 0x93, 0x08, 0xff, 0xe1, 0x49, 0x80, 0x00,
    0x01, 0xff, 0xff, 0xff, 0xff, 0x31, 0x00,
    0xb0
  )
  val headers = b.tail.take(4)
  val headerCrc = b(5)
  val data = b.drop(6).init
  val dataCrc = b.last

  "Running CRC8 on the following" should {
    "yield true for headers part" in {
      Crc8(headers) shouldEqual headerCrc
    }
    "yield true for data part" in {
      Crc8(data) shouldEqual dataCrc
    }
    "yield true for 0 only 0s" in {
      Crc8(CBS(0x00, 0x00, 0x00, 0x00)) shouldEqual 0x00.toByte
    }
  }
}
