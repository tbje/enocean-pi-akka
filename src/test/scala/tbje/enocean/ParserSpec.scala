package tbje.enocean

import akka.util.{ ByteString => BS, CompactByteString => CBS }

import org.scalatest.{ Matchers, WordSpec }
import tbje.enocean.util.DSL


class ParserSpec extends WordSpec with Matchers {
  import TestData._
  import Parser._
  "Using Parser" should {
    "parse iRTV diagrams" in {
      Parser.parse(b0, 10, 7) shouldEqual
        IRTV(sender = CBS(0xFF, 0xE1, 0x49, 0x80), valvePos = 50, eneryHarvesting = false, sufficienEnergy = true, intTemp = -17, dbm = -49, dest = CBS(0xFF,0xFF,0xFF,0xFF))
    }
    "parse iRTV learn diagrams" in {
      val l = Parser.parse(learn, 10, 7)
        l shouldBe a[Learn]
        import DSL._
        //println(0x54.bits) // 01010100
        //println(0xa0.bits) // 10100000
        //println(0x0f.bits) // 00001111

        println(Bits.int(CBS(0x54, 0xa0), 6, 7))
        val dbs = learn.slice(7, 11)
        println(Bits.int(dbs, 6, 7))
        println(l)
        l shouldEqual Learn(CBS(0xff, 0xd0, 0x50, 0x81), CBS(0xff, 0xff, 0xff, 0xff), 0xa5, bit"00010101", bit"00010100" , 15, 1, 1)
    }
    "parse RES_OK" in {
      Parser.parse(resOk, 1, 0) shouldEqual Response(0x00, "RET_OK")
    }
  }
}
