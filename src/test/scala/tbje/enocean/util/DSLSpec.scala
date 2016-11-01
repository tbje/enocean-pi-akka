package tbje.enocean.util

import akka.util.{ ByteString => BS, CompactByteString => CBS }
import org.scalatest.{ Matchers, WordSpec }
import tbje.enocean.util.DSL._

class DSLSpec extends WordSpec with Matchers {

  "Running CRC8 on the following" should {
    "yield true for headers part" in {
      bit"10001000" shouldEqual -120
    }
  }
}
