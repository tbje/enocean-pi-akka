package tbje.enocean

import akka.util.{ CompactByteString => CBS }
import org.scalatest.{ Matchers, WordSpec }

class MessageOrganiserSpec extends WordSpec with Matchers {

  import TestData._
  "Calling checkData" should {
    "return Complete for message with exactly amount of data" in {
      MessageOrganiser.checkData(b0) shouldEqual MessageOrganiser.Complete(b0, 10, 7)
    }
    "return Complete for message with garbage in front" in {
      MessageOrganiser.checkData(CBS(0x88, 0x22) ++ b0) shouldEqual MessageOrganiser.Complete(b0, 10, 7)
    }
    "return TooLong for message with exactly amount of data" in {
      MessageOrganiser.checkData(b0 ++ CBS(0x55)) shouldEqual MessageOrganiser.TooLong(b0, 10, 7, CBS(0x55))
    }
    "return Incomlete for message with less then enough info" in {
      val data = CBS(0x55, 0x00, 0x05)
      MessageOrganiser.checkData(data) shouldEqual MessageOrganiser.Incomplete(data)
    }
  }
}
