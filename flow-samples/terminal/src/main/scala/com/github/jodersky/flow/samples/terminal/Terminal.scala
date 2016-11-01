package com.github.jodersky.flow
package samples.terminal

import akka.actor.{ Actor, ActorLogging, ActorRef, Props, Terminated, actorRef2Scala }
import akka.io.IO
import akka.util.{ ByteString, CompactByteString }

class Terminal(port: String, settings: SerialSettings) extends Actor with ActorLogging {
  import Terminal._
  import context._

  val reader = actorOf(Props[ConsoleReader])

  var storage = ByteString.empty
        import TerminalOps._

  log.info(s"Requesting manager to open port: ${port}, baud: ${settings.baud}")
  IO(Serial) ! Serial.Open(port, settings)

  override def postStop() = {
    system.shutdown()
  }

  def receive = {
    case Serial.CommandFailed(cmd, reason) => {
      log.error(s"Connection failed, stopping terminal. Reason: ${reason}")
      context stop self
    }
    case Serial.Opened(port) => {
      log.info(s"Port ${port} is now open.")
      val operator = sender
      context become opened(operator)
      context watch operator

      def createPacket(command: Int, data: ByteString, opt: ByteString = CompactByteString()): ByteString = {
      	  val header = fromInt(data.length) ++ CompactByteString(opt.length, command)
    	  val headerCrc = Crc8(header)
    	  val body = data ++ opt
    	  val bodyCrc = Crc8(body)
    	  CompactByteString(0x55) ++ header ++ CompactByteString(headerCrc) ++ body ++ CompactByteString(bodyCrc)
      }
      val newData = createPacket(0x05, CompactByteString(0x08))
      log.info(newData.hex)
      operator ! Serial.Write(newData, length => Wrote(newData take length))
      reader ! ConsoleReader.Read
    }
  }

  val baseId = CompactByteString(0xff, 0xbe, 0x2f, 0x80)

  private[this] def getBit(db: Int, bit: Int, data: Seq[Byte]) =
    (data(data.length - db - 1) >> bit) & 1

  private[this] def getDb(db: Int, data: Seq[Byte]) =
    (data(data.length - db - 1))

  private[this] def fromInt(x: Int): Seq[Byte] =
    CompactByteString(x >> 8, x.toByte.toInt)

  def opened(operator: ActorRef): Receive = {

    case Serial.Received(data) => {
      log.info(s"Received data: ${formatData(data)}")
      val b = storage ++ data
      log.info(s"Handling data: ${formatData(b)}")
      val initLen = 6
      if (b.length < initLen) {
        log.info(s"Too small")
        storage = b
      } else {
      val dataLen = b(1) << 8 | b(2)
      val optLen = b(3)
      val crcLen = 1
      val totalLen = Seq(initLen, dataLen, optLen, crcLen).sum
      if (b.length < totalLen) {
        storage = b
      } else if (b.length > totalLen) {
        log.error("Too long Resetting")
        storage = ByteString.empty
      } else {
        val (init, allData) = b splitAt initLen
        //init.hex
        //data.hex
        val packType = b(4).hex
        val headerCrc = b(5)
        val (data, rest) =  allData splitAt dataLen
        val opt = rest.init
        val dataCrc = b(initLen + dataLen + optLen) // allData.last
        val calculatedHeaderCrc = Crc8(init.drop(1).init)
        require(calculatedHeaderCrc == headerCrc, f"Header CRC failed, expected 0x$headerCrc%02x was 0x$calculatedHeaderCrc%02x")
        val calculatedDataCrc = Crc8(allData.init)
        require(calculatedDataCrc == dataCrc, f"Data CRC failed, expected 0x$dataCrc%02x was 0x$calculatedDataCrc%02x")

        val rorg = data(0).hex
        val dest = opt.drop(1).take(4).hex
        val senderBytes = data.reverse.drop(1).take(4).reverse
        if (opt.size >= 5) {
        val dbm = -opt(5)
        val dbs = data.take(5)
        val learn = getBit(0,3, dbs) // DB0.bit_3

        val intTemp = 40 * (getDb(1,dbs) & 0xff) / 255 // DB0.bit_7
        val valvePos = getDb(3, dbs) & 0xff
        val energyHarvested = getBit(2, 5, dbs)
        val energyHarvesting = getBit(2, 6, dbs)
        val sender = s"${Terminal.mappedDevices.getOrElse(senderBytes, ("Unknown"->0))._1}(${senderBytes.hex})"
        log.info(s" $sender -> $dest $rorg - $dbm dBM - Learn: $learn - Temp: $intTemp - Valvepos: $valvePos - Energy $energyHarvested $energyHarvesting")
        storage = ByteString.empty

        def createReply(dest: ByteString, newPos: Int, teach: Boolean = false)(implicit sender: ByteString): ByteString = {
          require(sender.size == 4, s"sender size should be 4 was ${sender.hex}")
          require(dest.size == 4, s"dest size should be 4 was ${dest.hex}")
          val packType = 0x01
          val len = 10
          val optLen = 7
          val sync = 0x55
          val init = fromInt(len) ++ CompactByteString(optLen, packType)
          val headerCrc = Crc8(init)
          //                    76543210
          val db0NotLearn = bit"00001000"
          val db0Learn    = bit"11110000"
          val db1Summer   = bit"00001000"
          val db1Regular  = bit"00000000"
          val db3Learn    = getDb(3, dbs)
          val db2Learn    = getDb(2, dbs)
          val db1Learn    = getDb(1, dbs)
          val db1 = db1Regular
          val temp = 22
          val db2 = temp * 255 / 20
          val db3 = newPos
          val newDbs = if(teach)
            CompactByteString(db3Learn, db2Learn, db1Learn, db0Learn)
          else
            CompactByteString(db3, db2, db1, db0NotLearn)
          val senderId = sender
          val sec = 0x00
          val dbm = 0xff
          val opt = CompactByteString(0x01) ++ dest ++ CompactByteString(dbm, sec)
          val status = 0x00
          val data = CompactByteString(0xa5) ++ newDbs ++ senderId ++ CompactByteString(status)
          val dataCrc = Crc8(data ++ opt)
          CompactByteString(sync) ++ init ++ CompactByteString(headerCrc.toInt) ++ data ++ opt ++ CompactByteString(dataCrc.toInt)
        }
	  mappedDevices.get(senderBytes) match {
	    case Some((name, id)) =>
	              val broadcast  = CompactByteString(0xff, 0xff, 0xff, 0xff)
          	      implicit val us = baseId.take(3) ++ CompactByteString(baseId(3)+ id) // +CompactByteString(0xff, 0xd0, 0x50, 0x81) // Terminal.deviceId
		      val learnStatus = getBit(0,4, dbs)
                      val learnType = getBit(0,7, dbs)
		      val rorg = getDb(3, dbs) 
                      val learnString = if (learn == 0x00) s"LearnRequest($learnStatus, $learnType)" else  ""
                      log.info(learnString)
                      val newData = createReply(broadcast, 75, learn == 0x00)
                      operator ! Serial.Write(newData, length => Wrote(newData take length))
  	    case None => 
	      log.info("Not detected in config")
	  }
          storage = ByteString.empty
          reader ! ConsoleReader.Read
        } else {
          log.info(s" $sender -> $dest $rorg")
          storage = ByteString.empty
        }
      }

      }
    }

    case Terminal.Wrote(data) => {
      log.info(s"Wrote data: ${formatData(data)}")
    }
    case Serial.Closed => {
      log.info("Operator closed normally, exiting terminal.")
      context unwatch operator
      context stop self
    }

    case Terminated(`operator`) => {
      log.error("Operator crashed, exiting terminal.")
      context stop self
    }

    case ConsoleReader.EOT => {
      log.info("Initiating close.")
      operator ! Serial.Close
    }

    case ConsoleReader.ConsoleInput(input) => {
      val data = ByteString(input.getBytes)
      operator ! Serial.Write(data, length => Wrote(data.take(length)))
      reader ! ConsoleReader.Read
    }
  }

}

object TerminalOps {
  implicit class ByteStringOps(val a: ByteString) extends AnyVal {
    def hex: String = "0x" + a.map(x => f"$x%02x").mkString(",")
  }

  implicit class ArrayOps(val a: Seq[Byte]) extends AnyVal {
    def hex: String = "0x" + a.map(x => f"$x%02x").mkString(",")
  }

  implicit class IntOps(val x: Int) extends AnyVal {
    def hex: String = f"0x$x%02x"
  }
  implicit class ByteOps(val x: Byte) extends AnyVal {
    def hex: String = f"0x$x%02x"
  }

  implicit class BitCtxt(val sc: StringContext)  {
    def bit(args: Any*): Int = {
      val str = sc.parts.mkString("")
      require(str.length == 8, s"Byte size should be 8, was ${str.length} for $str")
      Integer.parseInt(str, 2)
    }
  }

}

object Terminal {
  case class Wrote(data: ByteString) extends Serial.Event

  val deviceId = CompactByteString(0xff, 0xbe, 0x2f, 0x80)

  val mappedDevices: Map[ByteString, (String, Int)] = Map(
    CompactByteString(0xff,0x8a,0xc0,0x00) -> ("Bedroom", 1),
    CompactByteString(0xff,0xbe,0x2f,0x80) -> ("Babyroom", 2),
    CompactByteString(0xff,0xb0,0xe4,0x80) -> ("Bathroom", 3),
    CompactByteString(0xff,0xe1,0x49,0x80) -> ("Kitchen", 4),
    CompactByteString(0xff,0xd6,0x65,0x80) -> ("Livingroom", 5))

  def apply(port: String, settings: SerialSettings) = Props(classOf[Terminal], port, settings)

  private def formatData(data: ByteString): String =
    (for {
      d <- data
    } yield f"0x$d%02x") mkString ","


}
object Crc8 {
  private[this] val lookup: Array[Byte] = {
    implicit def intToByte(i: Int) = i.toByte
      Array[Byte](
        0x00, 0x07, 0x0e, 0x09, 0x1c, 0x1b, 0x12, 0x15, 0x38, 0x3f, 0x36, 0x31, 0x24, 0x23, 0x2a,
        0x2d, 0x70, 0x77, 0x7e, 0x79, 0x6c, 0x6b, 0x62, 0x65, 0x48, 0x4f, 0x46, 0x41, 0x54, 0x53,
        0x5a, 0x5d, 0xe0, 0xe7, 0xee, 0xe9, 0xfc, 0xfb, 0xf2, 0xf5, 0xd8, 0xdf, 0xd6, 0xd1, 0xc4,
        0xc3, 0xca, 0xcd, 0x90, 0x97, 0x9e, 0x99, 0x8c, 0x8b, 0x82, 0x85, 0xa8, 0xaf, 0xa6, 0xa1,
        0xb4, 0xb3, 0xba, 0xbd, 0xc7, 0xc0, 0xc9, 0xce, 0xdb, 0xdc, 0xd5, 0xd2, 0xff, 0xf8, 0xf1,
        0xf6, 0xe3, 0xe4, 0xed, 0xea, 0xb7, 0xb0, 0xb9, 0xbe, 0xab, 0xac, 0xa5, 0xa2, 0x8f, 0x88,
        0x81, 0x86, 0x93, 0x94, 0x9d, 0x9a, 0x27, 0x20, 0x29, 0x2e, 0x3b, 0x3c, 0x35, 0x32, 0x1f,
        0x18, 0x11, 0x16, 0x03, 0x04, 0x0d, 0x0a, 0x57, 0x50, 0x59, 0x5e, 0x4b, 0x4c, 0x45, 0x42,
        0x6f, 0x68, 0x61, 0x66, 0x73, 0x74, 0x7d, 0x7a, 0x89, 0x8e, 0x87, 0x80, 0x95, 0x92, 0x9b,
        0x9c, 0xb1, 0xb6, 0xbf, 0xb8, 0xad, 0xaa, 0xa3, 0xa4, 0xf9, 0xfe, 0xf7, 0xf0, 0xe5, 0xe2,
        0xeb, 0xec, 0xc1, 0xc6, 0xcf, 0xc8, 0xdd, 0xda, 0xd3, 0xd4, 0x69, 0x6e, 0x67, 0x60, 0x75,
        0x72, 0x7b, 0x7c, 0x51, 0x56, 0x5f, 0x58, 0x4d, 0x4a, 0x43, 0x44, 0x19, 0x1e, 0x17, 0x10,
        0x05, 0x02, 0x0b, 0x0c, 0x21, 0x26, 0x2f, 0x28, 0x3d, 0x3a, 0x33, 0x34, 0x4e, 0x49, 0x40,
        0x47, 0x52, 0x55, 0x5c, 0x5b, 0x76, 0x71, 0x78, 0x7f, 0x6a, 0x6d, 0x64, 0x63, 0x3e, 0x39,
        0x30, 0x37, 0x22, 0x25, 0x2c, 0x2b, 0x06, 0x01, 0x08, 0x0f, 0x1a, 0x1d, 0x14, 0x13, 0xae,
        0xa9, 0xa0, 0xa7, 0xb2, 0xb5, 0xbc, 0xbb, 0x96, 0x91, 0x98, 0x9f, 0x8a, 0x8d, 0x84, 0x83,
        0xde, 0xd9, 0xd0, 0xd7, 0xc2, 0xc5, 0xcc, 0xcb, 0xe6, 0xe1, 0xe8, 0xef, 0xfa, 0xfd, 0xf4, 0xf3)
    }
    def apply(x: Seq[Byte]) =
      x.foldLeft(0x00.toByte)((crc, v) => lookup((crc ^ (v & 0xff)).toByte & 0xff))

  }
