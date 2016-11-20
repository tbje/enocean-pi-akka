package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.util.{ ByteString => BS, CompactByteString => CBS }
import tbje.enocean.util.Crc8

object Parser {

  case class Parse(data: BS, dataLength: Int, optLength: Int)

  def props(mappedDevices: Map[BS, (String, Int)], mappedActors: Map[Int, ActorRef]): Props = Props(new Parser(mappedDevices, mappedActors))

  sealed trait ParseResult
  case class Response(code: Int, msg: String) extends ParseResult

  def parseResponse(data: BS, optData: BS): Response =
    data(0) match {
      case 0x00 => Response(0x00, "RET_OK")  // OK ... command is understood and triggered
      case 0x01 => Response(0x01, "RET_ERROR")  // There is an error occurred
      case 0x02 => Response(0x02, "RET_NOT_SUPPORTED")  // The functionality is not supported by that implementation
      case 0x03 => Response(0x03, "RET_WRONG_PARAM")  // There was a wrong parameter in the command
      case 0x04 => Response(0x04, "RET_OPERATION_DENIED")  // Example: memory access denied (code-protected)
      case 0x05 => Response(0x05, "RET_LOCK_SET")  // Duty cycle lock
      case 0x06 => Response(0x06, "RET_BUFFER_TO_SMALL")  // The internal ESP3 buffer of the device is too small, to handle
      case 0x07 => Response(0x07, "RET_NO_FREE_BUFFER")  // Currently all internal buffers are used.
      case    x => Response(x   , "Unknown")
    }

  import util.DSL._
  def parseErp1(data: BS, optData: BS): ParseResult =
    data(0) match {
      case 0xa5 => parse4bs(data, optData)
      case -91 => parse4bs(data, optData)
      case    x => Response(x   , s"Unknown ${x.hex}")
    }

  class DBS(data: BS) {
    def getDb(db: Int) =
      data(data.length - db - 1)

    def getBit(db: Int, bit: Int): Int =
      getDb(db) >> bit & 1

    def int(offset: Int, length: Int) =
      Bits.int(data, offset, length)
  }

  case class IRTV(sender: BS, dest: BS, valvePos: Int, eneryHarvesting: Boolean, sufficienEnergy: Boolean, dbm: Int, intTemp: Int) extends ParseResult {
    override def toString =
      s"IRTV(sender = ${sender.hex2}, valvePos = $valvePos, eneryHarvesting: $eneryHarvesting, sufficienEnergy: $sufficienEnergy, intTemp: $intTemp, dbm = $dbm, dest = ${dest.hex2})"
  }
  case class Learn(sender: BS, dest: BS, rorg: Int, func: Int, eeptype: Int, manufacturer: Int, learnType: Int, learnStatus: Int) extends ParseResult {
    override def toString =
      s"Lern(sender: ${sender.hex2}, rorg: ${rorg}, func: ${func.hex}, type: ${eeptype.hex}, manu: ${manufacturer.hex}, dest: ${dest.hex2}, status: $learnStatus, learnType: $learnType)"
  }

  def parse4bs(data: BS, optData: BS): ParseResult = {
    val dbs = new DBS(data.slice(1,5))
    val learn = dbs.getBit(0, 3) // DB0.bit_3
    if (learn == 0)
      Learn(
        rorg = Bits.int(data, 0, 8),
        func = dbs.int(0, 6), // 6 first bits of DB
        eeptype = dbs.int(6, 7), // 7 next bits
        manufacturer = dbs.int(13, 11), // 11 next bits
        learnType = dbs.getBit(0, 7),
        learnStatus = dbs.getBit(0, 4),
        dest = optData.slice(1, 5),
        sender = data.slice(data.length-5, data.length-1)
      )
    else
      IRTV(
        dest = optData.slice(1, 5),
        sender = data.slice(data.length-5, data.length-1),
        dbm = -optData(5),
        intTemp = 40 * (dbs.getDb(1) & 0xff) / 255, // DB0.bit_7
        valvePos = dbs.getDb(3).toInt,
        eneryHarvesting = dbs.getBit(2, 6) == 1,
        sufficienEnergy = dbs.getBit(2, 5) == 1
      )
  }

  def parse(payload: BS, dataLen: Int, optLen: Int): ParseResult = {
    val headers = payload.slice(1, 6)
    val dataOffsetEnd = 6 + dataLen
    val optOffsetEnd = dataOffsetEnd + optLen
    val allData = payload.slice(6, optOffsetEnd)
    val data = payload.slice(6, dataOffsetEnd)
    val optData = payload.slice(dataOffsetEnd, optOffsetEnd)
    val crcHeaders = headers.last
    val crcAllData = payload.last
    require(crcHeaders == Crc8(headers.init), s"Headers CRC not valid, was ${Crc8(headers.init)}")
    require(crcAllData == Crc8(allData), s"Data CRC not valid, was ${Crc8(allData)}")
    val packageType = headers(3)
    packageType match {
      case 0x02 => parseResponse(data, optData)
      case 0x01 => parseErp1(data, optData)
      case x =>  ???
    }
  }


}

class Parser(mappedDevices: Map[BS, (String, Int)], mappedActors: Map[Int, ActorRef]) extends Actor with ActorLogging {
  import Parser._

  val addressToActor: Map[BS, ActorRef] =
    mappedDevices.collect {
      case (bs, (name, id)) if mappedActors contains id => bs -> mappedActors(id)
    }(collection.breakOut)

  def receive: Receive = {
    case Parse(payload, dataLen, optLen) =>
      val parseResult = parse(payload, dataLen, optLen)
      log.debug(parseResult.toString)
      parseResult match {
        case msg @ IRTV(sender, _, valvePos, eneryHarvesting, sufficienEnergy, dbm, intTemp) =>
          addressToActor.get(sender).foreach(_ ! msg)
        case msg @ Learn(sender, _, rorg, func, eeptype, manufacturer, learnType, learnStatus) =>
          addressToActor.get(sender).foreach(_ ! msg)
      }
  }
}
