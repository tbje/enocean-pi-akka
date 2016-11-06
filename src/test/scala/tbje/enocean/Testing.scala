import akka.util.{ ByteString => BS, CompactByteString => CBS }

import scala.collection.immutable.Seq
import tbje.enocean.util._
import tbje.enocean.util.DSL._

object Testing {

  def getDb(db: Int, data: Seq[Byte]) =
    data(data.length - db - 1)

  def getBit(db: Int, bit: Int, data: Seq[Byte]) =
    getDb(db, data) >> bit & 1

  val bO = CBS(
    0x55,
    0x00, 0x0a, 0x07, 0x01, 0xeb,
    0xa5, 0x32, 0x20, 0x93, 0x08, 0xff, 0xe1, 0x49, 0x80, 0x00,
    0x01, 0xff, 0xff, 0xff, 0xff, 0x31, 0x00,
    0xb0)

  val b3 = CBS(
    0x55,
    0x00, 0x0a, 0x07, 0x01, 0xeb,
    0xa5, 0x32, 0x20, 0xa5, 0x08, 0xff, 0xd6, 0x65, 0x80, 0x00,
    0x01, 0xff, 0xff, 0xff, 0xff, 0x4d, 0x00, 0xbe)

  val b4 = CBS(
    0x55,
    0x00, 0x0a, 0x07, 0x01, 0xeb,
    0xa5, 0x32, 0x20, 0x9c, 0x08, 0xff, 0xe1, 0x49, 0x80, 0x00,
    0x01, 0xff, 0xff, 0xff, 0xff, 0x3a, 0x00, 0xaf)

//  val b: Seq[Byte] = Seq(0x55, 0x00, 0x0a, 0x07, 0x01, 0xeb, 0xa5, 0x32,
//      0x20, 0x96, 0x08, 0xff, 0xd6, 0x65, 0x80, 0x00, 0x01, 0xff, 0xff, 0xff, 0xff, 0x44, 0x00, 0xa5).map(_.toByte)
  val b = CBS(
    0x55,
    0x00, 0x0a, 0x07, 0x01, 0xeb,
    0xa5, 0x4b, 0x18, 0x00, 0x08, 0xff, 0xbe, 0x2f, 0x80, 0x00,
    0x01, 0xff, 0xe1, 0x49, 0x80, 0xff, 0x00, 0xdb)

  val z = CBS(
    0x55,
    0x00, 0x0a, 0x07, 0x01, 0xeb,
    0xa5, 0x32, 0x20, 0x96, 0x08, 0xff, 0xd6, 0x65, 0x80, 0x00,
    0x01, 0xff, 0xff, 0xff, 0xff, 0x44, 0x00, 0xa5)

  /*
    val b = Seq(0x55,0x00,0x0a,0x07,0x01,0xeb,0xa5,0x54,0xa0,0x0f,0xf0,0xff,0xd0,0x50,0x81,0x00,0x01,0xff,0xff,0xff,0xff,0xff,0x00,0xb9).map(_.toByte)
 */

  val initLen = 6
  val dataLen = b(1) << 8 | b(2)
  val optLen = b(3)
  val crcLen = 1
  val totalLen = Seq(initLen, dataLen, optLen.toInt, crcLen).sum
  require(totalLen == b.length, "Length not correct")
  val (init, allData) = b splitAt initLen
  val packType = b(4).hex
  val headerCrc = b(5) // ( & 0xff )
  val (data, rest) =  allData splitAt dataLen
  val opt = rest.init
  val dataCrc = b(initLen + dataLen + optLen) // allData.last
  require(Crc8(init.drop(1).init) == headerCrc, "Header CRC failed")
  require(Crc8(allData.init) == dataCrc, "Data CRC failed")
  val rorg = data(0).hex
  val dest = opt.drop(1).take(4).hex
  val sender = data.reverse.drop(1).take(4).reverse.hex
  val dbm = -opt(5)

  val dbs = data.take(5)
  val learn = getBit(0,3, dbs) // DB0.bit_
  val learn2 = getDb(1, dbs).hex // DB0.bit_


  val intTemp = 40 * getDb(1,dbs) / 255 // DB0.bit_7
  val valvePos = getDb(3, dbs)
  val energyHarvested = getBit(2, 5, dbs)
  val energyHarvesting = getBit(2, 6, dbs)

  def bitFill(x: Int*) = x.length
    bitFill(getBit(1,3,dbs), getBit(0,3,dbs), getBit(7, 2, dbs), getBit(6, 2, dbs), getBit(5, 2, dbs), getBit(4, 2, dbs), getBit(3, 2, dbs))

  def fromInt(x: Int): BS =
    CBS(x >> 8, x.toByte.toInt)

  def createPacket(command: Int, data: BS, opt: BS): BS = {
    val header = fromInt(data.length) ++ CBS(opt.length, command)
    val headerCrc = Crc8(header)
    val body = data ++ opt
    val bodyCrc = Crc8(body)
    CBS(0x55) ++ header ++ CBS(headerCrc) ++ body ++ CBS(bodyCrc)
  }

  val baseId = CBS(0xff, 0xbe, 0x2f, 0x80)

  def createReply(dest: BS, newPos: Int, teach: Boolean = false)(implicit sender: BS): BS = {
    require(sender.size == 4, s"sender size should be 4 was ${sender.hex}")
    require(dest.size == 4, s"dest size should be 4 was ${dest.hex}")
    val packType = 0x01
    val sync = 0x55
    //                    76543210
    val db0Learn    = bit"00001000"
    val db0Nolearn  = bit"00000000"
    val db1Summer   = bit"00001000"
    val db1Regular  = bit"00000000"
    val db3Learn    = bit"01010100" //first 6 func
    val db2Learn    = bit"10100000"
    val db1Learn    = bit"00001111"
    val db1 = db1Regular
    val temp = 22
    val db2 = temp * 255 / 20
    val db3 = newPos
    val dbs = if(teach) CBS(db3, db2, db1, db0Learn) else CBS(db3, db2, db1, db0Nolearn)
    val senderId = sender
    val dbm = 0xff
    val sec = 0x00
    val status = 0x00
    val data = CBS(0xa5) ++ dbs ++ senderId ++ CBS(status)
    val opt = CBS(0x01) ++ dest ++ CBS(dbm, sec)
    createPacket(packType, data, opt)
  }
}
