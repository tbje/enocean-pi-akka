package tbje.enocean

import akka.util.{ ByteString => BS, CompactByteString => CBS }

object iRTV {

  val dbs = CBS() // todo
  val opt = CBS() // todo

  def getDb(db: Int, data: BS) =
    data(data.length - db - 1)

  def getBit(db: Int, bit: Int, data: BS) =
    getDb(db, data) >> bit & 1

  val dbm = -opt(5)

  val learn = getBit(0,3, dbs) // DB0.3

  def intTemp = 40 * getDb(1,dbs) / 255 // DB1

  val valvePos = getDb(3, dbs) // DB3

  val energyHarvested = getBit(2, 5, dbs) // DB2.5

  val energyHarvesting = getBit(2, 6, dbs) // DB2.6

}
