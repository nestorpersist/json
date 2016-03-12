package com.persist

import java.io._
import com.persist.Exceptions.SystemException
import com.persist.JsonOps._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder

object Smile {

  private[this] val PREFIX1 = 0x3A.toByte
  private[this] val PREFIX2 = 0x29.toByte
  private[this] val PREFIX3 = 0x0A.toByte
  private[this] val PREFIX4 = 0x00.toByte
  private[this] val EMPTY_STRING = 0x22.toByte
  private[this] val NULL = 0x21.toByte
  private[this] val FALSE = 0x22.toByte
  private[this] val TRUE = 0x23.toByte
  private[this] val START_OBJECT = 0xFA.toByte
  private[this] val END_OBJECT = 0xFB.toByte
  private[this] val START_ARRAY = 0xF8.toByte
  private[this] val END_ARRAY = 0xF9.toByte
  private[this] val STRING64 = 0x80.toByte
  private[this] val START_STRING = 0xE1.toByte
  private[this] val END_STRING = 0xFC.toByte
  private[this] val INT = 0x24.toByte
  private[this] val LONG = 0x25.toByte
  private[this] val END = 0xFF.toByte
  private[this] val UTF8 = java.nio.charset.Charset.forName("UTF-8")

  // TODO better exceptions
  case class SmileException() extends SystemException("Smile", "")

  def stringToBytes(s: String): Array[Byte] = {

    val ss = new ByteArrayOutputStream()
    val x = new OutputStreamWriter(ss, UTF8)
    x.write(s)
    x.close()
    ss.toByteArray
  }

  def bytesToString(b: Array[Byte]): String = {
    val ss = new ByteArrayInputStream(b)
    val x = new InputStreamReader(ss, UTF8)
    val sb = new StringBuilder(10)
    @tailrec def read(): Unit = {
      val i = x.read()
      if (i != -1) {
        sb.append(i.toChar)
        read()
      }
    }
    read()
    x.close()
    sb.toString()
  }

  def intToZigZag(n: Int): Int = (n << 1) ^ (n >> 31)

  def zigZagToInt(z: Int): Int = (z >> 1) ^ (-(z & 1))

  @tailrec def intToVInt7(n: Int, accum: Array[Byte]): Array[Byte] = {
    if (n == 0) {
      accum
    } else {
      val last7 = (n & 0x7F).toByte
      val rest = n >> 7
      intToVInt7(rest, last7 +: accum)
    }
  }

  def intToVInt(n: Int): Array[Byte] = {
    val last6 = (n & 0x3F).toByte
    val rest = n >> 6
    intToVInt7(rest, Array(last6))
  }

  // TODO
  def longToVInt(n: Long): Array[Byte] = {
    // at least 5 bytes
    ???
  }

  def longToZigZag(n: Long): Long = (n << 1) ^ (n >> 63)

  def zigZagToLong(z: Long): Long = (z >> 1) ^ (-(z & 1))

  def dump(a: Array[Byte]): Unit = {
    val s = for (i <- a) yield {
      val high = (i & 0xF0) >> 4
      val low = i & 0x0F
      f"$high%H$low%H"
    }
    println(s.mkString(" "))
  }

  def check(s: String) = {
    try {
      dump(ToBinary(Json(s)))
    } catch {
      case ex: Throwable =>
        println(ex)
    }
  }

  def ToBinary(j: Json): Array[Byte] = {
    val ab = ArrayBuilder.make[Byte]

    def add(b: Byte): Unit = ab += b

    def adda(b: Array[Byte]): Unit = ab ++= b

    def toBinary(j: Json) {
      j match {
        case s: String => {
          val b = stringToBytes(s)
          val size = b.size
          if (size == 0) {
            add(EMPTY_STRING)
          } else if (size < 64) {
            add((STRING64 + size).toByte)
            adda(b)
          } else {
            add(START_STRING)
            adda(b)
            add(END_STRING)
          }
        }
        case m: scala.collection.Map[_, _] => {
          add(START_OBJECT)
          for ((k, v) <- m) {
            toBinary(k)
            toBinary(v)
          }
          add(END_OBJECT)
        }
        case list: Seq[_] => {
          add(START_ARRAY)
          for (l <- list) toBinary(l)
          add(END_ARRAY)
        }
        case x: Int =>
          add(INT)
          val z = intToZigZag(x)
          val v = intToVInt(z)
          adda(v)
        case x: Long =>
          add(LONG)
          val z = longToZigZag(x)
          val v = longToVInt(z)
          adda(v)
        case x: BigDecimal => // TODO
        case x: Double => // TODO
        case x: Boolean => add(if (x) TRUE else FALSE)
        case x: Float => // TODO
        case x: Number => //TODO dispatch to int long, double or float, bigdecimal
        case null => add(NULL)
        case x => throw new SmileException()
      }
    }

    add(PREFIX1)
    add(PREFIX2)
    add(PREFIX3)
    add(PREFIX4)
    toBinary(j)
    add(END)
    ab.result()
  }

  def FromBinary(a: Array[Byte]): Json = {
    val it = a.toIterator
    var pos = 0;
    def getNext() = {
      if (it.hasNext) {
        pos += 1
        it.next
      } else {
        0xFF.toByte
      }
    }
    @tailrec def getObj(accum: JsonObject): Json = {
      val next = getNext()
      if (next == END_OBJECT) {
        accum
      } else {
        val k = fromBinary(next) match {
          case s: String => s
          case _ => throw SmileException()
        }
        val next1 = getNext()
        val v = fromBinary(next1)
        val accum1 = accum + (k -> v)
        getObj(accum1)
      }
    }

    def getString(): String = {
      // TODO
      ???
    }

    def getStringCnt(size: Int): String = {
      val b = for (i <- (0 until size).toArray) yield getNext()
      val s = bytesToString(b)
      s
    }

    def fromBinary(next: Byte): Json = {
      next match {
        case EMPTY_STRING => ""
        case NULL => jnull
        case FALSE => false
        case TRUE => true
        case START_OBJECT =>
          getObj(emptyJsonObject)
        case START_ARRAY => ???
        case START_STRING =>
          getNext()
          getString()
        case x =>
          val prefix = next & 0xF0
          prefix match {
            case 0X80 | 0x90 | 0xA0 | 0XB0 =>
              getStringCnt(next - STRING64)
            case x1 =>
              dump(Array(x))
              println(pos)
              throw SmileException()
          }
      }
    }
    if (getNext() != PREFIX1) throw SmileException()
    if (getNext() != PREFIX2) throw SmileException()
    if (getNext() != PREFIX3) throw SmileException()
    if (getNext() != PREFIX4) throw SmileException()
    val result = fromBinary(getNext())
    if (getNext() != END) throw SmileException()
    result
  }
}
