package com.persist

import JsonOps._
import scala.annotation.implicitNotFound

package object json {

  trait ReadWriteCodec[T] extends ReadCodec[T] with WriteCodec[T]

  object ReadWriteCodec {
    trait SimpleCodec[T] extends ReadWriteCodec[T] {
      def write(x: T):Json = x
      def read(x: Json):T = x.asInstanceOf[T]
    }
  }

  @implicitNotFound(msg = "Cannot find ReadCodec for ${T}")
  trait ReadCodec[+T] {
    def read(json: Json): T
  }

  object ReadCodec {
    trait SimpleCodec[T] extends ReadCodec[T] {
      def read(x: Json):T = x.asInstanceOf[T]
    }
    implicit object StringCodec extends SimpleCodec[String]
    implicit object IntCodec extends SimpleCodec[Int]
    implicit object BooleanCodec extends SimpleCodec[Boolean]
    implicit object LongCodec extends SimpleCodec[Long]
    implicit object ShortCodec extends SimpleCodec[Short]
    implicit object DoubleCodec extends SimpleCodec[Double]
    implicit object BigDecimalCodec extends SimpleCodec[BigDecimal]

    def extractSeq[T: ReadCodec](json: Json): Seq[T] = json.asInstanceOf[JsonArray].map(implicitly[ReadCodec[T]].read(_))

    implicit def set[T: ReadCodec] = new ReadCodec[Set[T]] {
      def read(json: Json): Set[T] = extractSeq[T](json).toSet
    }

    implicit def list[T: ReadCodec] = new ReadCodec[List[T]] {
      def read(json: Json): List[T] = extractSeq[T](json).toList
    }

    implicit def simpleMap[V: ReadCodec] = new ReadCodec[Map[String, V]] {
      def read(json: Json): Map[String, V] = json.asInstanceOf[JsonObject].mapValues(implicitly[ReadCodec[V]].read(_))
    }

    implicit def complexMap[K: ReadCodec, V: ReadCodec] = new ReadCodec[Map[K, V]] {
      def read(json: Json): Map[K, V] = json.asInstanceOf[JsonObject].map { case (key, value) =>
        (implicitly[ReadCodec[K]].read(Json(key)), implicitly[ReadCodec[V]].read(value))
      }
    }

    implicit def option[T: ReadCodec] = new ReadCodec[Option[T]] {
      def read(json: Json): Option[T] = if (json == jnull) None else Some(implicitly[ReadCodec[T]].read(json))
    }
  }

  @implicitNotFound(msg = "Cannot find WriteCodec for ${T}")
  trait WriteCodec[-T] {
    def write(obj: T): Json
  }

  object WriteCodec {
    trait SimpleCodec[T] extends WriteCodec[T] {
      def write(x: T):Json = x
    }
    implicit object StringCodec extends SimpleCodec[String]
    implicit object IntCodec extends SimpleCodec[Int]
    implicit object BooleanCodec extends SimpleCodec[Boolean]
    implicit object LongCodec extends SimpleCodec[Long]
    implicit object ShortCodec extends SimpleCodec[Short]
    implicit object DoubleCodec extends SimpleCodec[Double]
    implicit object BigDecimalCodec extends SimpleCodec[BigDecimal]
    implicit def simpleMap[V: WriteCodec] = new WriteCodec[scala.collection.Map[String, V]] {
      def write(obj: scala.collection.Map[String, V]): JsonObject = obj.mapValues(toJson(_)).toMap
    }
    implicit def complexMap[K: WriteCodec, V: WriteCodec] = new WriteCodec[scala.collection.Map[K,V]] {
      def write(obj: scala.collection.Map[K, V]) = obj.map { case (key, value) =>
        (Compact(implicitly[WriteCodec[K]].write(key)), implicitly[WriteCodec[V]].write(value))
      }
    }
    implicit def iterable[V: WriteCodec] = new WriteCodec[Iterable[V]] {
      def write(obj: Iterable[V]): JsonArray = obj.map(toJson(_)).toSeq
    }
    implicit def option[V: WriteCodec] = new WriteCodec[Option[V]] {
      def write(obj: Option[V]): Json = obj.map(toJson(_)).getOrElse(jnull)
    }
  }

  def toJson[T](obj: T)(implicit ev: WriteCodec[T]): Json = ev.write(obj)
  def read[T](json: Json)(implicit ev: ReadCodec[T]): T = ev.read(json)
}


