package com.persist

import JsonOps._

package object Format {

  def toJson(x: Any): Json = {
    x match {
      case _: Short | _: Int | _: Long | _: String | _: Boolean | _: Double | _: BigDecimal => x
      case x: Map[_, _] => x.map { case (key, value) => (toJson(key), toJson(value))}
      case x: Iterable[_] => x.map(toJson).toSeq
      case Some(v) => toJson(v)
      case None => jnull
      case obj => obj.asInstanceOf[ {def toJson(): Json}].toJson()
    }
  }

  trait Readable[+A] {
    def read(json: Json): A
  }

  object read {
    def apply[A: Readable](json: Json): A = implicitly[Readable[A]].read(json)
  }

  object Readable {

    implicit object ReadString extends Readable[String] {
      def read(json: Json) = json.asInstanceOf[String]
    }

    implicit object ReadShort extends Readable[Short] {
      def read(json: Json) = json.asInstanceOf[Short]
    }

    implicit object ReadInt extends Readable[Int] {
      def read(json: Json) = json.asInstanceOf[Int]
    }

    implicit object ReadLong extends Readable[Long] {
      def read(json: Json) = json.asInstanceOf[Long]
    }

    implicit object ReadBoolean extends Readable[Boolean] {
      def read(json: Json) = json.asInstanceOf[Boolean]
    }

    implicit object ReadDouble extends Readable[Double] {
      def read(json: Json) = json.asInstanceOf[Double]
    }

    implicit object ReadBigDecimal extends Readable[BigDecimal] {
      def read(json: Json) = json.asInstanceOf[BigDecimal]
    }

    def extractSeq[T: Readable](json: Json): Seq[T] = json.asInstanceOf[Seq[Json]].map(implicitly[Readable[T]].read(_))

    implicit def readSet[T: Readable]: Readable[Set[T]] = new Readable[Set[T]] {
      def read(json: Json): Set[T] = extractSeq[T](json).toSet
    }

    implicit def readList[T: Readable]: Readable[List[T]] = new Readable[List[T]] {
      def read(json: Json): List[T] = extractSeq[T](json).toList
    }

    implicit def readMap[T: Readable, X: Readable]: Readable[Map[T, X]] = new Readable[Map[T, X]] {
      def read(json: Json): Map[T, X] = json.asInstanceOf[Map[Json, Json]].map { case (key, value) => (implicitly[Readable[T]].read(key), implicitly[Readable[X]].read(value))}
    }

    implicit def readOption[T: Readable]: Readable[Option[T]] = new Readable[Option[T]] {
      def read(json: Json): Option[T] = if (json == jnull) None else Some(implicitly[Readable[T]].read(json))
    }
  }
}


