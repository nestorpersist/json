/*
 *  Copyright 2012-2015 Persist Software
 *
 *   http://www.persist.com
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
*/

package com.persist

import java.nio.ByteBuffer

import com.persist.Exceptions.MappingException
import com.persist.JsonOps._
import shapeless._
import shapeless.labelled._
import shapeless.syntax.typeable._

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

//import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.util.Try

/**
 * This package contains the codec operations used to
 * implement ToJson and ToObject.
 * These operations are normally not called directly.
 */
package object json {

  trait ReadWriteCodec[T] extends ReadCodec[T] with WriteCodec[T]

  object ReadWriteCodec {
    trait SimpleCodec[T] extends ReadWriteCodec[T] {
      def write(x: T):Json = x
      def read(x: Json):T = x.asInstanceOf[T]
    }
  }

  @implicitNotFound(msg = "Cannot find ReadCodec for ${T}")
  trait ReadCodec[T] {
    def read(json: Json): T
  }

  trait LowPriorityReadCodec {

    def castOrThrow(json: Json): JsonObject = json.cast[JsonObject].getOrElse(throw new MappingException(s"Expected JsonObject but found $json"))

    implicit def deriveHCons[K <: Symbol, V, T <: HList]
    (implicit
     key: Witness.Aux[K],
     headCodec: Lazy[ReadCodec[V]],
     tailCodec: Lazy[ReadCodec[T]]
      ): ReadCodec[FieldType[K, V] :: T] = new ReadCodec[FieldType[K, V] :: T] {
      def read(json: Json): FieldType[K, V] :: T = {
        val map = castOrThrow(json)
        val name = key.value.name
        // This is so that we gracefully handle a missing field if it's type is optional
        val fieldValue = map.getOrElse(name, throw new MappingException(s"""Expected field "${key.value.name}" on JsonObject $map"""))
        // Try reading the value of the field
        // If we get a mapping exception, intercept it and add the name of this field to the path
        // If we get another exception, don't touch!
        // Pitfall: if handle did not accept a PartialFunction, we could transform an unknown exception into a match exception
        val head: V = Try(headCodec.value.read(fieldValue)).recover{ case MappingException(msg, path) => throw MappingException(msg, s"$name/$path")}.get
        val tail = tailCodec.value.read(json)
        field[K](head) :: tail
      }
    }
  }

  object ReadCodec extends LowPriorityReadCodec {

    val ShortMinValueAsBI = BigInt(Short.MinValue)
    val ShortMaxValueAsBI = BigInt(Short.MaxValue)

    val ByteMinValueAsBI = BigInt(Byte.MinValue)
    val ByteMaxValueAsBI = BigInt(Byte.MaxValue)

    def apply[T](implicit st: Lazy[ReadCodec[T]]): ReadCodec[T] = st.value

    implicit val string = new ReadCodec[String] {
      def read(x: Json): String = x.cast[String].getOrElse(throw new MappingException(s"Expected: String but found $x"))
    }
    implicit val char = new ReadCodec[Char] {
      def read(x: Json): Char = x.cast[String].filter(_.length == 1).map(_.charAt(0)).getOrElse(
        throw new MappingException(s"Expected String of length one but found $x")
      )
    }
    implicit val int = new ReadCodec[Int] {
      def read(x: Json): Int = x match {
        case x: Byte => x.toInt
        case x: Short => x.toInt
        case x: Int => x
        case x: Long =>
          if (x >= Int.MinValue && x <= Int.MaxValue) x.toInt
          else throw new MappingException(s"Expected number that can fit into an Int, but found $x")
        case x: BigInt =>
          if (x >= Int.MinValue && x <= Int.MaxValue) x.toInt
          else throw new MappingException(s"Expected number that can fit into an Int, but found $x")
        case _ => throw new MappingException(s"Expected: Int but found $x")
      }
    }
    implicit val boolean = new ReadCodec[Boolean] {
      def read(x: Json): Boolean = x.cast[Boolean].getOrElse(throw new MappingException(s"Expected: Boolean but found $x"))
    }
    implicit val long = new ReadCodec[Long] {
      def read(x: Json): Long = x match {
        case x: Byte => x.toLong
        case x: Int => x.toLong
        case x: Short => x.toLong
        case x: Long => x
        case x: BigInt =>
          if (x >= Long.MinValue && x <= Long.MaxValue) x.toLong
          else throw new MappingException(s"Expected number that can fit into a Long, but found $x")
        case _ => throw new MappingException(s"Expected: Long but found $x")
      }
    }
    implicit val short = new ReadCodec[Short] {
      def precisionException(num: Any) = throw new MappingException(s"Expected number that can fit into a Short, but found $num")
      def read(x: Json): Short = x match {
        case x: Byte => x.toShort
        case x: Short => x
        case x: Int =>
           if (x >= Short.MinValue && x <= Short.MaxValue) x.toShort
           else precisionException(x)
        case x: Long =>
          if (x >= Short.MinValue && x <= Short.MaxValue) x.toShort
          else precisionException(x)
        case x: BigInt =>
          if (x >= ShortMinValueAsBI && x <= ShortMinValueAsBI) x.toShort
          else precisionException(x)
        case _ => throw new MappingException(s"Expected: Short, but found $x")
      }
    }
    implicit val byte = new ReadCodec[Byte] {
      def precisionException(num: Any) = throw new MappingException(s"Expected number that can fit into a Byte, but found $num")
      def read(x: Json): Byte = x match {
        case x: Byte => x
        case x: Int =>
          if (x >= Byte.MinValue && x <= Byte.MaxValue) x.toByte
          else precisionException(x)
        case x: Short =>
          if (x >= Byte.MinValue && x <= Byte.MaxValue) x.toByte
          else precisionException(x)
        case x: Long =>
          if (x >= Byte.MinValue && x <= Byte.MaxValue) x.toByte
          else precisionException(x)
        case x: BigInt =>
          if (x >= ByteMinValueAsBI && x <= ByteMinValueAsBI) x.toByte
          else precisionException(x)
        case _ => throw new MappingException(s"Expected: Byte, but found $x")
      }
    }
    implicit val double = new ReadCodec[Double] {
      def read(x: Json): Double = x match {
        case x: Byte => x.toDouble
        case x: Int => x.toDouble
        case x: Float => x.toDouble
        case x: Short => x.toDouble
        case x: Long => x.toDouble
        case x: BigInt => x.toDouble
        case x: BigDecimal => x.toDouble
        case x: Double => x
        case _ => throw new MappingException(s"Expected: Double, but found $x")
      }
    }
    implicit val float = new ReadCodec[Float] {
      def read(x: Json): Float = x match {
        case x: Byte => x.toFloat
        case x: Float => x
        case x: Double => x.toFloat
        case x: Int => x.toFloat
        case x: Short => x.toFloat
        case x: Long => x.toFloat
        case x: BigInt => x.toFloat
        case x: BigDecimal => x.toFloat
        case _ => throw new MappingException(s"Expected Float, but found $x")
      }
    }
    implicit val bigInteger = new ReadCodec[BigInt] {
      def read(x: Json): BigInt = x match {
        case x: Byte => BigInt(x)
        case x: BigDecimal => x.toBigInt()
        case x: Float => BigDecimal(x.toDouble).toBigInt()
        case x: Double => BigDecimal(x).toBigInt()
        case x: Int => BigInt(x)
        case x: Short => BigInt(x)
        case x: Long => BigInt(x)
        case x: BigInt => x
        case _ => throw new MappingException(s"Expected BigDecimal, but found $x")
      }
    }
    implicit val bigDecimal = new ReadCodec[BigDecimal] {
      def read(x: Json): BigDecimal = x match {
        case x: Byte => BigDecimal(x)
        case x: BigDecimal => x
        case x: Float => BigDecimal(x.toDouble)
        case x: Double => BigDecimal(x)
        case x: Int => BigDecimal(x)
        case x: Short => BigDecimal(x)
        case x: Long => BigDecimal(x)
        case x: BigInt => BigDecimal(x)
        case _ => throw new MappingException(s"Expected BigDecimal, but found $x")
      }
    }
    implicit val integer = new ReadCodec[Integer] {
      def read(x: Json): Integer = int.read(x)
    }

    implicit val jobj = new ReadCodec[JsonObject] {
      def read(x: Json): JsonObject = jgetObject(x)
    }

    implicit val jarr = new ReadCodec[JsonArray] {
      def read(x: Json): JsonArray = jgetArray(x)
    }

    implicit val jjson = new ReadCodec[Json] {
      def read(x: Json): Json = x
    }

    def extractSeq[T: ReadCodec](json: Json): Seq[T] =
      json.cast[JsonArray].map(
        _.map(implicitly[ReadCodec[T]].read(_))
      ) getOrElse (
        throw new MappingException(s"Expected JsonArray but found $json")
        )

    implicit def set[T: ReadCodec] = new ReadCodec[Set[T]] {
      def read(json: Json): Set[T] = extractSeq[T](json).toSet
    }

    implicit def list[T: ReadCodec] = new ReadCodec[List[T]] {
      def read(json: Json): List[T] = extractSeq[T](json).toList
    }

    implicit def seq[T: ReadCodec] = new ReadCodec[Seq[T]] {
      def read(json: Json): Seq[T] = extractSeq[T](json)
    }

    implicit def arr[T: ReadCodec](implicit T1:ClassTag[T]) = new ReadCodec[Array[T]] {
      def read(json: Json): Array[T] = extractSeq[T](json).toArray[T]
    }

    //val x = arr[ReadCodec[Int]]

    implicit def simpleMap[V: ReadCodec] = new ReadCodec[Map[String, V]] {
      def read(json: Json): Map[String, V] = castOrThrow(json).mapValues(implicitly[ReadCodec[V]].read(_))
    }

    implicit def complexMap[K: ReadCodec, V: ReadCodec] = new ReadCodec[Map[K, V]] {
      def read(json: Json): Map[K, V] = castOrThrow(json).map { case (key, value) =>
        (implicitly[ReadCodec[K]].read(Json(key)), implicitly[ReadCodec[V]].read(value))
      }
    }

    implicit def option[T: ReadCodec] = new ReadCodec[Option[T]] {
      def read(json: Json): Option[T] = if (json == jnull) None else Some(implicitly[ReadCodec[T]].read(json))
    }

    implicit val byteBuffer = new ReadCodec[ByteBuffer] {
      def read(json: Json): ByteBuffer = {
        val string = com.persist.json.read[String](json)
        val bytes: Array[Byte] = string.map(_.toByte).toArray
        ByteBuffer.wrap(bytes)
      }
    }

    implicit def deriveHNil: ReadCodec[HNil] =
      new ReadCodec[HNil] {
        // This will silently accept extra fields within a JsonObject
        // To change this behavior make sure json is a JsonObject and that it is empty
        def read(json: Json) = HNil
      }

    implicit def deriveHConsWithOption[K <: Symbol, V, T <: HList]
    (implicit
     key: Witness.Aux[K],
     headCodec: Lazy[ReadCodec[V]],
     tailCodec: Lazy[ReadCodec[T]]
      ): ReadCodec[FieldType[K, Option[V]] :: T] = new ReadCodec[FieldType[K, Option[V]] :: T] {
        def read(json: Json): FieldType[K, Option[V]] :: T = {
          val map = castOrThrow(json)
          val name = key.value.name
          // This is so that we gracefully handle a missing or null-valued field if it's type is optional
          val fieldValue = map.get(name).flatMap(Option(_))
          // Try reading the value of the field
          // If we get a mapping exception, intercept it and add the name of this field to the path
          // If we get another exception, don't touch!
          // Pitfall: if handle did not accept a PartialFunction, we could transform an unknown exception into a match exception
          val head: Option[V] = fieldValue.map(field => Try{headCodec.value.read(field)}.recover{ case MappingException(msg, path) => throw MappingException(msg, s"$name/$path")}.get)
          val tail = tailCodec.value.read(json)
          field[K](head) :: tail
        }
      }

    implicit def deriveCNil: ReadCodec[CNil] = new ReadCodec[CNil] {
      def read(json: Json): CNil = throw new MappingException("no subclass of sealed trait found that could read this")
    }

    implicit def deriveCCons[K <: Symbol, V, T <: Coproduct]
    (implicit
     key: Witness.Aux[K],
     codecHead: Lazy[ReadCodec[V]],
     codecTail: Lazy[ReadCodec[T]]
      ): ReadCodec[FieldType[K, V] :+: T] =
      new ReadCodec[FieldType[K, V] :+: T] {
        def read(json: Json): FieldType[K, V] :+: T = {
          Try(Inl(field[K](codecHead.value.read(json)))).orElse(Try(Inr(codecTail.value.read(json)))).get
        }
      }

    implicit def deriveInstance[F, G]
    (implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[ReadCodec[G]]): ReadCodec[F] = new ReadCodec[F] {
        def read(json: Json): F = gen.from(sg.value.read(json))
      }
  }

  @implicitNotFound(msg = "Cannot find WriteCodec for ${T}")
  trait WriteCodec[T] {
    def write(obj: T): Json
  }

  object WriteCodec extends LabelledTypeClassCompanion[WriteCodec] {
    trait SimpleCodec[T] extends WriteCodec[T] {
      def write(x: T):Json = x
    }
    implicit object StringCodec extends SimpleCodec[String]
    implicit object CharCodec extends WriteCodec[Char] {
      def write(x: Char): Json = x.toString
    }
    implicit object IntCodec extends SimpleCodec[Int]
    implicit object BooleanCodec extends SimpleCodec[Boolean]
    implicit object LongCodec extends SimpleCodec[Long]
    implicit object ShortCodec extends SimpleCodec[Short]
    implicit object ByteCodec extends SimpleCodec[Byte]
    implicit object DoubleCodec extends SimpleCodec[Double]
    implicit object BigIntCodec extends SimpleCodec[BigInt]
    implicit object BigDecimalCodec extends SimpleCodec[BigDecimal]
    implicit object IntegerCodec extends SimpleCodec[Integer]
    implicit def simpleMap[V: WriteCodec] = new WriteCodec[scala.collection.Map[String, V]] {
      def write(obj: scala.collection.Map[String, V]): JsonObject = obj.mapValues(toJson(_)).toMap
    }
    implicit def simpleImmutableMap[V: WriteCodec] = new WriteCodec[Map[String, V]] {
      def write(obj: Map[String, V]): JsonObject = obj.mapValues(toJson(_)).toMap
    }
    implicit val jsonObject = new WriteCodec[JsonObject] {
      def write(obj: JsonObject): JsonObject = obj
    }
    implicit def complexMap[K: WriteCodec, V: WriteCodec] = new WriteCodec[scala.collection.Map[K,V]] {
      def write(obj: scala.collection.Map[K, V]) = obj.map { case (key, value) =>
        (Compact(implicitly[WriteCodec[K]].write(key)), implicitly[WriteCodec[V]].write(value))
      }
    }
    implicit def iterable[V: WriteCodec] = new WriteCodec[Iterable[V]] {
      def write(obj: Iterable[V]): JsonArray = obj.map(toJson(_)).toSeq
    }
    implicit def seq[V: WriteCodec] = new WriteCodec[Seq[V]] {
      def write(obj: Seq[V]): JsonArray = obj.map(toJson(_))
    }
    implicit def list[V: WriteCodec] = new WriteCodec[List[V]] {
      def write(obj: List[V]): JsonArray = obj.map(toJson(_))
    }
    implicit def vector[V: WriteCodec] = new WriteCodec[Vector[V]] {
      def write(obj: Vector[V]): JsonArray = obj.map(toJson(_))
    }
    implicit def option[V:WriteCodec] = new WriteCodec[Option[V]] {
      def write(obj: Option[V]):Json = obj.map(toJson(_)).getOrElse(jnull)
    }
    implicit def arr[V: WriteCodec] = new WriteCodec[Array[V]] {
      def write(obj: Array[V]): Json = obj.map(toJson(_)).toSeq
    }

        implicit val byteBuffer = new WriteCodec[ByteBuffer] {
      def write(obj: ByteBuffer): String = obj.array().slice(obj.position(),obj.limit()).map(_.toChar).mkString
    }

    val typeClass: LabelledTypeClass[WriteCodec] = new LabelledTypeClass[WriteCodec] {
      def emptyProduct = new WriteCodec[HNil] {
        def write(t: HNil): JsonObject = Map()
      }

      def product[F, T <: HList](name: String, FHead: WriteCodec[F], FTail: WriteCodec[T]) = new WriteCodec[F :: T] {
        def write(ft: F :: T) = {
          val head = FHead.write(ft.head)
          val tail = FTail.write(ft.tail).asInstanceOf[JsonObject]
          if (head == jnull) tail else tail + (name -> head)
        }
      }

      def project[F, G](instance: => WriteCodec[G], to : F => G, from : G => F) = new WriteCodec[F] {
        def write(f: F) = instance.write(to(f))
      }

      def coproduct[L, R <: Coproduct](name: String, cl: => WriteCodec[L], cr: => WriteCodec[R]) = new WriteCodec[L :+: R] {
        def write(lr: L :+: R): Json = lr match {
          case Inl(l) => cl.write(l)
          case Inr(r) => cr.write(r)
        }
      }

      def emptyCoproduct: WriteCodec[CNil] = sys.error("Could not serialize class, this should not have happened")
    }
  }

  def toJson[T](obj: T)(implicit codec: WriteCodec[T]): Json = codec.write(obj)
  def read[T](json: Json)(implicit codec: ReadCodec[T]): T = codec.read(json)
}