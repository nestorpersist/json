/*
 *  Copyright 2012-2013 Persist Software
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

import com.persist.json.{ReadWriteCodec, ReadCodec, WriteCodec}
import org.specs2.mutable._
import com.persist.JsonOps._
import shapeless._
import syntax.singleton._

case class Ref(name: String)
case class Individual(name: String, age: Option[Int], friend: Option[Ref])
case class Meetup(city: String, people: Seq[Individual], cnt: Int, props: JsonObject/*, value:BigDecimal*/)
case class Meetup1(city: String, people: Seq[Individual], cnt: Int, props: Map[String, Any]/*, value:BigDecimal*/)
case class Meetup2(city: String, people: Seq[Individual], cnt: Int)
case class ByteTest(buffer: ByteBuffer)

case class FullTrip(s: Short, l: Long)

object Codecs {
  implicit val ref: ReadCodec[ByteTest] = ReadCodec[ByteTest]
  implicit val blablabla: ReadCodec[FullTrip] = ReadCodec[FullTrip]
}

class JsonFormatTest extends Specification {

  import Codecs._
  val individual = Individual("Bill", Some(45), Some(Ref("Bob")))
  val expectedP = JsonObject("name" -> "Bill", "age" -> 45, "friend" -> JsonObject("name" -> "Bob"))

  "JsonFormat" should {
    "automatic codec generation" in {
      "simple" in {

        val p = Individual("Bill", Some(45), Some(Ref("Bob")))

        val j = json.toJson(individual)

        j ==== expectedP
      }

      "Seq" in {
        val m = Meetup2("Montreal", List(individual), 1)
        val j = json.toJson(m)
        val expected = JsonObject("city" -> "Montreal", "people" -> JsonArray(expectedP), "cnt" -> 1)
        j ==== expected
      }

      "JsonObject" in {
        implicit val refCodec = WriteCodec[Ref]
        implicit val individualCodec = WriteCodec[Individual]
        implicit val meetupCodec = WriteCodec[Meetup]

        val m = Meetup("Montreal", List(individual), 1, JsonObject("name" -> true) /*, 3.6*/)
        val j = json.toJson(m)
        val expected = JsonObject("city" -> "Montreal", "people" -> JsonArray(expectedP), "cnt" -> 1, "props" -> JsonObject("name" -> true) /*, "value" -> 3.6*/)
        j ==== expected
      }

      "Map" in {
        implicit val refCodec = WriteCodec[Ref]
        implicit val individualCodec = WriteCodec[Individual]
        implicit val meetupCodec = WriteCodec[Meetup1]

        val m = Meetup1("Montreal", List(individual), 1, Map("name" -> true) /*, 3.6*/)
        val j = json.toJson(m)
        val expected = JsonObject("city" -> "Montreal", "people" -> JsonArray(expectedP), "cnt" -> 1, "props" -> Map("name" -> true) /*, "value" -> 3.6*/)
        j ==== expected
      }
      //    "just map" in {
      //      val map = Map("hello" -> true)
      //      val j = json.toJson(map)
      //      val expected = JsonObject("hello" -> true)
      //      j ==== expected
      //    }
      "full trip" in {
        val test = FullTrip(4, 4)
        val jsonValue = json.toJson(test)
        val stringValue = Compact(jsonValue)
        val otherJsonValue = Json(stringValue)

        val otherTest = blablabla.read(otherJsonValue)//json.read[FullTrip](otherJsonValue)
        test ==== otherTest
      }
      "Integer" in {
        "read" in {
          val a: Integer = json.read[Integer](5)
          a ==== 5
        }
        "write" in {
          val jsonValue = json.toJson(new Integer(5))
          jsonValue ==== 5
        }
      }
    }
    "read and readWrite" in {
      implicit val codec = new ReadWriteCodec[FullTrip] {
        def read(j: Json): FullTrip = FullTrip(1,1)
        def write(obj: FullTrip): Json = Map()
      }
      val optionTest = json.read[Option[FullTrip]](JsonObject("s" -> 4, "l" -> 4))
      optionTest ==== Some(FullTrip(1,1))
    }
    "byteBuffer" in {
      //implicit val writeCodec1: WriteCodec[ByteTest] = WriteCodec[ByteTest]
      //

      val obj = ByteTest(ByteBuffer.wrap(Array[Byte](1,2,3,4,5,6)))
      val jsonThing = json.toJson(obj)
      val obj1 = json.read[ByteTest](jsonThing)
      obj ==== obj1
    }
  }
}