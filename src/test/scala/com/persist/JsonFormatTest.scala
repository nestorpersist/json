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

import com.persist.json.WriteCodec
import org.specs2.mutable._
import com.persist.JsonOps._

case class Ref(name: String)
case class Individual(name: String, age: Option[Int], friend: Option[Ref])
case class Meetup(city: String, people: Seq[Individual], cnt: Int, props: JsonObject/*, value:BigDecimal*/)
case class Meetup1(city: String, people: Seq[Individual], cnt: Int, props: Map[String, Any]/*, value:BigDecimal*/)
case class Meetup2(city: String, people: Seq[Individual], cnt: Int)

class JsonFormatTest extends Specification {

  val individual = Individual("Bill", Some(45), Some(Ref("Bob")))
  val expectedP = JsonObject("name" -> "Bill", "age" -> 45, "friend" -> JsonObject("name" -> "Bob"))

  "automatic codec generation" should {
    "simple" in {

      val p = Individual("Bill", Some(45), Some(Ref("Bob")))

      import WriteCodec.auto._


      val j = json.toJson(individual)

      j ==== expectedP
    }

    "Seq" in {
      import WriteCodec.auto._

      val m = Meetup2("Montreal", List(individual), 1)
      val j = json.toJson(m)
      val expected = JsonObject("city" -> "Montreal", "people" -> JsonArray(expectedP), "cnt" -> 1)
      j ==== expected
    }

    "JsonObject" in {
      implicit val refCodec = WriteCodec[Ref]
      implicit val individualCodec = WriteCodec[Individual]
      implicit val meetupCodec = WriteCodec[Meetup]

      val m = Meetup("Montreal", List(individual), 1 , JsonObject("name" -> true)/*, 3.6*/)
      val j = json.toJson(m)
      val expected = JsonObject("city" -> "Montreal", "people" -> JsonArray(expectedP), "cnt" -> 1 , "props" -> JsonObject("name" -> true)/*, "value" -> 3.6*/)
      j ==== expected
    }

    "Map" in {
      implicit val refCodec = WriteCodec[Ref]
      implicit val individualCodec = WriteCodec[Individual]
      implicit val meetupCodec = WriteCodec[Meetup1]

      val m = Meetup1("Montreal", List(individual), 1 , Map("name" -> true)/*, 3.6*/)
      val j = json.toJson(m)
      val expected = JsonObject("city" -> "Montreal", "people" -> JsonArray(expectedP), "cnt" -> 1 , "props" -> Map("name" -> true)/*, "value" -> 3.6*/)
      j ==== expected
    }
//    "just map" in {
//      val map = Map("hello" -> true)
//      val j = json.toJson(map)
//      val expected = JsonObject("hello" -> true)
//      j ==== expected
//    }
  }
}