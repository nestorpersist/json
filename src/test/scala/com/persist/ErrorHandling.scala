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

import com.persist.Exceptions.MappingException
import com.persist.json.ReadCodec
import org.specs2.mutable._
import com.persist.JsonOps._
import shapeless._
import syntax.singleton._

case class Ref4(name: String)
case class Individual4(name: String, age: Option[Int], friend: Option[Ref4])
case class Meetup4(city: String, people: Seq[Individual], cnt: Int, props: JsonObject/*, value:BigDecimal*/)
case class Meetup41(city: String, people: Seq[Individual], cnt: Int, props: Map[String, Any]/*, value:BigDecimal*/)
case class Meetu4p2(city: String, people: Seq[Individual], cnt: Int)

class ErrorHandlingTest extends Specification {

  val individual = Individual4("Bill", Some(45), Some(Ref4("Bob")))
  val expectedP = JsonObject("name" -> "Bill", "age" -> 45, "friend" -> JsonObject("name" -> "Bob"))

  "error handling" should {
    "missing field" in {
      "simple" in {
        val json_ = JsonObject("name1" -> "Bill", "age" -> 45)

        json.read[Individual4](json_) must throwA[MappingException].like { case ex =>
          ex ==== MappingException(s"Expected field name on JsonObject $json_", "")
        }
      }
      "nested" in {
        val json_ = JsonObject("name" -> "Bill", "age" -> 45, "friend" -> JsonObject("name1" -> "Bob"))

        json.read[Individual4](json_) must throwA[MappingException].like { case ex =>
          ex ==== MappingException(s"""Expected field name on JsonObject Map(name1 -> Bob)""", "friend/")
        }
      }
      "optional" in {
        val json_ = JsonObject("name" -> "Bill", "age" -> 45)

        json.read[Individual4](json_) ==== Individual4("Bill", Some(45), None)
      }
    }
    "wrong type" in {
      val json_ = JsonObject("name" -> 45, "age" -> 45, "friend" -> JsonObject("name" -> "Bob"))

      json.read[Individual4](json_) must throwA[MappingException].like { case ex =>
        ex ==== MappingException(s"""Expected: String but found 45""", "name/")
      }
    }
  }
}