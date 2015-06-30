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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import com.persist.JsonOps._
//import com.persist.JsonMapper._
import java.nio.ByteBuffer
import com.persist.json.{ReadWriteCodec, ReadCodec, WriteCodec}

case class Person1(name: String, age: Option[Int])
case class Person(name: String, age: Option[Int], friend: Option[Person])
case class Group(city: String, people: Seq[Person], var cnt: Int, props: JsonObject, value:BigDecimal)


@RunWith(classOf[JUnitRunner])
class PersonTest extends FunSuite {
  implicit val rr = ReadCodec[Person1]
  implicit val pr = ReadCodec[Person]
  implicit val pw = WriteCodec[Person]
  implicit val gr = ReadCodec[Group]
  implicit val gw = WriteCodec[Group]

  test("mapper") {


    val j: Json = Json("""{city:"Seattle", cnt:2, props:{i:1, j:2}, value:2.3,
                         people:[{name:"Joe", friend:{name:"Sam"}},
                                 {name:"Tom", age:20}]
                        }""")

    //val mv = ToObject[Map[String,Json]](Map("a"->3,"b"->"foo"))
    //val iv = ToObject[Integer](17)
    val group: Group = json.read[Group](j)
    val j1: Json = json.toJson(group)

    assert(j1 === j, "mapper fail")
  }

}