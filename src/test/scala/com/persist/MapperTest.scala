/*
 *  Copyright 2012 Persist Software
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
import com.persist.JsonMapper._

case class Person(name: String, age: Option[Int])
case class Group(city: String, people: Seq[Person], var cnt: Int, props: JsonObject)

@RunWith(classOf[JUnitRunner])
class MapperTest extends FunSuite {

  test("mapper") {


    val j: Json = Json("""{city:"Seattle", cnt:2, props:{i:1, j:2},
                         people:[{name:"Joe"},
                                 {name:"Tom", age:20}]
                        }""")

    val group: Group = ToObject[Group](j)

    val j1: Json = ToJson(group)

    assert(j1 == j, "mapper fail")
  }

}