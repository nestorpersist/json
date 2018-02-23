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
import com.persist.JsonMapper._
import com.persist.json._

case class Person(name: String, age: Option[Int], friend: Option[Person], id: BigInt)

case class Group(city: String, people: Seq[Person], kinds:Array[String],
                 var cnt: Int, props: JsonObject, value: BigDecimal)

@RunWith(classOf[JUnitRunner])
class MapperTest extends FunSuite {

  test("mapper") {


    val j: Json = Json( """{city:"Seattle", cnt:2, props:{i:1, j:2}, value:2.3,
                         people:[{name:"Joe", friend:{name:"Sam"}, id: 9872349872349827349872349872},
                                 {name:"Tom", age:20, id: 989823498723498234982349829}], kinds:["red","blue"]
                        }""")

    val group = ToObject[Group](j)

    val j1: Json = ToJson(group)

    assert(j1 === j, "mapper fail")
  }

}
