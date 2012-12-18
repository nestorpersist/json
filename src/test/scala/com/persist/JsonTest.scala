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

import com.persist._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import JsonOps._
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class JsonTest extends FunSuite {

  test("json") {

    val s = """
      {
        "a":3,          // a comment
        "b":"foo", 
        c:17,           // quotes not needed on simple field names
        "d":[1,2,3,4,5],
        "e":false,
        e1:true,
        e2:null,
        "f":-3,
        "g":2.3,
        "h":3e4, 
        "h1":-3E-4,
        i:""{
          A "raw string"
          with /multiple\ lines.
          }""
      }
      """
    val j = Json(s)
    val c = Compact(j)
    val j1 = Json(c)
    assert(j == j1, "same tree")
    assert(c == Compact(j1), "same string")

    val A = jfield("a")
    val B = jfield("b")
    val C = jfield("c")
    val F1 = jfield(1)
    val F2 = jfield(2)

    val a: Json = JsonArray(2, 3, "a")
    val m: Json = JsonObject("a" -> 3, "b" -> 4)
    val n = JsonObject("a" -> m, "c" -> a)

    assert(jgetInt(m, "a") == 3, "getInt")
    assert(jgetLong(m, "a") == 3L, "getLong")

    assert(jsize(a) == 3, "array size")
    assert(jget(a, 0) == 2, "a(0)")
    assert(jget(a, 1) == 3, "a(1)")
    assert(jget(a, 2) == "a", "a(2)")

    assert(jsize(m) == 2, "object size")
    assert(jget(m, "a") == 3, "m.a")
    assert(jget(m, "b") == 4, "m.a")

    assert(jgetString(a, 2) == "a", "get array string")
    assert(jgetInt(m, "a") == 3, "get object int")
    assert(jget(n, "c", 0) == 2, "multilevel get")

    n match {
      case A(B(a: Int)) & C(F2(b: String)) => {
        assert(a == 4 && b == "a", "pattern problem")
      }
      case x => assert(false, "pattern fail")
    }
  }

}