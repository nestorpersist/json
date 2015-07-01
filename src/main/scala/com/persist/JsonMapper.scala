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

import JsonOps._
//import Exceptions._
import com.persist.json.{WriteCodec, ReadCodec}
//import scala.collection.immutable.HashMap

/**
 * This object has methods that use a collection of codecs for converting between
 * Json and user defined case classes. For case classes, Shapeless is used.
 * Here is an example:
 *
 * {{{
 * import com.persist.JsonOps._
 * import com.persist.JsonMapper._
 * import com.persist.json._   // brings in implicit codecs!
 *
 * case class Person(name:String, age:Option[Int])
 * case class Group(city: String, people:Seq[Person], var cnt:Int, props:JsonObject)
 *
 * val j:Json = Json("""{city:"Seattle", cnt:2, props:{i:1, j:2},
 *                       people:[{name:"Joe"},
 *                               {name:"Tom", age:20}]
 *                      }""")
 *
 * val group:Group = ToObject[Group](j)
 *
 * val j1:Json = ToJson(group)
 *
 * assert(j1 == j)
 * }}}
 *
 */
object JsonMapper {

  /**
   * Converts Json to an object of a specified type.
   *
   * @tparam T the type for the result.
   * @param j the Json to convert.
   * @return the produced object.
   */
  def ToObject[T](j: Json)(implicit codec: ReadCodec[T]): T = codec.read(j)

  /**
   * Converts an object to Json.
   *
   * @param x the object.
   * @return the Json form for the object.
   *
   */
  def ToJson[T](x: T)(implicit codec: WriteCodec[T]): Json = codec.write(x)

}
