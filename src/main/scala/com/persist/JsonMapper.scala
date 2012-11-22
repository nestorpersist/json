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

import JsonOps._
import Exceptions._

/**
 * This object has methods that use reflection for converting between
 * Json and user defined case classes. Here is an example:
 *
 *  {{{
 *  import com.persist.JsonOps._
 *  import com.persist.JsonMapper._
 *  
 *  case class Person(name:String, age:Option[Int])
 *  case class Group(city: String, people:Seq[Person], var cnt:Int, props:JsonObject)
 *  
 *  val j:Json = Json("""{city:"Seattle", cnt:2, props:{i:1, j:2},
 *                        people:[{name:"Joe"},
 *                                {name:"Tom", age:20}]
 *                       }""")
 *         
 *  val group:Group = ToObject[Group](j)
 *  
 *  val j1:Json = ToJson(group)
 *  
 *  assert(j1 == j)
 *  }}}
 *  
 */
object JsonMapper {

  private def box(j: Json): AnyRef = {
    j match {
      case i: Int => new java.lang.Integer(i)
      case l: Long => new java.lang.Long(l)
      case b: Boolean => new java.lang.Boolean(b)
      case d: Double => new java.lang.Double(d)
      case a: AnyRef => a
    }
  }

  private class ClassInfo(clazz: java.lang.Class[_]) {
    // Note: works only on simple case classes
    // TODO cache instances (thread safe)
    val name = clazz.getName()
    private val constructors = clazz.getConstructors()
    private val constructor = constructors(0)
    val types: List[java.lang.Class[_]] = constructor.getParameterTypes().toList
    val names: List[String] = clazz.getDeclaredFields().map(_.getName()).toList
    def vals(x: Any): List[Any] = names.map(clazz.getDeclaredMethod(_).invoke(x))
    def apply(vals: List[AnyRef]) = constructor.newInstance(vals: _*)
    def elemTypes: List[java.lang.Class[_]] = {
      constructor.getGenericParameterTypes().map(gt => {
        gt match {
          case t: java.lang.reflect.ParameterizedType => {
            val t1 = t.getActualTypeArguments()
            if (t1.size == 1) {
              t1(0).asInstanceOf[java.lang.Class[_]]
            } else {
              null
            }
          }
          case x => null
        }
      }).toList
    }
  }

  private def toJson(clazz: java.lang.Class[_], x: Any): Json = {
    try {
      x match {
        case i: Int => i
        case l: Long => l
        case s: String => s
        case b: Boolean => b
        case d: Double => d
        case bd: BigDecimal => bd
        case seq: Seq[_] => seq map (v => toJson(v.getClass(), v))
        case option: Option[_] => {
          x match {
            case Some(v) => v
            case None => null
          }
        }
        case map: Map[String, _] => map
        case obj => {
          val ci = new ClassInfo(clazz)
          val args = (ci.vals(x) zip ci.types) map { case (v, clazz1) => toJson(clazz1, v) }
          (ci.names zip args).filter { case (n, v) => v != null }.toMap
        }
      }
    } catch {
      case se: SystemException => throw se
      case ex => throw new SystemException("JsonMapper", JsonObject("from" -> clazz.getName()))
    }
  }

  /**
   * Converts an object to Json.
   * 
   * @param x the object.
   * @return the Json form for the object.
   * 
   */
  def ToJson[T](x: T)(implicit m: ClassManifest[T]): Json = {
    toJson(m.erasure, x)
  }

  private def toObject(clazz: java.lang.Class[_], elemClazz: java.lang.Class[_], j: Json): AnyRef = {
    try {
      if (clazz == classOf[java.lang.Object]) return box(j)
      if (clazz == classOf[Map[String, _]]) return jgetObject(j)
      if (clazz == classOf[Option[_]]) return if (j == null) None else Some(toObject(elemClazz, null, j))
      j match {
        case s: String => s
        case i: Int => new java.lang.Integer(i)
        case l: Long => new java.lang.Long(l)
        case b: Boolean => new java.lang.Boolean(b)
        case d: Double => new java.lang.Double(d)
        case bd: BigDecimal => bd
        case arr: JsonArray => {
          val clazz1 = clazz.getTypeParameters()(0)
          arr map (v => toObject(elemClazz, null, v))
        }
        case obj: JsonObject => {
          val ci = new ClassInfo(clazz)
          val args = (ci.names zip (ci.types zip ci.elemTypes)) map {
            case (name, (clazz1, elemClazz)) => {
              toObject(clazz1, elemClazz, jget(j, name))
            }
          }
          val x = ci.apply(args).asInstanceOf[AnyRef]
          x
        }
      }
    } catch {
      case se: SystemException => throw se
      case ex => throw new SystemException("JsonMapper", JsonObject("from" -> j, "to" -> clazz.getName()))
    }
  }

  /**
   * Converts Json to an object of a specified type.
   * 
   * @tparam T the type for the result.
   * @param j the Json to convert.
   * @return the produced object.
   */
  def ToObject[T](j: Json)(implicit m: ClassManifest[T]): T = {
    val ta = m.typeArguments
    val t = if (ta.size == 1) ta.head.asInstanceOf[ClassManifest[_]].erasure else null
    val x = toObject(m.erasure, t, j)
    x.asInstanceOf[T]
  }

}