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

import JsonOps._
import Exceptions._
import scala.collection.immutable.HashMap

/**
 * This object has methods that use reflection for converting between
 * Json and user defined case classes. Here is an example:
 *
 * {{{
 * import com.persist.JsonOps._
 * import com.persist.JsonMapper._
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
  private[this] val infos = new collection.mutable.HashMap[java.lang.Class[_], ClassInfo]() // access must be synchronized

  private def box(j: Json): AnyRef = {
    j match {
      case s: Short => new java.lang.Short(s)
      case i: Int => new java.lang.Integer(i)
      case l: Long => new java.lang.Long(l)
      case b: Boolean => new java.lang.Boolean(b)
      case d: Double => new java.lang.Double(d)
      case a: AnyRef => a
    }
  }

  private class ClassInfo(clazz: java.lang.Class[_]) {
    // Note: works only on simple case classes
    private[this] val name = clazz.getName()
    private[this] val constructors = clazz.getConstructors()
    private[this] val constructor = constructors(0)
    private[this] val types: List[java.lang.Class[_]] = constructor.getParameterTypes().toList

    def getTypes = types

    private[this] val gtypes = constructor.getGenericParameterTypes()
    private[this] val names: List[String] = clazz.getDeclaredFields().map {
      case x =>
        val name = x.getName()
        // Remove _ from start of constructor parameter names
        // Escape for reserved words (e.g. _type matches Json field "type")
        if (name.startsWith("_")) name.substring(1) else name
    }.toList

    def getNames = names

    def vals(x: Any): List[Any] = names.map(clazz.getDeclaredMethod(_).invoke(x))

    def apply(vals: List[AnyRef]) = constructor.newInstance(vals: _*)

    private[this] val elemTypes: List[(java.lang.Class[_], java.lang.Class[_])] = {
      gtypes.map(gt => {
        gt match {
          case t: java.lang.reflect.ParameterizedType => {
            val t1 = t.getActualTypeArguments()
            if (t1.size == 1) {
              t1(0) match {
                case t2: java.lang.reflect.ParameterizedType =>
                  val t3 = t2.getActualTypeArguments
                  val t4 = t2.getRawType
                  (t4.asInstanceOf[java.lang.Class[_]], t3(0).asInstanceOf[java.lang.Class[_]])
                case t10 => (t10.asInstanceOf[java.lang.Class[_]], null)
              }
            } else {
              (null, null)
            }
          }
          case x =>
            (null, null)
        }
      }).toList
    }
    private[this] val nte = (names zip (types zip elemTypes))

    def getNTE = nte

  }


  private def getClassInfo(clazz: java.lang.Class[_]): ClassInfo = {
    infos.synchronized(infos.get(clazz)) match {
      case Some(ci: ClassInfo) => ci
      case None => {
        val ci = new ClassInfo(clazz)
        infos.synchronized {
          infos += (clazz -> ci)
        }
        ci
      }
    }
  }

  private def toJson(clazz: java.lang.Class[_], x: Any): Json = {
    try {
      x match {
        case s: Short => s
        case i: Int => i
        case l: Long => l
        case s: String => s
        case b: Boolean => b
        case d: Double => d
        case bd: BigDecimal => bd
        case seq: Seq[_] => seq map (v => toJson(v.getClass(), v))
        case option: Option[_] => {
          x match {
            case Some(v) => toJson(v.getClass(), v)
            case None => jnull
          }
        }
        case map: Map[_, _] => map
        case obj => {
          val ci = getClassInfo(clazz)
          val args = (ci.vals(x) zip ci.getTypes) map {
            case (v, clazz1) => toJson(clazz1, v)
          }
          (ci.getNames zip args).filter {
            case (n, v) => v != null
          }.toMap
        }
      }
    } catch {
      case se: SystemException => throw se
      case ex: Throwable =>
        throw new SystemException("JsonMapper", JsonObject("from" -> clazz.getName(), "ex" -> ex.toString))
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

  private def toObject(clazz: java.lang.Class[_],
                       elemClazz: java.lang.Class[_],
                       elemClazz1: java.lang.Class[_],
                       j: Json): AnyRef = {
    try {
      if (clazz == classOf[java.lang.Object]) return box(j)
      if (clazz == classOf[Map[String, _]]) return jgetObject(j)
      if (clazz == classOf[Option[_]]) return if (j == null) None else Some(toObject(elemClazz, elemClazz1, null, j))
      j match {
        case s: String => s
        case s: Short => new java.lang.Short(s)
        case i: Int => new java.lang.Integer(i)
        case l: Long => new java.lang.Long(l)
        case b: Boolean => new java.lang.Boolean(b)
        case d: Double => new java.lang.Double(d)
        case bd: BigDecimal => bd
        case arr: JsonArray => {
          //arr map (v => {
          //  toObject(elemClazz, elemClazz1, null, v)
          //})
          arr.zipWithIndex map ((vi) => {
            vi match {
              case (v, i) =>
                try {
                  toObject(elemClazz, elemClazz1, null, v)
                } catch {
                  case ex: SystemException =>
                    val j = jgetObject(ex.info)
                    val path = i + "/" + jgetString(j, "to")
                    val j1 = j + ("to" -> path)
                    throw new SystemException(ex.kind, j1)
                }
            }
          })
        }
        case obj: JsonObject => {
          val ci = getClassInfo(clazz)
          val args = ci.getNTE map {
            case (name, (clazz1, (elemClazz, elemClazz1))) => {
              //val v = toObject(clazz1, elemClazz, elemClazz1, jget(j, name))
              //v
              try {
                toObject(clazz1, elemClazz, elemClazz1, jget(j, name))
              } catch {
                case ex: SystemException =>
                  val j = jgetObject(ex.info)
                  val path = name + "/" + jgetString(j, "to")
                  val j1 = j + ("to" -> path)
                  throw new SystemException(ex.kind, j1)
              }
            }
          }
          val x = ci.apply(args).asInstanceOf[AnyRef]
          x
        }
      }
    } catch {
      case se: SystemException => throw se
      case ex: Throwable => throw new SystemException("JsonMapper", JsonObject("from" -> j, "to" -> clazz.getName()))
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
    val x = toObject(m.erasure, t, null, j)
    x.asInstanceOf[T]
  }

}
