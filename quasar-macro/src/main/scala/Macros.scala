package com.ataraxer.quasar

import scala.reflect.macros.Context
import java.nio.ByteBuffer

import scala.language.experimental.macros


trait Serializable[T] {
  def put(buffer: ByteBuffer): Unit
  def get(buffer: ByteBuffer): T
}


object Serializable {
  implicit class RichBuffer(buffer: ByteBuffer) {
    def getString: String = {
      val size = buffer.getShort
      if (size > 0) {
        val bytes = new Array[Byte](size)
        buffer.get(bytes)
        new String(bytes, "UTF-8")
      } else {
        throw new Exception("empty string")
      }
    }

    def getSeq[T](implicit serializer: Serializable[T]): Seq[T] = {
      val size = buffer.getInt
      val result = for (_ <- 1 to size) yield getObject[T]
      result.toSeq
    }

    def getObject[T](implicit serializer: Serializable[T]): T = {
      serializer.get(buffer)
    }
  }


  implicit def materializeSerializable[T]: Serializable[T] =
    macro materializeSerializableImpl[T]


  def materializeSerializableImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val inputType = weakTypeOf[T]


    val getter = inputType match {
      /* ==== Primitives ==== */
      case t if t == typeOf[Short]  => q"buffer.getShort"
      case t if t == typeOf[Int]    => q"buffer.getInt"
      case t if t == typeOf[String] => q"buffer.getString"

      /* ==== Sequences ==== */
      case t if t <:< typeOf[Seq[Any]] => {
        val tParam = t.typeArgs.head
        q"buffer.getSeq[$tParam]"
      }

      /* ==== Case classes ==== */
      case t => {
        val declarations = t.declarations
        val constructor = declarations.collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m
        } get

        val fields = constructor.paramss.head

        val serializedParams = fields map { field =>
          //val fieldName = field.asTerm.name
          val fieldType = field.typeSignature
          q"buffer.getObject[$fieldType]"
        }

        val fieldIdents = fields map { field => q"$field" }
        q"new $inputType(..$serializedParams)"
      }
    }


    c.Expr[Serializable[T]] { q"""
      new Serializable[$inputType] {
        def put(buffer: ByteBuffer): Unit = ???
        def get(buffer: ByteBuffer): $inputType = { $getter }
      }
    """ }
  }
}


// vim: set ts=2 sw=2 et:
