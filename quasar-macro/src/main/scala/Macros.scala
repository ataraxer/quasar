package com.ataraxer.quasar

import scala.reflect.macros.blackbox.Context
import java.nio.ByteBuffer

import scala.language.experimental.macros


trait Serializable[T] {
  def put(buffer: ByteBuffer, value: T): Unit
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
      val result = for (_ <- 1 to size) yield read[T]
      result.toSeq
    }

    def read[T](implicit serializer: Serializable[T]): T = {
      serializer.get(buffer)
    }
  }


  implicit def materializeSerializable[T]: Serializable[T] =
    macro SerializableImpl.materializeSerializable[T]
}


class SerializableImpl(val c: Context) {
  import c.universe._

  private def caseClassFields(caseClass: Type) = {
    val declarations = caseClass.declarations
    val constructor = declarations.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    } get

    constructor.paramss.head
  }


  def materializeSerializable[T: c.WeakTypeTag] = {
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
        val fields = caseClassFields(inputType)

        val serializedParams = fields map { field =>
          val fieldType = field.typeSignature
          q"buffer.read[$fieldType]"
        }

        q"new $inputType(..$serializedParams)"
      }
    }


    c.Expr[Serializable[T]] { q"""
      new Serializable[$inputType] {
        def put(buffer: ByteBuffer, value: $inputType) = ???
        def get(buffer: ByteBuffer) = { $getter }
      }
    """ }
  }
}


// vim: set ts=2 sw=2 et:
