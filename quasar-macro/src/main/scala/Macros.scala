package com.ataraxer.quasar

import scala.reflect.macros.blackbox.Context
import java.nio.ByteBuffer

import scala.language.experimental.macros


trait Serializable[T] {
  def put(buffer: ByteBuffer, value: T): Unit
  def get(buffer: ByteBuffer): T
  def sizeOf(value: T): Int
}


object Serializable {
  implicit class RichBuffer(buffer: ByteBuffer) {
    val Encoding = "UTF-8"

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

    def putString(string: String): Unit = {
      val stringBytes = string.getBytes(Encoding)
      buffer.putShort(stringBytes.size.toShort)
      buffer.put(stringBytes)
    }

    def read[T](implicit serializer: Serializable[T]): T = {
      serializer.get(buffer)
    }

    def write[T](value: T)(implicit serializer: Serializable[T]): Unit = {
      serializer.put(buffer, value)
    }
  }


  def sizeOf[T](value: T)(implicit serializer: Serializable[T]): Int = {
    serializer.sizeOf(value)
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


  private def generatePutter(inputType: Type, valueId: Tree): Tree = {
    inputType match {
      /* ==== Primitives ==== */
      case t if t <:< typeOf[Short]  => q"buffer.putShort($valueId)"
      case t if t <:< typeOf[Int]    => q"buffer.putInt($valueId)"
      case t if t <:< typeOf[String] => q"buffer.putString($valueId)"

      /* ==== Sequences ==== */
      case t if t <:< typeOf[Seq[Any]] => {
        val tParam = t.typeArgs.head
        val putter = generatePutter(tParam, q"item")
        q"""
        buffer.putInt($valueId.size)
        for (item <- $valueId) $putter
        """
      }

      /* ==== Case classes ==== */
      case t => {
        val fields = caseClassFields(inputType)

        val serializedParams = fields map { field =>
          val fieldName = field.asTerm.name
          val fieldType = field.typeSignature
          generatePutter(fieldType, q"$valueId.$fieldName")
        }

        q"..$serializedParams"
      }
    }
  }


  private def generateGetter(inputType: Type): Tree = {
    inputType match {
      /* ==== Primitives ==== */
      case t if t <:< typeOf[Short]  => q"buffer.getShort"
      case t if t <:< typeOf[Int]    => q"buffer.getInt"
      case t if t <:< typeOf[String] => q"buffer.getString"

      /* ==== Sequences ==== */
      case t if t <:< typeOf[Seq[Any]] => {
        val tParam = t.typeArgs.head
        val getter = generateGetter(tParam)
        q"""
        (for (_ <- 1 to buffer.getInt) yield $getter).toSeq
        """
      }

      /* ==== Case classes ==== */
      case t => {
        val fields = caseClassFields(inputType)

        val serializedParams = fields map { field =>
          val fieldType = field.typeSignature
          generateGetter(fieldType)
        }

        q"new $inputType(..$serializedParams)"
      }
    }
  }


  private def generateSizer(inputType: Type, valueId: Tree): Tree = {
    inputType match {
      /* ==== Primitives ==== */
      case t if t <:< typeOf[Short]  => q"2"
      case t if t <:< typeOf[Int]    => q"4"
      case t if t <:< typeOf[String] =>
        q"""2 + $valueId.getBytes("UTF-8").size"""

      /* ==== Sequences ==== */
      case t if t <:< typeOf[Seq[Any]] => {
        val tParam = t.typeArgs.head
        val sizer = generateSizer(tParam, q"item")
        q"""
        4 + (for (item <- $valueId) yield $sizer).sum
        """
      }

      /* ==== Case classes ==== */
      case t => {
        val fields = caseClassFields(inputType)

        val serializedParams = fields map { field =>
          val fieldName = field.asTerm.name
          val fieldType = field.typeSignature
          generateSizer(fieldType, q"$valueId.$fieldName")
        }

        q"List(..$serializedParams).sum"
      }
    }
  }


  def materializeSerializable[T: c.WeakTypeTag] = {
    val inputType = weakTypeOf[T]

    val getter = generateGetter(inputType)
    val putter = generatePutter(inputType, q"value")
    val sizer = generateSizer(inputType, q"value")

    c.Expr[Serializable[T]] { q"""
      new Serializable[$inputType] {
        def put(buffer: ByteBuffer, value: $inputType) = { $putter }
        def get(buffer: ByteBuffer) = { $getter }
        def sizeOf(value: $inputType) = { List(..$sizer).sum }
      }
    """ }
  }
}


// vim: set ts=2 sw=2 et:
