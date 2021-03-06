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


  def encode[T](value: T)(implicit serializer: Serializable[T]): ByteBuffer = {
    val byteSize = sizeOf(value)
    val buffer = ByteBuffer.allocate(byteSize)
    buffer.write(value)
    buffer.rewind
    buffer
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


  private def generatePutter(inputType: Type): Tree = {
    def gen(inputType: Type, valueId: Tree): List[Tree] = {
      inputType match {
        /* ==== Primitives ==== */
        case t if t <:< typeOf[Short]  => List(q"buffer.putShort($valueId)")
        case t if t <:< typeOf[Int]    => List(q"buffer.putInt($valueId)")
        case t if t <:< typeOf[String] => List(q"buffer.putString($valueId)")

        /* ==== Sequences ==== */
        case t if t <:< typeOf[Seq[Any]] => {
          val tParam = t.typeArgs.head
          val putter = gen(tParam, q"item")
          List(
            q"buffer.putInt($valueId.size)",
            q"for (item <- $valueId) { ..$putter }")
        }

        /* ==== Case classes ==== */
        case t => {
          val fields = caseClassFields(inputType)

          fields flatMap { field =>
            val fieldName = field.asTerm.name
            val fieldType = field.typeSignature
            gen(fieldType, q"$valueId.$fieldName")
          }
        }
      }
    }

    val subTrees = gen(inputType, q"value")

    q"..$subTrees"
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


  private def generateSizer(inputType: Type, valueId: Tree = q"value"): Tree = {
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
        q"4 + (for (item <- $valueId) yield $sizer).sum"
      }

      /* ==== Case classes ==== */
      case t => {
        val fields = caseClassFields(inputType)

        fields map { field =>
          val fieldName = field.asTerm.name
          val fieldType = field.typeSignature
          generateSizer(fieldType, q"$valueId.$fieldName")
        } reduce { (agg, item) =>
          q"$agg + $item"
        }
      }
    }
  }


  def materializeSerializable[T: c.WeakTypeTag] = {
    val inputType = weakTypeOf[T]

    val getter = generateGetter(inputType)
    val putter = generatePutter(inputType)
    val sizer = generateSizer(inputType)

    c.Expr[Serializable[T]] { q"""
      new Serializable[$inputType] {
        def put(buffer: ByteBuffer, value: $inputType) = $putter
        def get(buffer: ByteBuffer) = $getter
        def sizeOf(value: $inputType) = $sizer
      }
    """ }
  }
}


// vim: set ts=2 sw=2 et:
