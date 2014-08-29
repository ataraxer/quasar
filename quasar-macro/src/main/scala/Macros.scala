package com.ataraxer.quasar

import scala.reflect.macros.Context
import java.nio.ByteBuffer

import scala.language.experimental.macros


trait Serializable[T] {
  def put(buffer: ByteBuffer): Unit
  def get(buffer: ByteBuffer): T
}


object Serializable {
  implicit def materializeSerializable[T]: Serializable[T] =
    macro materializeSerializableImpl[T]


  def materializeSerializableImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val inputType = weakTypeOf[T]
    val declarations = inputType.declarations
    val constructor = declarations.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    } get

    val fields = constructor.paramss.head

    val serializedParams = fields map { field =>
      val fieldName = field.asTerm.name
      val fieldType = field.typeSignature

      fieldType match {
        case t if t == typeOf[Short] =>
          q"buffer.getShort"
        case t if t == typeOf[Int] =>
          q"buffer.getInt"
        case _ =>
          q"???"
      }
    }

    val fieldIdents = fields map { field => q"$field" }

    val getter = q"new $inputType(..$serializedParams)"

    c.Expr[Serializable[T]] { q"""
      new Serializable[$inputType] {
        def put(buffer: ByteBuffer): Unit = ???
        def get(buffer: ByteBuffer): $inputType = { $getter }
      }
    """ }
  }
}


// vim: set ts=2 sw=2 et:
