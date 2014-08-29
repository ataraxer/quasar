package com.ataraxer.quasar

import java.nio.ByteBuffer


case class Foo(name: String, replicas: Seq[Int])
case class Bar(name: String, foo: Foo)

object QuasarMain extends App {
  import Serializable._

  val fooName = "Sir Foo".getBytes("UTF-8")
  val barName = "Mr. Bar".getBytes("UTF-8")

  val buffer = ByteBuffer.allocate(
    2 + barName.size + // Bar.name
    2 + fooName.size + // Foo.name | Bar.foo
    4 + (4 * 3))       // Foo.replicas | Bar.foo

  buffer.putShort(barName.size.toShort)
  buffer.put(barName)
  buffer.putShort(fooName.size.toShort)
  buffer.put(fooName)
  buffer.putInt(3)
  buffer.putInt(9000)
  buffer.putInt(42)
  buffer.putInt(-1)
  buffer.rewind

  val bar = buffer.getObject[Bar]

  println(bar)
}


// vim: set ts=2 sw=2 et:
