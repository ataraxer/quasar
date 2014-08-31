package com.ataraxer.quasar

import java.nio.ByteBuffer


case class Foo(name: String, replicas: Seq[Int])
case class Bar(name: String, foo: Foo)

object QuasarMain extends App {
  import Serializable._

  val fooName = "Sir Foo"
  val barName = "Mr. Bar"

  val buffer = ByteBuffer.allocate(
    2 + barName.size + // Bar.name
    2 + fooName.size + // Foo.name | Bar.foo
    4 + (4 * 3))       // Foo.replicas | Bar.foo

  val originalBar = Bar(barName, Foo(fooName, List(1, 2, 3)))
  buffer.write(originalBar)
  buffer.rewind

  val bar = buffer.read[Bar]

  println(bar)
}


// vim: set ts=2 sw=2 et:
