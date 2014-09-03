package com.ataraxer.quasar

import java.nio.ByteBuffer


case class Foo(name: String, replicas: Seq[Int])
case class Bar(name: String, foo: Foo)

object QuasarMain extends App {
  import Serializable._

  val fooName = "Sir Foo"
  val barName = "Mr. Bar"

  val foo = Foo("Sir Foo", List(9000, 42, 1337))
  val bar = Bar("Mr. Bar", foo)

  val encodedBar = encode(bar)
  val decodedBar = encodedBar.read[Bar]

  println(decodedBar)
  println(bar == decodedBar)
}


// vim: set ts=2 sw=2 et:
