package com.ataraxer.quasar

import java.nio.ByteBuffer


case class Foo(a: Int, s: String)

object QuasarMain extends App {
  import Serializable._

  val string = "Freaking awesome!".getBytes("UTF-8")
  val buffer = ByteBuffer.allocate(4 + 2 + string.size)
  buffer.putInt(9000)
  buffer.putShort(string.size.toShort)
  buffer.put(string)
  buffer.rewind

  val serializer = materializeSerializable[Foo]
  val foo = serializer.get(buffer)

  println(foo)
}


// vim: set ts=2 sw=2 et:
