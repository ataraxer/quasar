package com.ataraxer.quasar

import java.nio.ByteBuffer


case class Foo(ap: Int, b: Short)

object QuasarMain extends App {
  import Serializable._

  val buffer = ByteBuffer.allocate(4 + 2)
  buffer.putInt(9000)
  buffer.putShort(42)
  buffer.rewind

  val serializer = materializeSerializable[Foo]
  val foo = serializer.get(buffer)

  println(foo)
}


// vim: set ts=2 sw=2 et:
