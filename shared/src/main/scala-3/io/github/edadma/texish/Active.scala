package io.github.edadma.texish

import io.github.edadma.char_reader.CharReader

abstract class Active(val name: String) extends ((CharReader, Renderer) => Any) {
  override def toString = s"""active character: '$name'"""
}
