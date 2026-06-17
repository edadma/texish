package io.github.edadma.texish.parser

import scala.scalajs.js

/** Reading a process environment variable, per platform. On JS this reads Node's `process.env`; in an
  * environment with no `process` (a browser), or for an unset name, it yields None. */
object PlatformEnv:
  def get(name: String): Option[String] =
    try
      val proc = js.Dynamic.global.process
      if js.isUndefined(proc) then None
      else
        val v = proc.env.selectDynamic(name)
        if js.isUndefined(v) || v == null then None else Some(v.toString)
    catch case _: Throwable => None
