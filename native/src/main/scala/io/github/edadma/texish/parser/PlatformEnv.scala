package io.github.edadma.texish.parser

import scala.scalanative.unsafe.{Zone, toCString, fromCString}
import scala.scalanative.posix.stdlib.getenv

/** Reading a process environment variable, per platform. On Native this calls libc `getenv` directly,
  * which reads the live environment — unlike `System.getenv`, whose backing map is a startup snapshot,
  * so a variable a host sets after start (e.g. TEXISHHOME) would be invisible through it. */
object PlatformEnv:
  def get(name: String): Option[String] =
    Zone:
      val p = getenv(toCString(name))
      if p == null then None else Some(fromCString(p))
