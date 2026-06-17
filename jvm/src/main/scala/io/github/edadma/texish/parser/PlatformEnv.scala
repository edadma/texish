package io.github.edadma.texish.parser

/** Reading a process environment variable, per platform. On the JVM `System.getenv` is live. */
object PlatformEnv:
  def get(name: String): Option[String] = Option(System.getenv(name))
