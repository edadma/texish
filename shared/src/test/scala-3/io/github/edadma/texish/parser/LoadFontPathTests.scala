package io.github.edadma.texish.parser

import io.github.edadma.path.Path
import io.github.edadma.texish.{HeadlessTypesetter, TexishException}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `\loadfont{name}{path}` resolves a relative path against the directory of the file doing the loading before the
  * current directory, the same order in which `\use` resolves a module. Without that root a font kept beside the
  * document is only found when the engine happens to be run from the document's directory — true of the CLI, false
  * of a host that embeds the engine and is launched from anywhere else.
  *
  * The headless typesetter's font face *is* the path it resolved, so these read the resolution directly.
  */
class LoadFontPathTests extends AnyFreeSpec with Matchers:

  /** A headless typesetter that remembers every path it was handed, after resolution. The buffer is lazy
    * because Typesetter registers its bundled faces from its own constructor — before a subclass field would
    * have been initialised. */
  private class Recording extends HeadlessTypesetter:
    lazy val loaded = scala.collection.mutable.ArrayBuffer[String]()
    override def loadFont(path: String): FontFace =
      loaded += path
      super.loadFont(path)

  /** Run `src` with the document's base directory set to `dir`, and report the path `\loadfont` resolved to.
    * The engine registers its own bundled faces while constructing, so only paths loaded by the source itself
    * are of interest — the ones after construction settles. */
  private def resolveIn(dir: String, src: String): String =
    val t       = new Recording
    val before  = t.loaded.length
    val handler = new TypesetterHandler(t)
    val proc    = new Processor(handler)
    registerTypesettingPrimitives(proc, handler)
    proc.setBaseDir(dir)
    Console.withOut(new java.io.ByteArrayOutputStream)(proc.process(src))
    t.loaded.drop(before).lastOption.getOrElse(fail("\\loadfont loaded nothing"))

  /** A fresh temp directory holding the named files, cleaned up afterward. The contents never matter — the
    * headless typesetter does not parse a face, and resolution only probes for existence. */
  private def withFiles(names: String*)(body: Path => Unit): Unit =
    val dir = Path.createTempDirectory("texish-loadfont-")
    val written = names.map { name =>
      val p = dir / name
      p.writeText("not a real font")
      p
    }
    try body(dir)
    finally
      written.foreach(p => try p.delete() catch { case _: Exception => () })
      try dir.delete() catch { case _: Exception => () }

  "a font beside the document is found when the engine runs from elsewhere" in {
    withFiles("MyFace.ttf") { dir =>
      resolveIn(dir.toPlatformString, "\\loadfont{mine}{MyFace.ttf}") shouldBe (dir / "MyFace.ttf").toPlatformString
    }
  }

  "a subdirectory of the document's directory resolves too" in {
    withFiles("MyFace.ttf") { dir =>
      val sub = dir / "faces"
      sub.createDirectories()
      val face = sub / "Nested.ttf"
      face.writeText("not a real font")
      try
        resolveIn(dir.toPlatformString, "\\loadfont{mine}{faces/Nested.ttf}") shouldBe face.toPlatformString
      finally
        try face.delete() catch { case _: Exception => () }
        try sub.delete() catch { case _: Exception => () }
    }
  }

  "an absolute path is used exactly as written" in {
    withFiles("MyFace.ttf") { dir =>
      val abs   = (dir / "MyFace.ttf").toAbsolutePath.toPlatformString
      resolveIn(".", s"\\loadfont{mine}{$abs}") shouldBe abs
    }
  }

  // A font the document named but that no root has is a mistake in the document, and saying so at the \loadfont
  // beats registering a face that fails later — or, on a backend that tolerates a bad path, drawing tofu.
  "a path that resolves nowhere is an error naming the path" in {
    withFiles("MyFace.ttf") { dir =>
      val thrown = the[TexishException] thrownBy resolveIn(dir.toPlatformString, "\\loadfont{mine}{NoSuchFace.ttf}")
      thrown.getMessage should include("NoSuchFace.ttf")
    }
  }
