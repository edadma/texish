import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.4"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.25.0"
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

ThisBuild / sonatypeProfileName := "io.github.edadma"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/texish"),
    "scm:git@github.com:edadma/texish.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage    := Some(url("https://github.com/edadma/texish"))
ThisBuild / description := "A TeX-style document layout and PDF rendering engine for Scala"

ThisBuild / publishTo := sonatypePublishToBundle.value

// Scala.js is not in the cross-build. The browser backends (`js/`) are kept on disk and in git — in-browser
// rendering is still wanted for documentation someday — but they are not compiled, published or tested, and the
// effort of keeping a third platform green is not currently paying for itself. Images cover the documentation
// need meanwhile. To bring it back: add JSPlatform here and `texish.js` to the root aggregate, and restore the
// .jsSettings block (jsEnv, ESModule linker config, scalajs-dom). The font and package embedding the browser
// needs is no longer JS-specific — it is how every platform loads them (see EmbeddedFonts).
lazy val texish = crossProject(JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "texish",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    organization := "io.github.edadma",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "char_reader"    % "0.1.29",
      "io.github.edadma" %%% "cross_platform" % "0.1.7",
      "io.github.edadma" %%% "path"           % "0.0.6",
      "io.github.edadma" %%% "highlighter"    % "0.0.10",
      "io.github.edadma" %%% "qr"             % "0.0.1",
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "pprint" % "0.9.0" % "test",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
    // Scaladoc on the Scala.js / Scala Native back ends is handed the platform's compiler-plugin flag, which the
    // doc tool does not accept ("Setting -Xplugin is currently not supported"). The plugin is only needed to
    // compile, not to build docs from TASTy, so drop it from the doc invocation to keep the doc build clean.
    Compile / doc / scalacOptions ~= { _.filterNot(_.startsWith("-Xplugin")) },
    // Embed every TeX hyphenation pattern file (shared/src/main/resources/hyph-*.tex) as a Scala
    // constant at build time, keyed by language tag (hyph-en-us.tex -> "en-us"), so they ship compiled
    // into every target — a native binary has no pattern file to read at runtime. Hyphenation reaches
    // them through EmbeddedHyphenationPatterns.byTag. Each string is escaped so any content is safe.
    Compile / sourceGenerators += Def.task {
      val root  = (LocalRootProject / baseDirectory).value
      val resir = root / "shared" / "src" / "main" / "resources"
      val files = (resir * "hyph-*.tex").get.sortBy(_.getName)
      val out   = (Compile / sourceManaged).value / "io" / "github" / "edadma" / "texish" / "EmbeddedHyphenationPatterns.scala"
      val entries = files.map { f =>
        val tag     = f.getName.stripPrefix("hyph-").stripSuffix(".tex")
        val escaped = IO.read(f).replace("\\", "\\\\").replace("\"", "\\\"").replace("\r", "").replace("\n", "\\n")
        "    \"" + tag + "\" -> \"" + escaped + "\""
      }
      IO.write(
        out,
        "package io.github.edadma.texish\n\n" +
          "// Generated at build time from shared/src/main/resources/hyph-*.tex — do not edit.\n" +
          "private[texish] object EmbeddedHyphenationPatterns:\n" +
          "  val byTag: Map[String, String] = Map(\n" + entries.mkString(",\n") + "\n  )\n",
      )
      Seq(out)
    }.taskValue,
    // Embed a small set of `packages/*.texish` modules as Scala constants at build time, so they ship compiled
    // into every target. The module loader consults these as a fallback after its filesystem search, which lets
    // a host with no package directory on disk resolve them, while a local package file still shadows the
    // embedded one wherever the filesystem search finds it first. Each module's source is split into chunks
    // small enough for the compiler and rejoined at runtime.
    //
    // This is a whitelist, not everything in packages/, and the bar is what a document needs to be an ordinary
    // document: `base` and the `document` format it builds on. The set is closed under module inclusion, since
    // document includes base and base includes nothing.
    //
    // Everything else — the diagram family, plot, book, usfm, music, … — resolves from a packages/ folder on
    // disk. That is where a package with its own font requirements belongs anyway: `music` needs a SMuFL face
    // and those are catalogue fonts, so embedding it would ship a module that resolves and then cannot draw a
    // note. A package is embedded only if it can work from the embed alone, and only if it is basic enough to
    // be worth the weight in every artifact.
    Compile / sourceGenerators += Def.task {
      val root   = (LocalRootProject / baseDirectory).value
      val pkgDir = root / "packages"
      val out =
        (Compile / sourceManaged).value / "io" / "github" / "edadma" / "texish" / "EmbeddedPackages.scala"
      def esc(s: String): String =
        s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\r", "").replace("\n", "\\n")
      val embedded = Set("base", "document")
      val files    = (pkgDir * "*.texish").get.filter(f => embedded(f.getName.stripSuffix(".texish"))).sortBy(_.getName)
      val sb    = new StringBuilder
      sb.append("package io.github.edadma.texish\n\n")
      sb.append("// Generated at build time from the packages/ directory — do not edit.\n")
      sb.append("private[texish] object EmbeddedPackages {\n")
      sb.append("  val sources: Map[String, Array[String]] = Map(\n")
      for (f <- files) {
        val name   = f.getName.stripSuffix(".texish")
        val chunks = IO.read(f).grouped(8000).toSeq
        sb.append("    \"").append(name).append("\" -> Array(\n")
        for (c <- chunks) sb.append("      \"").append(esc(c)).append("\",\n")
        sb.append("    ),\n")
      }
      sb.append("  )\n")
      sb.append("}\n")
      IO.write(out, sb.toString)
      Seq(out)
    }.taskValue,
    // Embed the core faces as base64 in a generated Scala source, so a consumer that adds texish as a dependency
    // gets a working engine with no font tree on disk and nothing to configure — a browser has no filesystem at
    // all, and a Native binary has no resource loading, so the bytes must be compiled in. Only the core ships
    // this way: the full bundled set is ~151MB against this curated stack's ~5.8MB. It is exactly the set
    // Typesetter.loadCoreFonts registers, and that method is the engine's guaranteed baseline — the body, math,
    // glyph-fallback and code faces every host has. The wider set is opt-in (loadBundledCatalogue) and comes
    // from a font tree on disk. The path list MUST stay in sync with loadCoreFonts: a path here that nothing
    // loads is dead weight in every artifact, and a core load missing from here is a face that vanishes wherever
    // there is no font tree. EmbeddedCoreTests asserts both directions.
    Compile / sourceGenerators += Def.task {
      val root = (LocalRootProject / baseDirectory).value
      val out =
        (Compile / sourceManaged).value / "io" / "github" / "edadma" / "texish" / "EmbeddedFontData.scala"
      val fontPaths = Seq(
        "fonts/LatinModernRoman/lmroman10-regular.otf",
        "fonts/LatinModernRoman/lmroman10-bold.otf",
        "fonts/LatinModernRoman/lmroman10-italic.otf",
        "fonts/LatinModernRoman/lmroman10-bolditalic.otf",
        "fonts/LatinModernRoman/lmromanslant10-regular.otf",
        "fonts/LatinModernRoman/lmromancaps10-regular.otf",
        "fonts/LatinModernRoman/lmromancaps10-oblique.otf",
        "fonts/LatinModernMono/lmmono10-regular.otf",
        "fonts/LatinModernMono/lmmonolt10-bold.otf",
        "fonts/LatinModernMono/lmmono10-italic.otf",
        "fonts/LatinModernMono/lmmonolt10-boldoblique.otf",
        "fonts/LatinModernSans/lmsans10-regular.otf",
        "fonts/LatinModernSans/lmsans10-bold.otf",
        "fonts/LatinModernSans/lmsans10-oblique.otf",
        "fonts/LatinModernSans/lmsans10-boldoblique.otf",
        "fonts/LatinModernMath/LatinModernMath-SMaFL.otf",
        // New Computer Modern, the glyph-fallback face. All four cuts, so a substituted run keeps the weight and
        // slope of the text around it rather than dropping to regular inside a bold heading.
        "fonts/NewComputerModern/NewCM10-Regular.otf",
        "fonts/NewComputerModern/NewCM10-Bold.otf",
        "fonts/NewComputerModern/NewCM10-Italic.otf",
        "fonts/NewComputerModern/NewCM10-BoldItalic.otf",
        // JetBrains Mono, the face \code sets a listing in. \code is a primitive whose TextMate grammars are
        // compiled in, so the face has to be too. Two cuts only; the rest of the weight range is catalogue.
        "fonts/JetBrainsMono/static/JetBrainsMono-Regular.ttf",
        "fonts/JetBrainsMono/static/JetBrainsMono-Bold.ttf",
      )
      val enc = java.util.Base64.getEncoder
      val sb  = new StringBuilder
      sb.append("package io.github.edadma.texish\n\n")
      sb.append("// Generated at build time from the bundled font files — do not edit.\n")
      sb.append("// Base64 of each embedded font, split into chunks small enough for the compiler.\n")
      sb.append("private[texish] object EmbeddedFontData:\n")
      sb.append("  val chunks: Map[String, Array[String]] = Map(\n")
      for (p <- fontPaths) {
        val b64   = enc.encodeToString(IO.readBytes(root / p))
        val parts = b64.grouped(32000).toSeq // 32000 is a multiple of 4 — chunks split on base64 boundaries
        sb.append("    \"").append(p).append("\" -> Array(\n")
        for (part <- parts) sb.append("      \"").append(part).append("\",\n")
        sb.append("    ),\n")
      }
      sb.append("  )\n")
      IO.write(out, sb.toString)
      Seq(out)
    }.taskValue,
    // Embed the bundled TextMate grammars (grammars/*.tmLanguage.json) as Scala constants at build time, so the
    // \code highlighter can resolve a language with no grammar file on disk — chiefly in the browser. A local
    // grammar file may still shadow the embedded one. Each grammar's JSON is split into chunks small enough for
    // the compiler and rejoined at runtime.
    Compile / sourceGenerators += Def.task {
      val root    = (LocalRootProject / baseDirectory).value
      val grmDir  = root / "grammars"
      val out =
        (Compile / sourceManaged).value / "io" / "github" / "edadma" / "texish" / "EmbeddedGrammars.scala"
      def esc(s: String): String =
        s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\r", "").replace("\n", "\\n")
      val files = (grmDir * "*.tmLanguage.json").get.sortBy(_.getName)
      val sb    = new StringBuilder
      sb.append("package io.github.edadma.texish\n\n")
      sb.append("// Generated at build time from the grammars/ directory — do not edit.\n")
      sb.append("private[texish] object EmbeddedGrammars {\n")
      sb.append("  val sources: Map[String, Array[String]] = Map(\n")
      for (f <- files) {
        val name   = f.getName.stripSuffix(".tmLanguage.json")
        val chunks = IO.read(f).grouped(8000).toSeq
        sb.append("    \"").append(name).append("\" -> Array(\n")
        for (c <- chunks) sb.append("      \"").append(esc(c)).append("\",\n")
        sb.append("    ),\n")
      }
      sb.append("  )\n")
      sb.append("}\n")
      IO.write(out, sb.toString)
      Seq(out)
    }.taskValue,
  )
  .jvmSettings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0" % "test",
  )
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "libcairo"  % "0.0.8",
      "io.github.edadma" %%% "freetype"  % "0.0.7",
      "io.github.edadma" %%% "turbojpeg" % "0.0.1",
    ),
  )

// The command-line tool is a native-only application, kept out of the published `texish` library so a library
// consumer never inherits the CLI's `@main` entry point or its scopt dependency. It depends on the native
// library for the engine and the Cairo PDF/PNG backends.
lazy val texishCli = project
  .in(file("cli"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(texish.native)
  .settings(
    name := "texish-cli",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
    libraryDependencies += "com.github.scopt" %%% "scopt"   % "4.1.0",
    publish / skip      := true,
    publishLocal / skip := true,
  )

lazy val root = project
  .in(file("."))
  .aggregate(texish.jvm, texish.native, texishCli)
  .settings(
    name                := "texish",
    publish / skip      := true,
    publishLocal / skip := true,
  )
