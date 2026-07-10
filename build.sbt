import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.4"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.19.3"
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

lazy val texish = crossProject(JSPlatform, JVMPlatform, NativePlatform)
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
    // Embed the bundled `packages/*.texish` modules as Scala constants at build time, so they ship compiled
    // into every target. The module loader consults these as a fallback after its filesystem search, which
    // lets a host with no package directory on disk — chiefly the browser — resolve the standard modules,
    // while a local package file still shadows the embedded one wherever the filesystem search finds it first.
    // Each module's source is split into chunks small enough for the compiler and rejoined at runtime.
    Compile / sourceGenerators += Def.task {
      val root   = (LocalRootProject / baseDirectory).value
      val pkgDir = root / "packages"
      val out =
        (Compile / sourceManaged).value / "io" / "github" / "edadma" / "texish" / "EmbeddedPackages.scala"
      def esc(s: String): String =
        s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\r", "").replace("\n", "\\n")
      val files = (pkgDir * "*.texish").get.sortBy(_.getName)
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
    libraryDependencies ++= Seq(
      "org.scala-js"           %% "scalajs-stubs" % "1.1.0" % "provided",
      "org.scala-lang.modules" %% "scala-swing"   % "3.0.0" % "test",
    ),
  )
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "libcairo"  % "0.0.8",
      "io.github.edadma" %%% "freetype"  % "0.0.7",
      "io.github.edadma" %%% "turbojpeg" % "0.0.1",
    ),
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    // Embed the fonts the in-browser SVG renderer draws with, as base64 in a generated Scala source, since a
    // browser has no filesystem. Only the Latin Modern text/math/mono stack is shipped — the full set is 61MB;
    // this curated subset is ~1.3MB. The path list MUST stay in sync with SvgTypesetterJS.loadBundledFonts.
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
  .aggregate(texish.js, texish.jvm, texish.native, texishCli)
  .settings(
    name                := "texish",
    publish / skip      := true,
    publishLocal / skip := true,
  )
