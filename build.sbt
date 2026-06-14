import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.4"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.2.0"
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

ThisBuild / sonatypeProfileName := "io.github.edadma"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/typesetter"),
    "scm:git@github.com:edadma/typesetter.git",
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

ThisBuild / homepage    := Some(url("https://github.com/edadma/typesetter"))
ThisBuild / description := "A document layout and PDF rendering engine for Scala"

ThisBuild / publishTo := sonatypePublishToBundle.value

lazy val typesetter = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "typesetter",
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
      "io.github.edadma" %%% "char_reader"    % "0.1.27",
      "io.github.edadma" %%% "cross_platform" % "0.1.6",
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "pprint" % "0.9.0" % "test",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
    // Embed the TeX en-US hyphenation patterns as a Scala constant at build time, so they ship
    // compiled into every target — a native binary has no pattern file to read at runtime.
    // Hyphenation.enableEnglish() loads this. The string is escaped so any pattern content is safe.
    Compile / sourceGenerators += Def.task {
      val root    = (LocalRootProject / baseDirectory).value
      val src     = root / "shared" / "src" / "main" / "resources" / "hyph-en-us.tex"
      val out     = (Compile / sourceManaged).value / "io" / "github" / "edadma" / "typesetter" / "EnglishHyphenationPatterns.scala"
      val escaped = IO.read(src).replace("\\", "\\\\").replace("\"", "\\\"").replace("\r", "").replace("\n", "\\n")
      IO.write(
        out,
        "package io.github.edadma.typesetter\n\n" +
          "// Generated at build time from shared/src/main/resources/hyph-en-us.tex — do not edit.\n" +
          "private[typesetter] object EnglishHyphenationPatterns:\n" +
          "  val content: String = \"" + escaped + "\"\n",
      )
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
      "io.github.edadma" %%% "libcairo" % "0.0.7",
      "io.github.edadma" %%% "freetype" % "0.0.6",
    ),
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
  )

lazy val root = project
  .in(file("."))
  .aggregate(typesetter.js, typesetter.jvm, typesetter.native)
  .settings(
    name                := "typesetter",
    publish / skip      := true,
    publishLocal / skip := true,
  )
