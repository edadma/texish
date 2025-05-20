ThisBuild / licenses += "ISC"      -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.7.0"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.0.5b"
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
)
ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("snapshots") ++ Resolver.sonatypeOssRepos("releases")

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

ThisBuild / homepage := Some(url("https://github.com/edadma/typesetter"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

lazy val typesetter = crossProject( /*JSPlatform,*/ JVMPlatform, NativePlatform)
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
//    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "pprint" % "0.9.0" % "test",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
    licenses += "ISC"      -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.scala-js"           %% "scalajs-stubs" % "1.1.0" % "provided",
      "org.scala-lang.modules" %% "scala-swing"   % "3.0.0" % "test",
      "io.github.edadma"       %% "texish"        % "0.0.1" % "test",
    ),
  )
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "libcairo" % "0.0.2",
      "io.github.edadma" %%% "freetype" % "0.0.1",
    ),
  )
//  .jsSettings(
//    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
//    //    Test / scalaJSUseMainModuleInitializer := true,
//    //    Test / scalaJSUseTestModuleInitializer := false,
//    Test / scalaJSUseMainModuleInitializer      := false,
//    Test / scalaJSUseTestModuleInitializer      := true,
//    scalaJSUseMainModuleInitializer             := true,
//    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
//  )

lazy val root = project
  .in(file("."))
  .aggregate( /*typesetter.js,*/ typesetter.jvm, typesetter.native)
  .settings(
    name                := "typesetter",
    publish / skip      := true,
    publishLocal / skip := true,
  )
