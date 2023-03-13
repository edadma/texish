ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

publish / skip := true

lazy val texish = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "texish",
    version := "0.0.2",
    scalaVersion := "3.2.2",
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
    githubOwner := "edadma",
    githubRepository := name.value,
    mainClass := Some(s"${organization.value}.${name.value}.Main"),
    //    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.14" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "char-reader" % "0.1.11",
    ),
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.1.0",
      //      "com.lihaoyi" %%% "pprint" % "0.8.1" % "test",
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .nativeSettings(
    nativeLinkStubs := true,
    libraryDependencies += "io.github.cquiroz" % "scala-java-time_native0.4_3" % "2.5.0",
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    //    Test / scalaJSUseMainModuleInitializer := true,
    //    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
  )
