ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme     := Some("semver-spec")

publish / skip := true

lazy val typesetter = crossProject( /*JSPlatform,*/ JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name         := "typesetter",
    version      := "0.0.18",
    scalaVersion := "3.5.2",
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
    organization     := "io.github.edadma",
    githubOwner      := "edadma",
    githubRepository := name.value,
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
      "org.scala-js"           %% "scalajs-stubs" % "1.1.0"  % "provided",
      "org.scala-lang.modules" %% "scala-swing"   % "3.0.0"  % "test",
      "io.github.edadma"       %% "texish"        % "0.0.12" % "test",
    ),
  )
  .nativeSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "libcairo" % "0.0.9",
      "io.github.edadma" %%% "freetype" % "0.0.4",
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
