val commonsCodecV = "1.9"
val logbackClassicV = "1.1.2"
scalaVersion := "2.11.12"

lazy val shPlanningSystem = (project in file("."))
  .settings(
    name := "SH Planning System",
    libraryDependencies ++= Seq(
      "org.clapper" %% "grizzled-slf4j" % "1.3.4",
      "ch.qos.logback" % "logback-classic" % logbackClassicV,
      "io.spray" %%  "spray-json" % "1.3.6",
      "io.spray" %% "spray-client" % "1.3.4",
      "io.spray" %% "spray-routing" % "1.3.4",
      "com.typesafe.akka" %% "akka-actor" % "2.5.32",
      "com.typesafe.akka" %% "akka-http-core" % "10.1.15",
      "com.typesafe.akka" %% "akka-http" % "10.1.15",
      "com.typesafe.akka" %% "akka-stream" % "2.5.32",
      "org.scalatest" %% "scalatest" % "3.3.0-SNAP3" % Test,
      "junit" % "junit" % "4.13.2" % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
      "com.typesafe.akka" %% "akka-http" % "10.1.15"
  )
  )