name := """play2-hands-on"""
organization := "com.example"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  guice,
  "mysql" % "mysql-connector-java" % "5.1.47",
  "org.scalikejdbc" %% "scalikejdbc" % "3.2.2", // 追加
  "org.scalikejdbc" %% "scalikejdbc-config" % "3.2.2", // 追加
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" % "2.6.0-scalikejdbc-3.2", // 追加
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
)
enablePlugins(ScalikejdbcPlugin)

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.example.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.example.binders._"
