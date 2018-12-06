lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "freebool",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies ++= Vector(
      "org.typelevel" %% "spire" % "0.16.0",
      "org.typelevel" %% "cats-core" % "1.2.0",
      "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    )
  )
