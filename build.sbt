name := "SlateMissingPersonUpdate"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/"

libraryDependencies ++= {
  val slickV = "3.1.1"
  val scalaTestV = "2.2.6"
  Seq(
    "edu.eckerd" %% "slate-core" % "0.1.0-SNAPSHOT",
    "com.typesafe.slick" %% "slick" % slickV,
    "com.typesafe.slick" %% "slick-hikaricp" % slickV,
    "com.typesafe.slick" %% "slick-extensions" % "3.1.0",
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
    "org.typelevel" %% "cats" % "0.6.1",
    "me.lessis" %% "courier" % "0.1.3",
    "com.h2database" % "h2" % "1.4.187" % "test",
    "org.scalatest" %% "scalatest" % scalaTestV % "test"
  )
}