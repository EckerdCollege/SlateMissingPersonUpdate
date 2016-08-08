name := "SlateMissingPersonUpdate"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/"
resolvers += "ERI OSS" at "http://dl.bintray.com/elderresearch/OSS"

libraryDependencies ++= {
  val slickV = "3.1.1"
  val scalaTestV = "3.0.0"
  Seq(
    "edu.eckerd" %% "slate-core" % "0.1.0",
    "com.typesafe.slick" %% "slick" % slickV,
    "com.typesafe.slick" %% "slick-hikaricp" % slickV,
    "com.typesafe.slick" %% "slick-extensions" % "3.1.0",
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
    "org.typelevel" %% "cats" % "0.6.1",
    "me.lessis" %% "courier" % "0.1.3",
    "com.elderresearch" %% "ssc" % "0.2.0",
    "com.googlecode.libphonenumber" % "libphonenumber" % "7.5.1",
    "com.h2database" % "h2" % "1.4.187" % "test",
    "org.scalatest" %% "scalatest" % scalaTestV % "test",
    "de.saly" % "javamail-mock2-fullmock" % "0.5-beta4" % "test"
  )
}