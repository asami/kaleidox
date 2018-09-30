organization := "org.goldenport"

name := "kaleidox"

version := "0.1"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.10.39.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// override goldenport-record
libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.27"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.2.24"

libraryDependencies += "org.goldenport" %% "goldenport-sexpr" % "2.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.1"

libraryDependencies += "cat.inspiracio" % "rhino-js-engine" % "1.7.7.1"

libraryDependencies += "org.apache.commons" % "commons-jexl3" % "3.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
