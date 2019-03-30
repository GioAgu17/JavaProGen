name := "scala_project"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"


libraryDependencies += "com.github.mifmif" % "generex" % "1.0.2"



libraryDependencies += "it.unibo.alice.tuprolog" % "tuprolog" % "3.1"




resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"



// library dependencies for using logback
libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3" % Test,
  "org.clapper" %% "grizzled-slf4j" % "1.3.2")


// setting main class

mainClass in (Compile, run) := Some("Main")

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2"

libraryDependencies+="com.typesafe" % "config" % "1.3.2"

libraryDependencies += "org.eclipse.jdt" % "org.eclipse.jdt.core" % "3.15.0"
dependencyOverrides += "org.eclipse.platform" % "org.eclipse.equinox.app" % "1.3.600"

libraryDependencies += "io.grpc" % "grpc-netty" % "1.9.0"