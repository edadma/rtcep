import AssemblyKeys._

import LaikaKeys._


name := "rtcep"

version := "0.1"

scalaVersion := "2.11.4"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "ca.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "http://hyperreal.ca/maven2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5" % "test"

//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.6"

//libraryDependencies ++= Seq(
//	"org.scala-lang.modules" %% "scala-swing" % "1.0.1",
//	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
//	)

//libraryDependencies += "org.scaloid" %% "scaloid" % "3.2-8"

//libraryDependencies += "org.clapper" %% "argot" % "1.0.3"

//libraryDependencies += "com.h2database" % "h2" % "1.3.176"

libraryDependencies ++= Seq(
//	"org.postgresql" % "postgresql" % "9.2-1004-jdbc4"
//	"mysql" % "mysql-connector-java" % "5.1.29"
//	"org.mongodb" %% "casbah" % "2.6.3"
//	"org.antlr" % "stringtemplate" % "4.0.2"
	)
	
//mainClass in (Compile, packageBin) := Some( "ca.hyperreal.myproject.Main" )

//mainClass in (Compile, run) := Some( "ca.hyperreal.myproject.Main" )

//offline := true

assemblySettings

mainClass in assembly := Some( "ca.hyperreal.myproject.Main" )

jarName in assembly := name.value + "-" + version.value + ".jar"


LaikaPlugin.defaults

templateDirectives in Laika += LaikaExtension.bootstrapToc


publishMavenStyle := true

publishTo := Some( Resolver.sftp( "private", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

//{
//  val nexus = "https://oss.sonatype.org/"
//  if (isSnapshot.value)
//    Some("snapshots" at nexus + "content/repositories/snapshots")
//  else
//    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
//}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/color"))

pomExtra := (
  <scm>
    <url>git@github.com:edadma/color.git</url>
    <connection>scm:git:git@github.com:edadma/color.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://hyperreal.ca</url>
    </developer>
  </developers>)
