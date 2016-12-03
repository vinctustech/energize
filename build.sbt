name := "cras"

version := "0.3"

scalaVersion := "2.12.0"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "xyz.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.0" % "test",
	"org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
//	"org.scala-lang.modules" %% "scala-swing" % "1.0.2"
)

libraryDependencies ++= Seq(
	"com.h2database" % "h2" % "1.4.193",
	"jline" % "jline" % "2.14.2",
	"org.apache.httpcomponents" % "httpcore-nio" % "4.4.5"
)

libraryDependencies ++= Seq(
	"xyz.hyperreal" %% "indentation-lexical" % "0.6",
	"xyz.hyperreal" %% "json" % "0.5",
	"xyz.hyperreal" %% "table" % "0.2",
	"xyz.hyperreal" %% "lia" % "0.19"
)

mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value + ".REPLMain" )

mainClass in assembly := Some( "xyz.hyperreal." + name.value + ".REPLMain" )

assemblyJarName in assembly := name.value + "-" + version.value + ".jar"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra := (
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
      <id>emaxedon</id>
      <name>Edward W. Maxedon, Jr.</name>
      <url>https://github.com/emaxedon</url>
    </developer>
  </developers>)

//enablePlugins(HugoPlugin)

//ghpages.settings

//git.remoteRepo := s"git@github.com:edadma/${name.value}.git"