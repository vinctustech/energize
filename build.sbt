name := "energize"

version := "0.13_snapshot_9.5"

scalaVersion := "2.12.6"

//crossScalaVersions := Seq( "2.11.11" )

scalacOptions ++= Seq( "-deprecation", "-feature", "-unchecked", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

//resolvers += Resolver.sonatypeRepo( "snapshots" )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

parallelExecution in Test := false

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
)

libraryDependencies ++= Seq(
	"com.typesafe" % "config" % "1.3.3"
)

libraryDependencies ++= Seq(
	"com.h2database" % "h2" % "1.4.197",
	//	"org.postgresql" % "postgresql" % "9.4.1212.jre7",	//latest release of 2016
	"org.postgresql" % "postgresql" % "42.2.1"		//latest as of Jan 2018
	//	"mysql" % "mysql-connector-java" % "6.0.5",
	//	"org.mariadb.jdbc" % "mariadb-java-client" % "1.5.7",
	//	"org.xerial" % "sqlite-jdbc" % "3.16.1",
	//	"org.firebirdsql.jdbc" % "jaybird-jdk16" % "2.2.12",
	//	"org.apache.derby" % "derby" % "10.13.1.1",
)

libraryDependencies ++= Seq(
	"jline" % "jline" % "2.14.6",
	"org.apache.httpcomponents" % "httpcore-nio" % "4.4.9",
	"org.apache.httpcomponents" % "httpclient" % "4.5.5",
  "org.apache.httpcomponents" % "fluent-hc" % "4.5.5",
	"org.mindrot" % "jbcrypt" % "0.3m",
	"com.pauldijou" %% "jwt-core" % "0.16.0"
)

libraryDependencies ++= Seq(
	"xyz.hyperreal" %% "bvm" % "0.4.3",
	"xyz.hyperreal" %% "funl2" % "0.4.5",
	"xyz.hyperreal" %% "lia" % "0.22.2",
	"xyz.hyperreal" %% "json" % "0.7.4",
	"xyz.hyperreal" %% "table" % "0.9",
	"xyz.hyperreal" %% "options" % "0.2",
	"xyz.hyperreal" %% "importer" % "0.4.1",
  "xyz.hyperreal" %% "liquescent" % "0.2.4"
)

mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".ServerMain" )

mainClass in assembly := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".ServerMain" )

assemblyJarName in assembly := name.value + "-" + version.value + ".jar"

coverageExcludedPackages := ".*PostgresDatabase;.*EnergizeServer.*;.*ServerMain;.*REPLMain;.*Unparse"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra :=
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
    </developer>
  </developers>
