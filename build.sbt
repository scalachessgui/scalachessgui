

import com.github.retronym.SbtOneJar._

oneJarSettings

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

libraryDependencies += "commons-io" % "commons-io" % "2.5"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

val ps = new sys.SystemProperties
val jh = ps("java.home")

javaHome := Some(file(jh.replaceAll("jre","jdk")))

unmanagedJars in Compile +=
{
	Attributed.blank(file(jh) / "lib/ext/jfxrt.jar")
}

addCommandAlias("c","~compile")

name := "guibuilder"

version := "1.0"

scalaVersion := "2.11.8"

