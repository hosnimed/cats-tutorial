name := "cats-tutorial"

version := "1.3.0"

scalaVersion := "2.11.11"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.3.0" withSources() withJavadoc()

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
  "-Ypartial-unification")
