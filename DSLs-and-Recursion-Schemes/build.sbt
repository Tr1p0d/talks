scalaVersion := "2.12.7"
name := "recursion-schemes"
organization := ""
version := "1.0"
scalacOptions ++= Seq(
  "-Yno-generic-signatures",
  "-language:higherKinds",
  "-feature"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"

