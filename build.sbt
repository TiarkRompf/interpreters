name := "interpreters"

version := "0.1"

scalaVersion := "2.10.0"

scalaOrganization := "org.scala-lang.virtualized"

scalacOptions += "-Yvirtualize"

// tests are not thread safe
parallelExecution in Test := false

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.0"

libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

retrieveManaged := true

