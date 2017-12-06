name := "PReo"

version := "1.1"

scalaVersion := "2.11.8"

// more warnings
scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

// more complete check for complete "cases" (scala 2.10)
// initialize ~= { _ => sys.props("scalac.patmat.analysisBudget") = "512" }


libraryDependencies ++= Seq(
  "junit" % "junit" % "4.12",
  "com.novocode" % "junit-interface" % "0.11" % "test", // to run junit tests from sbt
  "org.choco-solver" % "choco-solver" % "3.3.1-j7",
//  "org.slf4j" % "slf4j-simple" % "1.7.12",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value // needed for JIT
//   "org.scala-lang" % "scala-reflect" % scalaVersion.value,


)

// tests cannot be run in parallel, because of the Choco solver
parallelExecution in Test := false
