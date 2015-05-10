
scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
, "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)

scalacOptions ++= Seq(
  "-deprecation"
, "-encoding", "UTF-8"
, "-feature"
, "-unchecked"
, "-Xfatal-warnings"
, "-Xfuture"
  // Disable `infer-any` until
  // https://issues.scala-lang.org/browse/SI-9211 is fixed
, "-Xlint:-infer-any,_"
, "-Ywarn-dead-code"
, "-Ywarn-numeric-widen"
, "-Ywarn-unused"
, "-Ywarn-unused-import"
, "-Ywarn-value-discard"
)
