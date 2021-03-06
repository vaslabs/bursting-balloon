name := "bursting-balloon"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:postfixOps",              //Allow postfix operator notation, such as `1 to 10 toList'
  "-language:implicitConversions",
  "-language:higherKinds",
  "-Ywarn-dead-code",                  // Warn when dead code is identified.
  "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  "-Ywarn-numeric-widen",              // Warn when numerics are widened.
  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  "-Ywarn-unused:params",              // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",            // Warn if a private member is unused.
  "-Ywarn-value-discard",               // Warn when non-Unit expression results are unused.
  "-Ywarn-unused:imports",
  "-Xfatal-warnings"
)
