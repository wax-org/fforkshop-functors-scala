name := "Functional Forkshop - Functors"

scalacOptions += "-Ypartial-unification"

// Main dependencies
libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"
libraryDependencies += "com.twitter" %% "algebird-core" % "0.13.4"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")

// Test dependencies
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
libraryDependencies += "org.typelevel" %% "cats-laws" % "1.1.0" % Test
libraryDependencies += "org.typelevel" %% "cats-testkit" % "1.1.0" % Test
libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
