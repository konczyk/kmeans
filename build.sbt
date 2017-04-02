name := "kmeans"
scalaVersion := "2.12.1"
scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.rogach" %% "scallop" % "2.1.1"

assemblyJarName in assembly := "kmeans.jar"
test in assembly := {}
