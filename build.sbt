ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.17"

lazy val root = (project in file("."))
  .settings(
    name := "DSA_Assignments",
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10"
  )
