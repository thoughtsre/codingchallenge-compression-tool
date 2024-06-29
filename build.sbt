val scala3Version = "3.4.2"

lazy val root = project
    .in(file("."))
    .settings(
        name := "CodingChallenge Compression Tool",
        version := "0.1.0-SNAPSHOT",
        scalaVersion := scala3Version,
    )

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test