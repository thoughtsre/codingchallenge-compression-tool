val scala3Version = "3.4.2"

lazy val root = project
    .in(file("."))
    .settings(
        name := "CodingChallenge Compression Tool",
        version := "0.1.0-SNAPSHOT",
        scalaVersion := scala3Version,
    )

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"
