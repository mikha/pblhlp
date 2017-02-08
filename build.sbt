enablePlugins(ScalaJSPlugin)

name := "help"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

scalaJSStage in Global := FastOptStage

skip in packageJSDependencies := false

jsDependencies += RuntimeDOM

persistLauncher in Compile := true

persistLauncher in Test := false

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.5"
libraryDependencies += "com.lihaoyi" %%% "scalarx" % "0.3.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"