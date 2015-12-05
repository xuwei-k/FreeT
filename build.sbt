scalaVersion := "2.11.7"

name := "FreeT"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.5"
libraryDependencies += "com.github.scalaprops" %% "scalaprops-gen" % scalapropsVersion.value

licenses := Seq("MIT License" -> url("http://opensource.org/licenses/mit"))

scalapropsWithScalazlaws

scalapropsVersion := "0.2.0"

scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  Nil
)
