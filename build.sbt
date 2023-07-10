name := "BrahScala"

version := "0.1"

scalaVersion := "2.12.11"

idePackagePrefix := Some("institute.teias")

Compile / unmanagedJars += {
  baseDirectory.value / "unmanaged" / s"scalaz3-unix-x64-2.12.jar"
}
Compile / unmanagedJars += {
  baseDirectory.value / "unmanaged" / s"java-jar-4.11.2.jar"
}
