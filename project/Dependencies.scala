import sbt._

object Dependencies {
  private lazy val dependencies = {
    new {
      // Versions
      val betterMonadicForV = "0.2.4"
      val kindProjectorV = "0.9.3"

      // Managed libraries

      // Compiler plugins
      val kindProjector = "org.spire-math" %% "kind-projector" % kindProjectorV
      val betterFor = "com.olegpy" %% "better-monadic-for" % betterMonadicForV
    }
  }
  lazy val compilerPlugins: Seq[ModuleID] =
    Seq(compilerPlugin(dependencies.kindProjector),
        compilerPlugin(dependencies.betterFor))

}
