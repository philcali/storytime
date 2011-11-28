package storytime

import java.io.{
  BufferedReader, 
  File, 
  InputStreamReader,
  FileOutputStream
}

import StoryLoader.{ copy, configLocation, createIfNotExists }

object StoryCompiler {
  val version = "0.11.2"

  val baseUrl = 
    "http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-tools.sbt/sbt-launch/%s/sbt-launch.jar".format(version)

  def sbt = {
    val boot = createIfNotExists(configLocation.getAbsolutePath, "boot")

    val jarFile = new File(boot, "sbt-launch-%s.jar".format(version))

    if (!jarFile.exists) {
      println("Removing older versions")

      boot.listFiles.filter(_.getName.endsWith(".jar")).foreach(_.delete)

      println("Retrieving sbt launcher...")

      val file = new FileOutputStream(jarFile)

      val jar = new java.net.URL(baseUrl)
    
      StoryLoader.copy(jar.openStream, file)
    }

    jarFile.getAbsolutePath
  }

  def exec(cmd: ProcessBuilder) = {
    val pr = cmd.start()

    val in = new BufferedReader(new InputStreamReader(pr.getInputStream))
    
    def read(in: BufferedReader): Unit = in.readLine match {
      case line: String => println(line); read(in)
      case _ =>
    }

    read(in)
  }

  def compile(base: File) = {
    val pb = new ProcessBuilder("java", "-jar", sbt, "compile", "define-path")

    try {
      Some(exec(pb.directory(base)))
    } catch {
      case _ => None
    }
  }

  def loadClassPath(base: File) = {
    import scala.io.Source.{fromFile => open}

    try {
      val definedPath = new File(base, "defined.path")

      val files = open(definedPath).getLines.map(l => new File(l))

      Some(files.toList)
    } catch {
      case _ => None
    }
  }

  def classPath(base: File) {
    val currentlyLoaded = StoryLoader.loadedJars

    loadClassPath(base) map { projectDeps =>

      for (deps <- projectDeps; val name = deps.getName) {
        val potential = currentlyLoaded.find { f => 
          // No need to dupe the classPath
          val file = new File(f.getFile)

          file.getName.endsWith("jar") && name == file.getName
        }

        potential.orElse {
          Some(StoryLoader.addURL(deps))
        }
      }
    }
  }
}
