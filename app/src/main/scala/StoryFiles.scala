package storytime

import java.io.File

import scala.io.Source.{fromFile => open}

case class MdFile(input: File) extends ConfigFile {
  val key = "file"

  lazy val cachedSource = source

  if (cachedSource.trim.isEmpty) {
    clean(path)
  }

  override def source = {
    MetaStripper.create(super.source) match {
      case Array(meta, markdown) => meta
      case _ => ""
    }
  }

  override def changed() = 
    super.changed && !cachedSource.isEmpty && !same

  def same = try {
    generate.source == open(srcFile).getLines.mkString("\n")
  } catch {
    case _ => false
  }

  def clean(f: File): Unit = {
    if (f.isDirectory) {
      f.listFiles.filter(!_.getName.startsWith(".")).foreach(clean)
    }

    f.delete
  }
}

object GlobalFile extends ConfigFile {
  val key = "global"

  def input = new File(StoryLoader.configLocation, fileName)
}

object LocalFile extends ConfigFile {
  val key = "local"

  def input = new File(".", fileName)
}

trait ConfigFile extends LocalGeneration { self =>
  import StoryCompiler._

  def loadMode = {

    if (self.changed) {
      self.write()

      println("Found new meta in %s, compiling..." format (self.key))

      compile(self.path)
    } 

    // Refresh classpath
    println("Loading meta from %s" format(self.key))
    classPath(self.path)

    StoryLoader.loadClass(self.key).map(_.mode)
  }
}
