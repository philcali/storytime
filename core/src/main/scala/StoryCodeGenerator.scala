package storytime

import StoryLoader.configLocation
import java.io.File
import scala.io.Source.{fromFile => open}

trait LocalGeneration extends FileGeneration with StoryFileReader {
  val fileName = "%s.story" format (key)

  val path = new File(configLocation, key)

  def changed() = {
    if (file.exists && !path.exists()) {
      true
    } else if (file.exists() && path.exists()) {
      file.lastModified() > path.lastModified()
    } else {
      false
    }
  }
}

trait FileGeneration extends StoryCodeGenerator {
  val path: File 

  def write() {
    val src = srcPath

    if (!src.exists) {
      src.mkdirs()
    }

    writeSource(src)
  }

  private def srcPath() =  {
    new File("%s/src/main/scala".format(path.getAbsolutePath))
  }

  private def writeSource(srcPath: File) {
    import java.io.FileWriter

    val scalaFile = "%sTemplate.scala".format(key.capitalize)

    val writer = new FileWriter(new File(srcPath, scalaFile))
    writer.write(generate)
    writer.close()
  }
}

trait StoryFileReader extends StoryCodeGenerator {
  val file: File

  def source = readStory()

  private def readStory() = {
    open(file).getLines.mkString("\n")
  }
}

trait StoryCodeGenerator extends StoryKey {
  def source: String 

  private def parseSource() = {
    val lines = source.split("\n")

    val Import = """^import (.+)""".r

    val (imports, sourced) = lines.partition(Import.findFirstIn(_).map(_ => true).getOrElse(false))

    val pre = sourced.dropWhile(_.trim.isEmpty)

    val transformed = pre.map("    %s".format(_)).mkString("\n")

    (imports.mkString("\n"), transformed.replaceAll("\n\\s{4}\n", ",\n"))
  }

  def generate() = {
    val (imports, source) = parseSource()

    """|package storytime
       |package %s
       |import StoryMode._
       |
       |%s
       |
       |object %sTemplate extends StoryBoard {
       |  def story() = Seq (
       |    %s
       |  )
       |}""".stripMargin.format(
        key, imports, key.capitalize, source
      )
  }
}
