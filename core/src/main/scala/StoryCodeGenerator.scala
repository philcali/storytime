package storytime

import StoryLoader.configLocation
import java.io.File
import scala.io.Source.{fromFile => open}

trait StoryCodeGenerator extends StoryKey {
  val fileName = "%s.story" format (key)

  val path = new File(configLocation, key)

  val storyFile: File

  def changed() = {
    if (storyFile.exists && !path.exists()) {
      true
    } else if (storyFile.exists() && path.exists()) {
      storyFile.lastModified() > path.lastModified()
    } else {
      false
    }
  }

  private def writeSource() {
    val src = srcPath

    if (!src.exists) {
      src.mkdirs()
    }

    writeScala(src)
  }

  private def srcPath() =  {
    new File("%s/src/main/scala".format(path.getAbsolutePath))
  }

  private def readStory() = {
    val story = open(storyFile).getLines.mkString("\n")

    story.replaceAll("\n\n", ",")
  }

  private def writeScala(srcPath: File) {
    import java.io.FileWriter

    val source = 
    """ |package storytime
        |package %s
        |import StoryMode._
        |
        |object %sTemplate extends StoryBoard {
        |  def story() = StoryMode (
        |     %s
        |  )
        |}""".stripMargin.format(
        key, key.capitalize, readStory()
      )
    
    val writer = new FileWriter(new File(srcPath, storyFile.getName))
    writer.write(source)
    writer.close()
  }
}
