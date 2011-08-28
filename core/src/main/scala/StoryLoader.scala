package storytime

import java.net.{URLClassLoader, URL}
import java.io.File

object StoryLoader {
  lazy val templateLocation =
    createIfNotExists(configLocation.getAbsolutePath, "templates")

  lazy val configLocation = 
    createIfNotExists(System.getProperty("user.home"), ".storytime")

  private def loader = {
    this.getClass.getClassLoader.asInstanceOf[URLClassLoader]
  }

  private def addURL(url: URL) {
    val classLoader = loader
    val clazz = classOf[URLClassLoader]

    val method = clazz.getDeclaredMethod("addURL", classOf[URL])
    method.setAccessible(true)
    method.invoke(classLoader, url)
  }

  private def toURL(path: String) = {
    new File(path).toURI.toURL
  }

  private def createIfNotExists(base: String, name: String) = {
    val dir = new File(base, name)
  
    if (!dir.exists) dir.mkdir()

    dir
  }

  def loadedJars = loader.getURLs

  def listTemplates() = {
    val folders = (folder: File) =>
      !folder.getName.startsWith(".") && folder.isDirectory()

    val templates = templateLocation.listFiles.filter(folders)

    if (templates.isEmpty) None else Some(templates)
  }

  def loadTemplate(templateName: String) = {
    listTemplates.map {
      _.find(_.getName == templateName)
    } 
  }

  def loadMode(tname: String) = {
    val fullPackage = templateClassName(tname)

    try {
      val templateClazz = Class.forName(fullPackage)
      val story = templateClazz.getDeclaredMethod("story")

      Some(story.invoke(null).asInstanceOf[StoryMode]) 
    } catch {
      case e: Exception => None
    }
  }

  private def templateClassName(tname: String) =  
    "storytime.%s.%sTemplate".format(tname, tname.capitalize) 

}
