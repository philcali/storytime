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

  private def addURL(file: File) {
    val url = toURL(file)
    val classLoader = loader
    val clazz = classOf[URLClassLoader]

    val method = clazz.getDeclaredMethod("addURL", classOf[URL])
    method.setAccessible(true)
    method.invoke(classLoader, url)
  }

  private def toURL(path: File) = path.toURI.toURL

  private def createIfNotExists(base: String, name: String) = {
    val dir = new File(base, name)
  
    if (!dir.exists) dir.mkdir()

    dir
  }

  def loadedJars = loader.getURLs

  def listTemplates() = {
    val folders = (folder: File) =>
      !folder.getName.startsWith(".") && folder.isDirectory()

    val templates = templateLocation.listFiles.filter { f => 
      folders(f) && (f.getName != "local" || f.getName != "global")
    }

    if (templates.isEmpty) 
      Left("No templates installed.")
    else 
      Right(templates)
  }

  def loadTemplate(templateName: String) = {
    listTemplates.fold(str => Left(str), {
      _.find(_.getName == templateName) match {
        case Some(f) => 
          loadLocal(f.getName).map(Right(_)).orElse {
            Some(Left("Failed to load %s".format(templateClassName(templateName))))
          } get
        case None => Left("%s is not installed.".format(templateName))
      }
    })
  }

  def loadClass(templateName: String) = {
    val fullPackage = templateClassName(templateName)

    try {
      Some(new TemplateClass(templateName, Class.forName(fullPackage)))
    } catch {
      case _ => None
    }
  }

  private def loadLocal(name: String) = {
    val isJar = (file: File) => file.getName.endsWith(".jar")

    val libs = new File(templateLocation, name).listFiles.find(_.getName == "lib")

    // Load dependencies
    libs.map(_.listFiles.filter(isJar).foreach(addURL))

    // Load template
    val jar = new File(templateLocation, name).listFiles.find(isJar)
    jar.map(addURL)

    loadClass(name)
  }

  private def templateClassName(tname: String) =  
    "storytime.%s.%sTemplate".format(tname, tname.capitalize) 
}

class TemplateClass(name: String, clazz: Class[_]) extends StoryKey {
  val key = name

  def apply(book: StoryBook) {
    val app = clazz.getDeclaredMethod("apply", classOf[StoryBook])

    app.invoke(null, book)
  }

  def mode = {
    val story = clazz.getDeclaredMethod("story")

    story.invoke(null).asInstanceOf[StoryMode]
  }

  def arguments = {
    val metaKey = classOf[StoryMetaKey[_]]

    val keys = clazz.getDeclaredMethods.filter(_.getReturnType == metaKey)

    val metaKeys = keys.map(_.invoke(null).asInstanceOf[StoryMetaKey[_]])

    metaKeys.sortWith(_.key < _.key)
  } 
}
