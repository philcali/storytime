package storytime

import java.net.{URLClassLoader, URL}
import java.io.{
  File,
  OutputStream,
  InputStream,
  ByteArrayOutputStream,
  FileOutputStream,
  FileInputStream
}

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

  def outsource(filename: String) = {
    new File(filename.split("/").dropRight(1).mkString("/")).mkdirs
    new FileOutputStream(filename)
  }

  def copy(in: InputStream, out: OutputStream) {
    val buf = new Array[Byte](1024)
    in read(buf, 0, 1024) match {
      case n if n >= 0 => out.write(buf, 0, n); copy(in, out)
      case _ => in.close; out.close
    }
  }

  def copyFile(in: InputStream, filename: String) {
    copy(in, outsource(filename))
  }

  def copyBytes(in: InputStream) = {
    val out = new java.io.ByteArrayOutputStream

    copy(in, out)

    out.toByteArray
  }

  def loadResource(key: String, name: String) = { 
    val stream = this.getClass.getClassLoader.getResourceAsStream(key + "/" + name)  
    if (stream == null) {
      import java.io.FileInputStream

      try {
        Some(new FileInputStream(name))
      } catch {
        case e: java.io.FileNotFoundException => None
      }
    } else {
      Some(stream)
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
