package storytime

import StoryLoader.{ configLocation, templateLocation }
import java.io.File
import scala.io.Source.{fromFile => open}

trait LocalGeneration extends FileGeneration with StoryFileReader {
  def fileName = "%s.story" format (key)

  def path = new File(configLocation, key)

  def changed() = {
    if (input.exists && !path.exists()) {
      true
    } else if (input.exists() && path.exists()) {
      input.lastModified() > srcFile.lastModified()
    } else {
      false
    }
  }
}

trait FileGeneration extends StoryCodeGenerator {
  def path: File 

  def write() {
    val src = srcPath
    val build = buildPath

    if (!src.exists) {
      src.mkdirs()
    }

    if (!build.exists) {
      build.mkdirs()
    }

    writeSource()

    writeBuild()
  }

  def srcPath() =  {
    new File("%s/src/main/scala".format(path.getAbsolutePath, key))
  }

  def buildPath() = {
    new File("%s/project/".format(path.getAbsolutePath, key))
  }

  def srcFile = {
    val scalaFile = "%sTemplate.scala".format(key.capitalize)
    new File(srcPath, scalaFile)
  }

  def projectFile = new File(buildPath, "build.scala")

  private def write(location: File, contents: String) {
    import java.io.FileWriter

    val writer = new FileWriter(location)
    writer.write(contents)
    writer.close()
  }

  private def writeSource() {
    write(srcFile, generate.source)
  }

  private def writeBuild() {
    write(projectFile, generate.build)
  }
}

trait StoryFileReader extends StoryCodeGenerator {
  def input: File

  def source = readStory()

  private def readStory() = {
    open(input).getLines.mkString("\n")
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

    (imports, transformed.replaceAll("\n\\s{4}\n", ",\n"))
  }

  def generate() = {
    val (imports, source) = parseSource()

    sbtTemplate(imports,
    """|package storytime
       |package %s
       |import StoryMode._
       |import StoryKeys._
       |
       |%s
       |
       |object %sTemplate extends StoryBoard {
       |  val key = "%s"
       |  def story() = Seq (
       |%s
       |  )
       |}""".stripMargin.format(
        key, imports.mkString("\n"), key.capitalize, key, source
      )
    )
  }
}

case class sbtTemplate(imports: Seq[String], source: String) {
  val ProjectRef = """^import (.+)\._""".r

  private def isProjectRef(theImport: String) = {
    ProjectRef.findFirstIn(theImport).isDefined
  }

  private def dependencyExist(dep: String) = {
    val file = new File(templateLocation, dep)
    (file.exists && file.isDirectory)
  }

  private def createProjectRef(dep: String) = {
    val file = new File(templateLocation, dep)

    """lazy val %s = ProjectRef(uri("%s"), "%s-template")""".format(
      dep, file.getAbsolutePath, dep
    )
  }

  lazy val dependencies = imports.filter(isProjectRef).map { r =>
    val ProjectRef(ref) = r
    ref.split("\\.").head
  }

  def build = {
    val actualDeps = dependencies.filter(dependencyExist)

    val refs = actualDeps.map(createProjectRef)

    val depends = if (actualDeps.size > 0) { 
      "dependsOn(%s)".format(actualDeps.mkString(","))
    } else {
      ""
    }

    val lib = 
"""libraryDependencies += "com.github.philcali" %% "storytime-core" % "0.0.1"
"""

    """|import sbt._
       |import Keys._
       |
       |object StoryBuild extends Build {
       |  
       |  val definePath = TaskKey[Unit]("define-path")
       |
       |  private def definePathTask = (fullClasspath in Compile, streams) map {
       |    (fp, s) =>
       |      IO.write(file(".") / "defined.path", fp.map(_.data).mkString("\n"))
       |  }
       |
       |  def generalSettings = Defaults.defaultSettings ++ Seq (
       |    definePath <<= definePathTask,
       |    %s
       |  )
       |
       |  lazy val root = Project(
       |    "", 
       |    file("."), 
       |    settings = generalSettings
       |  ) %s
       |
       |  %s
       |}""".stripMargin.format(lib, depends, refs.mkString("\n"))
  }
}
