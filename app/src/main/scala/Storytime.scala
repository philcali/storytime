package storytime

import org.clapper.argot._

object Storytime {
  import ArgotConverters._

  val parser = new ArgotParser(
    "storytime",
    preUsage=Some("0.1.0 Copyright (c) 2011, Philip Cali")
  )

  val installTemplate = parser.option[String](
    List("i", "install"), "user/template-name.story",
    "Attempts to install the story template from github"
  )

  val uninstallTemplate = parser.option[String](
    List("u", "uninstall"), "template-name",
    "Attempts to uninstall the story template locally"
  )

  val listTemplates = parser.flag[Boolean](
    List("l", "list-templates"),
    "Displays all of the installed templates"
  )

  val clearTemplates = parser.flag[Boolean](
    List("c", "clear-templates"),
    "Uninstalls all templates"
  )

  val wipe = parser.flag[Boolean](
    List("w", "clean-compiled"),
    "Wipes the compiled story scala projects"
  )

  val help = parser.flag[Boolean](List("h", "help"), "Prints this help") {
    (value, opt) =>
    if (value) parser.usage() else value
  }

  val templateKeys = parser.option[String](
    List("k", "template-keys"), "template-name",
    "Displays special template keys. ie: -k default"
  )

  val recursive = parser.flag[Boolean](
    List("r", "recursive"),
    "Runs Storytime on a directory tree."
  )

  val file = parser.parameter[java.io.File](
    "markdown-file",
    "Text file containing Markdown source",
    true
  ) {
    (path, opt) =>
    
    val f = new java.io.File(path)
    if (!f.exists || f.isDirectory)
      parser.usage("Markdown file '%s' does not exists" format path)
    f
  }

  val templateName = parser.parameter[String](
    "template-name",
    "Use this template processor when outputing conversion",
    true
  )

  val inputArgs = parser.multiParameter[(String, String)](
    "input-args",
    "Direct arguments to override all story configuration options",
    true
  ) {
    (arg, opt) =>
    
    val valid = arg.contains("=")
    if (!valid)
      parser.usage("Input parameters must be in the form: 'key=value'")

    val Array(key, value) = arg.split("=")
    (key, value)
  }

  def main (args: Array[String]) {

    try {
      if (args.isEmpty) parser.usage()

      parser.parse(args)

      listTemplates.value.foreach { _ =>
        StoryLoader.listTemplates.fold(println, _.foreach(println))
      }

      clearTemplates.value.foreach { _ =>
        StoryLoader.delete(StoryLoader.templateLocation)
        println("Cleared installed templates")
      }

      wipe.value.foreach { _ =>
        StoryLoader.delete(StoryLoader.storyLocation)
        println("Cleared compiled configurations")
      }

      templateKeys.value.foreach { name =>
        StoryLoader.loadTemplate(name).map { template =>
          def formatKey(arg: StoryMetaKey[_]) {
            val req = if (arg.required) "[REQUIRED]" else ""

            println("  %1$-20s  %2$s %3$s".format(arg.key, arg.description, req)) 
          }

          println("Default Keys:")
          template.defaultKeys.foreach(formatKey)

          if (!template.templateKeys.isEmpty) {
            println
            println("%s Keys:".format(name))

            template.templateKeys.foreach(formatKey)
          }
        }
      }

      file.value.foreach { md =>
        import StoryKeys.defaultTemplate

        val emptyMode = StoryMode(Nil)

        val configs = List[ConfigFile](GlobalFile, LocalFile, MdFile(md))

        val mode = configs.foldLeft(emptyMode) { (in, config) =>
          in ++ config.loadMode.getOrElse(emptyMode)
        }

        val temp = templateName.value.getOrElse(
          mode.getOrElse(defaultTemplate, "default")
        )

        StoryLoader.loadTemplate(temp).map { t =>

          val converter = Converter(t.mode ++ mode)

          val book = converter.fromFile(md)
          
          t(book)
        }

        println("Done")
      }
  
    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }
}

class Storytime extends xsbti.AppMain {
  def run(configuration: xsbti.AppConfiguration) = {
    Storytime.main(configuration.arguments)
    Exit(0)
  }

  case class Exit(code: Int) extends xsbti.Exit
}
