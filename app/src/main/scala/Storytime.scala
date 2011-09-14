package storytime

object Storytime {
  def printHelp() {
    println(
    """    storytime [story commands]

        List of story commands:

          list-templates: displays all the installed templates
          clear-templates: Uninstall all templates
          template-args <template-name>: displays special template arguments
          install <[https://github.com/|file://]template-location>
          uninstall <template-name>
          get <template-arg>
          
        General actions:

          [options] [template-name] <markdown directory or file> [template-args]

        General options:
          
          -h: prints this help
          -r: recursively"""
    )
  }

  def main (args: Array[String]) {
    if (args.contains("-h") || args.length == 0) printHelp()
    else { 
      val recursively = args.contains("-r")

      val (options, rest) = args.partition(_.startsWith("-"))

      val (tempArgs, important) = rest.partition(_.contains("="))

      rest match {
        case Array(singleInput) => singleInput match {
          case "list-templates" => 
            StoryLoader.listTemplates.fold(println, _.foreach { f => 
              println(f.getName)
            })
          case "clear-templates" =>
            def delete(f: java.io.File) {
              if (f.isDirectory) {
                f.listFiles.filter(!_.getName.startsWith(".")).foreach(delete)
              }
              f.delete
            }
            println("Clearing templates...")
            delete(StoryLoader.templateLocation)
          case _ => 
            runStory(recursively, singleInput, None, tempArgs)
        }
        case Array(firstInput, secondInput) => firstInput match {
          case "template-args" =>
            StoryLoader.loadTemplate(secondInput).map {
              template =>

              def formatKey(arg: StoryMetaKey[_]) {
                val req = if (arg.required) "[REQUIRED]" else ""

                println("  %1$-20s  %2$s %3$s".format(arg.key, arg.description, req)) 
              }

              println("Default Keys:")

              template.defaultKeys.foreach(formatKey)

              if (!template.templateKeys.isEmpty) {
                println
                println("%s Keys:".format(secondInput))

                template.templateKeys.foreach(formatKey)
              }
            }
          case "install" =>
            // TODO: download / install
          case "uninstall" =>
            // TODO: delete / uninstall
          case _ => runStory(recursively, secondInput, Some(firstInput), tempArgs)
        }
      } 
    }
  }

  def runStory(recurse: Boolean, md: String, template: Option[String], 
               args: Array[String] = Array[String]()) {
    import java.io.File
    import StoryKeys.defaultTemplate

    val emptyMode = StoryMode(Nil)

    val configs = List[ConfigFile](
      GlobalFile, 
      LocalFile, 
      MdFile(new File(md))
    )

    val mode = configs.foldLeft(emptyMode) { (in, config) =>
      in ++ config.loadMode.getOrElse(emptyMode)
    }

    val temp = template.getOrElse(mode.getOrElse(defaultTemplate, "default"))

    StoryLoader.loadTemplate(temp).map { t =>

      val usedMode = t.mode ++ mode

      val converter = Converter(t.mode ++ mode)

      val book = converter.fromFile(configs.last.input.getAbsolutePath)
      
      t(book)
    }

    println("Done")
  }
}

class Storytime extends xsbti.AppMain {
  def run(configuration: xsbti.AppConfiguration) = {
    Storytime.main(configuration.arguments)
    Exit(0)
  }

  case class Exit(code: Int) extends xsbti.Exit
}
