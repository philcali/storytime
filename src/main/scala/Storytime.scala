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
          
        General actions:

          [options] [template-name] <markdown directory or file> [template-args]

        General options:
          
          -h: prints this help
          -l: list commands
          -r: recursively"""
    )
  }

  def main (args: Array[String]) {
    if (args.contains("-h") || args.length == 0) printHelp()
    else if (args.contains("-l")) {
      StoryLoader.listTemplates match {
        case Some(templates) =>
          templates.foreach(f => println(f.getName))
        case None => println("No templates installed.")
      }
    } else {
      val recursively = args.contains("-r")

      val (options, rest) = args.partition(_.startsWith("-"))

      rest match { 
        case Array(template, markdown) => DefaultTemplate(Converter(markdown))
        case Array(markdown) => DefaultTemplate(Converter(markdown)) 
        case _ => println("Provide a template and a markdown file")
      }
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
