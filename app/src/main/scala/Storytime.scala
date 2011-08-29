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
          -r: recursively"""
    )
  }

  def main (args: Array[String]) {
    if (args.contains("-h") || args.length == 0) printHelp()
    else { 
      val recursively = args.contains("-r")

      val (options, rest) = args.partition(_.startsWith("-"))

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
        }
        case Array(firstInput, secondInput) => firstInput match {
          case "template-args" =>
            StoryLoader.loadTemplate(secondInput).fold(println, {
              _.arguments.foreach { arg =>
                println("  %s: %s".format(arg.key, arg.description))        
              }
            })
          case "install" =>
          case "uninstall" =>  
          case _ => 
            StoryLoader.loadTemplate(firstInput).fold(println, { template =>
              val converter = Converter(template.mode)
              
              val book = converter.convert(secondInput)

              template(book)
            })
        }
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
