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
            StoryLoader.listTemplates match {
              case Some(templates) =>
                templates.foreach(f => println(f.getName))
              case None => println("No templates installed.")
            }
          case "clear-templates" =>
            def delete(f: java.io.File) {
              if (f.isDirectory) {
                f.listFiles.filter(!_.getName.startsWith(".")).foreach(delete)
              }
              f.delete
            }
            delete(StoryLoader.templateLocation)
          case "template-args" =>
            
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
