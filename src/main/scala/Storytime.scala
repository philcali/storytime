package storytime

object Storytime {
  def main (args: Array[String]) {
    args match { 
      case Array(markdown) => Output(Converter(markdown))
      case _ => println("Provide a markdown file as input")
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
