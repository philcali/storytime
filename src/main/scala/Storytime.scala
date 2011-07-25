package storytime

class Storytime extends xsbti.AppMain {
  def run(configuration: xsbti.AppConfiguration) = {
    println("hello bob")
    Exit(0)
  }

  case class Exit(code: Int) extends xsbti.Exit
}
