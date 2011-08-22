package storytime

import scala.io.Source.{fromFile => open}

// TODO: create Meta
object Converter extends StoryDiscounter {
  def apply(mdLocation: String) = {
    val contents = open(mdLocation).getLines.mkString("\n")

    val preprocessed = 
      StoryMode.preprocessors.foldLeft(contents) { (in, pre) =>
        pre.process(in)
      }

    val pages = Pages create preprocessed map (knockoff(_)) 

    pages map (toXHTML(_))
  }
}

object Pages extends StoryPreprocessor {
  val key = "page"
  
  def create(contents: String) = reg.split(contents)
  def preprocess(contents: String) = ""
}
