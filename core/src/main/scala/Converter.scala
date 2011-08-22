package storytime

import scala.io.Source.{fromFile => open}
import com.tristanhunt.knockoff.{Block}

object Converter {
  def apply(mode: StoryMode) = new Converter(mode)
}

class Converter private (val mode: StoryMode) extends StoryDiscounter {

  def convert(mdLocation: String) = {
    val contents = open(mdLocation).getLines.mkString("\n")

    val preprocessed = 
      mode.preprocessors.foldLeft(contents) { (in, pre) =>
        pre.process(in)
      }

    val pages = Pages create preprocessed map (knockoff(_)) 

    StoryBook(mode, pages.map(toPage)) 
  }

  private def toPage(blocks: Seq[Block]) = {
    new StoryPage(blocks, toXHTML(blocks))
  }
}

case class StoryBook(mode: StoryMode, pages: Seq[StoryPage])

object Pages extends StoryPreprocessor {
  val key = "page"
  
  def create(contents: String) = reg.split(contents)
  def preprocess(contents: String) = ""
}
