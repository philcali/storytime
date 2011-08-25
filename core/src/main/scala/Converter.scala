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

    val splitter = mode.meta.get("sep").map { sep =>
      val sepstr = sep.toString

      new StoryPreprocessor with Separator {
        val key = sepstr
        def create(contents: String) = contents.split(sepstr)
      }
    } getOrElse Pages

    val pages = splitter create preprocessed map (knockoff(_)) 

    StoryBook(mode, pages.zipWithIndex.map { 
      case (ps, n) => toPage(ps, n)
    }) 
  }

  private def toPage(blocks: Seq[Block], number: Int) = {
    new StoryPage(blocks, toXHTML(blocks), number + 1)
  }
}

case class StoryBook(mode: StoryMode, pages: Seq[StoryPage])

trait Separator extends StoryPreprocessor {
  def create (contents: String): Array[String]
  def preprocess (contents: String) = ""
}

object Pages extends StoryPreprocessor with Separator {
  val key = "page"
  
  def create(contents: String) = reg.split(contents)
}
