package storytime

import scala.io.Source.{fromFile => open}
import com.tristanhunt.knockoff.{Block}

import StoryKeys.{ preprocessors, separator }

object Converter {
  def apply(mode: StoryMode) = new Converter(mode)

  def stripMeta(contents: String) = {
    MetaStripper create contents match {
      case Array(definedMeta, markdown) => markdown
      case _ => contents
    }
  }
}

class Converter private (val mode: StoryMode) extends StoryDiscounter {

  def fromFile(mdLocation: String) = {
    val contents = open(mdLocation).getLines.mkString("\n")

    convert(contents)
  }

  def convert(contents: String) = {
    val stripped = Converter.stripMeta(contents)

    val preprocessed = 
      mode.getOrElse(preprocessors, Nil).foldLeft(stripped) { (in, pre) =>
        pre.process(in)
      }

    val splitter = mode.get(separator).map { sep =>
      new StoryPreprocessor with Separator {
        val key = sep 
        def create(contents: String) = key.r.split(contents)
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

trait Splitter extends Separator {
  def create (contents: String) = reg.split(contents)
}

object Pages extends StoryPreprocessor with Splitter {
  val key = "page"
}

object MetaStripper extends StoryPreprocessor with Splitter {
  val key = "meta"
}
