package storytime

trait Keys {
  implicit def overwriteKey[A](key: StoryMetaKey[A]) = new OverwriteKey[A](key)

  implicit def addSeq[A](key: StoryMetaKey[Seq[A]]) = new AddSeq[A](key)

  implicit def metaSeqtoMode(seq: Seq[Meta[_]]) = StoryMode(seq)

  val paginate = StoryMetaKey[Boolean]("paginate", 
    "Splits up a project into multiple files")

  val embed = StoryMetaKey[Boolean]("embed", 
    "Tries to embed resources into converted output")
 
  val namedFile = StoryMetaKey[Boolean]("named-file",
    "Will name the output html, the same name as the input markdown.")
 
  val separator = StoryMetaKey[String]("separator", 
    "This is used to determine the page separators")

  val title = StoryMetaKey[String]("title", "Title of the book")

  val resources = StoryMetaKey[Seq[String]]("resources", 
    "Relative or absolute path(s) to conversion assests")

  val output = StoryMetaKey[String]("output", 
    "Relative or absolute path to an output folder.")

  val templates = StoryMetaKey[Seq[String]]("templates", 
    "Load template macros and processors in scope")

  val macros = StoryMetaKey[Seq[StoryHandler]]("macros",
    "Defines template macros")

  val preprocessors = StoryMetaKey[Seq[StoryPreprocessor]]("preprocessors",
    "Defines template preprocessors")
}

object StoryKeys extends Keys 

