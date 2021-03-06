package storytime

case class StoryMetaKey[A](
  key: String, 
  description: String = "",
  required: Boolean = false) extends StoryKey {
}

class OverwriteKey[A](meta: StoryMetaKey[A]) {
  def := (value: A) = new OverwriteMeta[A](meta, value)
}

class AddSeq[A](meta: StoryMetaKey[Seq[A]]) {
  def += (value: A) = new AddSeqMeta[A](meta, Seq(value)) 

  def ++= (values: Seq[A]) = new AddSeqMeta[A](meta, values)
}

case class Meta[A](meta: StoryMetaKey[A], value: A)

trait MetaAction[A] {
  def process(original: A): Meta[A]
}

class OverwriteMeta[A](
  meta: StoryMetaKey[A], 
  value: A
) extends Meta[A](meta, value) with MetaAction[A] {
  def process(original: A) = new OverwriteMeta[A](this.meta, value)
}

class AddSeqMeta[A](
  meta: StoryMetaKey[Seq[A]],
  value: Seq[A]
) extends Meta[Seq[A]](meta: StoryMetaKey[Seq[A]], value) with MetaAction[Seq[A]] {
  def process(original: Seq[A]) =
    new AddSeqMeta[A](this.meta, original ++ this.value)
}
