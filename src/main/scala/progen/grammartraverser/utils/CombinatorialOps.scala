package progen.grammartraverser.utils

/**
  * Credits to https://kostyukov.net/posts/combinatorial-algorithms-in-scala/
  */
object CombinatorialOps {

  implicit class CombinatorialList[A](l: List[A]){

    def combinations(n: Int): List[List[A]] =
      if (n > l.size) Nil
      else l match {
        case _ :: _ if n == 1 =>
          l.map(List(_))
        case hd :: tl =>
          (tl.combinations(n - 1).map(hd :: _) ++ tl.combinations(n)).toList
        case _ => Nil
      }

  }

}
