import Formatters.Post
object PostScore {

    val sumarScores: List[Post] => Int =
        posts => posts.foldLeft(0)((acc, post) => acc + post._5.getOrElse(0))
}