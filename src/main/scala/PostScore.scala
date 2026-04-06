import Formatters.Post
object PostScore {

    val sumarScores: Option[List[Post]] => Option[Int] =
        postsOpt => postsOpt.map(posts =>
             posts.foldLeft(0)((acc, post) => acc + post._5.getOrElse(0))
       )
}
