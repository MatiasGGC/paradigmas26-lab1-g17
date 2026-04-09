import org.json4s._
import org.json4s.jackson.JsonMethods._

object Formatters {
  type Post = (Option[String], Option[String], Option[String], Option[String], Option[Int], Option[String]) 

  implicit val formats: Formats = DefaultFormats

  def formatSubscription(url: String, posts: Option[String]): Option[List[Post]] = posts match {
    case Some(posts) =>
      try {
        // Parseamos el JSON y navegamos hasta la lista de hijos (children)
        val json = parse(posts)
        val children = (json \ "data" \ "children").extractOpt[List[JValue]]

        children.map { list =>
          list.flatMap { child =>
            try {
              val data = child \ "data"
              val post: Post = (
                (data \ "subreddit").extractOpt[String],
                (data \ "title").extractOpt[String],
                (data \ "selftext").extractOpt[String],
                (data \ "created_utc").extractOpt[Double].map(_.toLong.toString), 
                (data \ "score").extractOpt[Int], 
                (data \ "permalink".extractOpt[String]
              )
              Some(post)
            } catch {
              // Si un post específico falla, lo ignoramos (flatMap eliminará los None)
              case _: Exception => None
            }
          }
        }
      } catch {
        // Si el JSON entero es inválido o no se puede parsear
        case _: Exception => None
      }
    case None => None
  }
}
