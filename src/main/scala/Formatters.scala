import org.json4s._
import org.json4s.jackson.JsonMethods._
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}


object Formatters {
  type Post = (Option[String], Option[String], Option[String], Option[String], Option[Int], Option[String]) 

  implicit val formats: Formats = DefaultFormats

  private def formatDate(seconds: Option[Long]): Option[String] = seconds match {
    case Some(seconds) => {
      val millis = seconds * 1000
      val instant = Instant.ofEpochMilli(millis)
      val created_utc = ZonedDateTime.ofInstant(instant, ZoneId.of("UTC"))
      val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss")
      Some(created_utc.format(formatter))
    }
    case None => None
  }

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
                formatDate((data \ "created_utc").extractOpt[Double].map(_.toLong)), 
                (data \ "score").extractOpt[Int], 
                (data \ "permalink").extractOpt[String]
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
