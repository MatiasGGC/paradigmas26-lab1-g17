import org.json4s._
import org.json4s.jackson.JsonMethods._
import java.time

object Formatters {
  // Definición del tipo Post para mayor claridad
  type Post = (Option[String], Option[String], Option[String], Option[String], Option[Int]) 

  implicit val formats: Formats = DefaultFormats

  /**
   * Formatea los posts de una suscripción.
   * Retorna Some(List) si el JSON es válido, o None si hay un error crítico en el parseo.
   */
  def formatSubscription(url: String, posts: String): Option[List[Post]] = {
    try {
      // Parseamos el JSON y navegamos hasta la lista de hijos (children)
      val json = parse(posts)
      val children = (json \ "data" \ "children").extractOpt[List[JValue]]

      // Preparación para darle formato a la fecha
      

      children.map { list =>
        list.flatMap { child =>
          try {
            // Extraemos los datos de cada post individualmente
            val data = child \ "data"
            val post: Post = (
              (data \ "subreddit").extractOpt[String],
              (data \ "title").extractOpt[String],
              (data \ "selftext").extractOpt[String],
              (data \ "created_utc").extractOpt[Double].map(_.toLong.toString), 
              (data \ "score").extractOpt[Int]
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
  }
}