import scala.io.Source
import scala.util.Using
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {
  type Subscription = (String, String)
  implicit val formats: Formats = DefaultFormats
  
  // funcion para leer una sola suscripcion
  private def readSubscription(item: JValue): Option[Subscription] =
    for {
      name <- (item \ "name").extractOpt[String]
      url  <- (item \ "url").extractOpt[String]
    } yield (name, url)


  // revisar caso donde el JSON es un array vacio
  def readSubscriptions(path: String): Option[List[Subscription]] = {
    try {
      Using.resource(Source.fromFile(path)) { source =>
        val json = parse(source.mkString)
        // extractOpt[List[JValue]] da None si el root no es un array
        json.extractOpt[List[JValue]].map { items =>
          items.flatMap(readSubscription)  // filtra suscripciones invalidas
        }
      }
    } catch {
      case e: Exception => None
    }
  }
  
  // Pure function to download JSON feed from a URL
  def downloadFeed(url: String): Option[String] = {
    Using(Source.fromURL(url)) { source =>
      source.mkString
    }.toOption
  }
}

