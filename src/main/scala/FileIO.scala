import scala.io.Source
import scala.util.Using
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {
  type Subscription = (String, String)

  // construimos una case class conveniente para parsear 
  private case class SubscriptionJson(name: String, url: String)

  // formateo estandar para json4s  
  implicit val formats: Formats = DefaultFormats

  def readSubscriptions(path: String): List[Subscription] =
    Using.resource(Source.fromFile(path)) { source =>
      parse(source.mkString)
        .extract[List[SubscriptionJson]]
        .map(s => (s.name, s.url))
    }
  
  // Pure function to download JSON feed from a URL
  def downloadFeed(url: String): String = {
    val source = Source.fromURL(url)
    source.mkString
  }
}
