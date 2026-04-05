import org.json4s._
import org.json4s.jackson.JsonMethods._

object Formatters {
  type Post = (String, String, String, String) // subreddit, title, selftext, created

  implicit val formats: Formats = DefaultFormats

  // Pure function to format posts from a subscription
  def formatSubscription(url: String, posts: String): List[Post] =
    (parse(posts) \ "data" \ "children")
    .extract[List[JValue]]
    .map { post => 
    ((post \ "data" \ "subreddit").extract[String],
    (post \ "data" \ "title").extract[String],
    (post \ "data" \ "selftext").extract[String],
    (post \ "data" \ "created_utc").extract[String])
    //(TextProcessing.formatDateFromUTC((post \ "data" \ "created_utc").extract[Double].toLong)))
    }
}
