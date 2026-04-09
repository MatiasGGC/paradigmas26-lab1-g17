import FileIO.Subscription
import Formatters.Post


object Informe {

  def renderSubscription(
      subscription: Subscription,
      posts: List[Post],
      topN: Int = 10
  ): String = {

    val (name, url) = subscription

    val totalScore = PostScore.sumarScores(Some(posts)).getOrElse(0)
    val topWords = Ejercicio5.wordFrequencies(Some(posts)).getOrElse(List()).take(topN)
    val top5Posts  = posts.take(5)

    val seccionScore =
      s"## 1. Score Total\n" +
      s"**$totalScore puntos** acumulados en ${posts.length} posts.\n"

    val seccionPalabras =
      "## 2. Palabras Más Frecuentes\n" +
        (if (topWords.isEmpty)
           "_No se encontraron palabras relevantes._\n"
         else
           "| Palabra | Ocurrencias |\n" +
             "|---------|-------------|\n" +
             topWords
               .map { case (word, count) => s"| $word | $count |" }
               .mkString("\n") + "\n"
        )

    val seccionPosts =
      "## 3. Primeros 5 Posts\n" +
        (if (top5Posts.isEmpty)
           "_No hay posts disponibles._\n"
         else
           top5Posts.zipWithIndex
             .map { case (post, idx) =>
               val title = post._2.getOrElse("Sin título")
               val date  = post._4.getOrElse("Fecha desconocida")
               s"${idx + 1}. **$title** — $date"
             }
             .mkString("\n") + "\n"
        )

    List(
      s"# $name\n",
      seccionScore,
      seccionPalabras,
      seccionPosts,
      "---"
    ).mkString("\n")
  }

  def printReport(
      subscriptions: List[Subscription],
      allPosts: Map[String, List[Post]]
  ): Unit =
    println(
      "# Informe de Suscripciones\n\n" +
        subscriptions
          .map { case (name, url) =>
            renderSubscription((name, url), allPosts.getOrElse(name, List.empty))
          }
          .mkString("\n")
    )
}
