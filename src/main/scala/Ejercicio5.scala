
object Ejercicio5 {

        val stopwords: Set[String] = Set("the", "about", "above", "after", "again", "against", "all", "am", "an",
        "and", "any", "are", "aren't", "as", "at", "be", "because", "been",
        "before", "being", "below", "between", "both", "but", "by", "can't",
        "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't",
        "doing", "don't", "down", "during", "each", "few", "for", "from", "further",
        "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd",
        "he'll", "he's", "her", "here", "here's", "hers", "herself", "him",
        "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if",
        "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me",
        "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off",
        "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves",
        "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's",
        "should", "shouldn't", "so", "some", "such", "than", "that", "that's",
        "the", "their", "theirs", "them", "themselves", "then", "there", "there's",
        "these", "they", "they'd", "they'll", "re", "they've", "this", "those",
        "through", "to", "too", "under", "until", "up", "very", "was", "wasn't",
        "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what",
        "what's", "when", "when's", "where", "where's", "which", "while", "who",
        "who's", "whom", "why", "why's", "with", "won't", "would",
        "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours",
        "yourself", "yourselves"
    )
    
    def wordFrequencies(postsOpt: Option[List[Formatters.Post]]): Option[List[(String, Int)]] = {

        postsOpt.map { posts =>

            val texts = posts.flatMap {
                case (_, Some(title), Some(selftext), _, _, _) =>
                    Some(title + " " + selftext)
                case _ => None
}

            val words = texts.flatMap(_.split("\\W+"))

            val filtered = words.filter { w =>
                w.nonEmpty &&
                w.head.isUpper &&
                !stopwords.contains(w.toLowerCase)   
            }

            val frequencies = filtered.groupBy(_.toLowerCase)
                .map{case (word, instances) => (word.capitalize, instances.size)}
                .toList
                .sortBy(-_._2) // Ordena por frecuencia descendente

            frequencies
        }
    }
}
