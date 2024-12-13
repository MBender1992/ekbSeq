#' Function to plot and save
#'
#' Text is searched for common terms and a wordcloud is generated based on frequency.
#'
#' @param text Character vector containing a list of pathway strings.
#' @export

# library(tm)
# library(SnowballC)

plot_wordcloud <- function(text){
  freq <- NULL

  docs <- Corpus(VectorSource(text))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix),decreasing=TRUE)
  x <- names(words)
  Encoding(x) <- 'latin1'
  names(words) <- x
  df <- data.frame(word = names(words), freq=words)
  ggplot(df, aes(label = word, size = freq, color = word)) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = 50, trans = power_trans(1/.7)) +
    theme_minimal() +
    scale_color_viridis_d()
}
