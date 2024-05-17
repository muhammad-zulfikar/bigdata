# Load required packages for web scraping, text mining, and visualizing
library(rvest)
library(stringr)
library(tidytext)
library(tm)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(dplyr)

# Specify the base URL of the website
base_url <- "https://www.tidytextmining.com"

# Read the main webpage
webpage <- read_html(base_url)

# Extract links from the table of contents
toc_links <- webpage %>%
  html_nodes("nav a") %>%
  html_attr("href")

# Filter out any relative links and create full URLs
toc_links <- toc_links[grepl("^/", toc_links)]
page_urls <- paste0(base_url, toc_links)

# Initialize a list to store scraped content
all_page_content <- list()

# Loop through each page URL and scrape content
for (full_url in page_urls) {
  cat("Scraping content from:", full_url, "\n")
  
  # Read the webpage HTML
  webpage <- read_html(full_url)
  
  # Extract content within the id="content" section using CSS selector
  content_section <- html_nodes(webpage, css = "#content")
  
  # Convert the extracted content to text
  content_text <- html_text(content_section)
  
  # Remove <script> and <style> tags and their content using regular expressions
  cleaned_content <- str_replace_all(content_text, "<script.*?</script>|<style.*?</style>", "")
  
  # Remove extra whitespace and line breaks
  final_content <- str_squish(cleaned_content)
  
  # Store the scraped content in the list
  all_page_content[[full_url]] <- final_content
}

# Combine all scraped content into a single data frame
all_content_df <- data.frame(page_url = names(all_page_content), content = unlist(all_page_content))

# Calculate total word count across all scraped content
total_word_count <- sum(nchar(all_content_df$content) - nchar(gsub("\\s+", "", all_content_df$content)))

# Print the total word count across all scraped pages
cat("\n")
cat("Total word count:", total_word_count, "\n")

# Save the combined content data frame as CSV
# write.csv(all_content_df, file = "all_content.csv", row.names = FALSE)


# Clean the corpus: convert to lowercase, remove punctuation, numbers, and stopwords
corpus <- Corpus(VectorSource(all_content_df$content))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert the term-document matrix to a matrix
m <- as.matrix(tdm)

# Calculate word frequencies and filter out non-word characters
word_freq <- sort(rowSums(m), decreasing = TRUE)
valid_words <- grep("^[a-zA-Z]+$", names(word_freq), value = TRUE)

# Generate word cloud based on filtered word frequencies
# png("wordcloud_output.png", width = 1200, height = 800, res = 150)
wordcloud(words = valid_words, freq = word_freq[valid_words], min.freq = 10, 
          random.order = FALSE, colors = brewer.pal(8, "Dark2"))
dev.off()


# Select the top N words by frequency for the bar plot
top_n <- 10
top_words <- head(valid_words, n = top_n)
word_counts <- word_freq[top_words]

# Create a data frame for the bar plot
bar_data <- data.frame(word = top_words, frequency = word_counts)

# Sort the bar_data by frequency in descending order
bar_data <- bar_data[order(-bar_data$frequency), ]

# Generate the vertical bar plot using ggplot2
ggplot(bar_data, aes(x = reorder(word, frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top", top_n, "Words by Frequency"),
       x = "Word", y = "Frequency") +
  theme_minimal() +
  coord_flip()

# Save the ggplot bar plot as PNG
# ggsave("barplot_output.png", width = 12, height = 8, dpi = 150)



# Tokenize the corpus for sentiment analysis
corpus_tokens <- all_content_df %>%
  mutate(content = tolower(content)) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words)

# Perform sentiment analysis using Bing lexicon
sentiment_scores <- corpus_tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

# Filter sentiment_scores for top 10 words by sentiment_score
top_sentiment_words <- sentiment_scores %>%
  top_n(10, abs(sentiment_score)) %>%
  mutate(word = reorder(word, sentiment_score))

# Plot sentiment analysis results for top 10 words
ggplot(top_sentiment_words, aes(x = word, y = sentiment_score, fill = sentiment_score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("red", "green"), guide = FALSE) +
  labs(title = "Sentiment Analysis (Top 10 Words)",
       x = "Word", y = "Sentiment Score") +
  theme_minimal() +
  coord_flip()


# Extract bigrams from the corpus
bigrams <- corpus_tokens %>%
  filter(!is.na(lead(word))) %>%
  mutate(bigram = paste(word, lead(word), sep = " ")) %>%
  count(bigram, sort = TRUE)

# Visualize top bigrams
top_bigrams <- bigrams %>%
  filter(n > 10) %>%
  slice_max(order_by = n, n = 10)

# Plot top bigrams
ggplot(top_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_col() +
  labs(title = "Top Bigrams",
       x = "Bigram", y = "Frequency") +
  theme_minimal() +
  coord_flip()