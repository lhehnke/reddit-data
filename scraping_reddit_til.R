##-----------------------------------------------------------------------------------##
##                     ALL THINGS WEIRD: SCRAPING REDDIT THREADS                     ##
##-----------------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Author: Lisa Hehnke || lhehnke.github.io || @DataPlanes


#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(magrittr, RedditExtractoR, reshape2, tidytext, tidyverse, wordcloud)


#---------------------------#
# Scraping data from Reddit #
#---------------------------#

# Get thread URLs in subreddit
links <- reddit_urls(subreddit = "todayilearned", page_threshold = 10, sort_by = "relevance")
#saveRDS(links, "links.rds")

# Find most commented threads and extract selected URL
links %<>% arrange(desc(num_comments))
url <- links[2, "URL"]

# Get comments for selected thread
comments <- reddit_content(url)
#saveRDS(comments, "comments.rds")


#----------------#
# Data wrangling #
#----------------#

# Extract comments
comments_tidy <- comments$comment 

# Remove numbers and punctuations and convert to lowercase
comments_tidy %<>%
  gsub("[^[:alpha:][:blank:]']", "", .) %>%
  tolower()

# Split strings and convert to data frame
comments_tidy %<>%
  strsplit(., " ") %>%
  unlist() %>%
  data.frame() 
colnames(comments_tidy) <- "word"

# Remove blanks
comments_tidy %<>% filter(word != "") 

# Remove stopwords
comments_tidy %<>% anti_join(stop_words)


#-------#
# Theme #
#-------#

# Set theme for visualizations
viz_theme <- theme(
  strip.background = element_rect(colour = "transparent", fill = "grey90"),
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  strip.text = element_text(size = rel(1), face = "bold"),
  plot.caption = element_text(colour = "grey50"),
  text = element_text(family = "Avenir"))


#------------------#
# Word frequencies #
#------------------#

# Find most common words
comments_wordfreq <- comments_tidy %>%
  count(word, sort = TRUE)

# Plot words
comments_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  theme(text = element_text(size = 30)) + 
  xlab("") + ylab("") + ggtitle("Most common words in Reddit thread", subtitle = " ") +
  ylim(0, 60) + coord_flip() + viz_theme 

ggsave("plot_words.png", width = 12, height = 8, units = "in", dpi = 100)


#--------------------#
# Sentiment analysis #
#--------------------#

# Calculate and plot total sentiment scores (nrc)
comments_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  ggplot(aes(sentiment, n)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(text = element_text(size = 30), axis.text.x = element_text(angle = 65, vjust = 0.5)) +
  xlab("") + ylab("") + ggtitle("Total sentiment scores in Reddit thread", subtitle = " ") +
  ylim(0, 500) + theme(legend.position = "none") + viz_theme 

ggsave("plot_sentiments.png", width = 12, height = 8, units = "in", dpi = 100)


#-------------------------#
# Positive/negative words #
#-------------------------#

# Calculate positive and negative sentiments (bing)  
bing_counts <- comments_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Calculate top word contributors
bing_counts_plot <- bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 

# Plot most common positive and negative words
ggplot(bing_counts_plot, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab("") + ylab("") + 
  theme(text = element_text(size = 30)) + 
  ggtitle("Most common +/- words in Reddit thread", subtitle = " ") +
  coord_flip() + viz_theme

ggsave("plot_pos_neg_words.png", width = 12, height = 8, units = "in", dpi = 100)


#-----------#
# Wordcloud #
#-----------#

# Plot comparison cloud
png("wordcloud.png", width = 3.5, height = 3.5, units = 'in', res = 300)
comments_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 60)
dev.off()


#----------------#
# Lesson learned #
#----------------#

# View title
comments[1, "title"]
