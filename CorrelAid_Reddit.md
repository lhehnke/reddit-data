---
layout: post
title: "Guess the Lesson: Scraping and mining Reddit comments from r/todayilearned"
date: "February 13, 2018"
type: post
published: false
status: publish
categories:
- text mining
- rstats
- sentiment analysis
tags:
- rstats
- text mining
- reddit
- todyilearned
---

Inspiration for this post hit me when our editor-in-chief Yannik told me he wants *“alles Kuriose”* (meaning all things weird) for future blog posts. Well, challenge accepted. 

After a short debate with myself on where to best find weird-but-not-too-weird things on the internet, I stumbled upon <a href="https://www.reddit.com/">Reddit</a>. At first, the initial idea was to search all subreddits for "weird" threads (honorable mention to <a href="https://www.reddit.com/r/dataisugly/">r/dataisugly/</a> and <a href="https://www.reddit.com/r/counting/">r/counting/</a>) and then analyze some comments. But since education is one of the main objectives of CorrelAid, I wanted to incorporate a learning component into this post which led me – thanks to the help of our fellow CorrelAider Josef – to the <a href="https://www.reddit.com/r/todayilearned/">r/todayilearned/</a> subreddit. In case you didn’t already guess from the name of the subreddit, it contains various things the contributors learned today. 

This post now works as follows: I will demonstrate how to scrape text data from this subreddit and mine the comments from one particular thread but without revealing the name of the thread. And you have to guess what the thread opener learned today based on the results of the text analysis.

## Setup

Reproducing this analysis requires the packages listed below. You can install and load them at once with `p_load()`, a wrapper function for `library()` and `require()` from the `pacman` package.

``` r
# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(RedditExtractoR, magrittr, reshape2, tidytext, tidyverse, wordcloud)
```

## Scraping data from Reddit

`RedditExtractoR` provides an easy way to access Reddit comments and statistics. For downloading the comments of a single thread in the `/r/todayilearned/` subreddit you can use `reddit_url()` to get the URLs of all threads, extract e.g. the most commented thread (I intentionally chose the second most commented thread), and then download the comments with `reddit_content()`.

Hence, to load the data into `R`, simply run

```r
# Get thread URLs in subreddit
links <- reddit_urls(subreddit = "todayilearned", page_threshold = 10, sort_by = "relevance")

# Find most commented threads and extract selected URL
links %<>% arrange(desc(num_comments))
url <- links[2, "URL"]

# Get comments for selected thread
comments <- reddit_content(url)
```

Please note that I scraped the comments on February 12 and since this subreddit is quite active, you'll most likely get different results than the ones analyzed in this post. For replication purposes, you can download the original data <a href="https://github.com/lhehnke/reddit-data">here</a>.


## Text cleaning

After extracting the raw comments from Reddit, some data cleaning needed to be done first before mining the text. 

In this case, data cleaning translated to removing punctuation (except for apostrophes), non-alphabetic characters, blank lines and stopwords from the text. In addition, the comments -- which are made up of a sequence of strings -- were split into single words. This process is called *tokenization* in language processing.

```r
# Extract comments
comments_tidy <- comments$comment 

# Remove numbers and punctuation and convert to lowercase
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
```

## Visualizing word frequencies

After processing the raw text and turning it into a tidy format, the first step of the analysis consisted of calculating word frequencies and extracting the most common words by running the following code:

```r
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

```  

<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/reddit-data/master/img/plot_words.png" width="550px" height="500px" vspace="50px"/></p>

As you can see in the above plot, *sound(s)* (you can prevent such word duplicates by stemming them beforehand using the `SnowballC` package), *people*, and *golf* were the most common words in the thread. 

Any guesses yet on what the thread opener learned? 

## Conducting a sentiment analysis

For the next step of the analysis, I conducted a basic sentiment analysis with `get_sentiments()` from the `tidytext` package and visualized the results. More specifically, I used the `NRC Emotion Lexicon` by Saif Mohammad and Peter Turney which categorizes words into positive and negative categories as well as in anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

```r
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
```

<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/reddit-data/master/img/plot_sentiments.png" width="550px" height="500px" vspace="50px"/></p>

According to the `NRC` sentiment analysis, most words in the comments are positively scored, followed by negatively scored words.

## Extracting positive and negative words

After obtaining the results of the first sentiment analysis, I decided to dig deeper into the previous finding by extracting the most common positive and negative words using the `Bing` sentiment lexicon by Bing Liu and collaborators.

```r
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
  ```
  
<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/reddit-data/master/img/plot_pos_neg_words.png" width="650px" height="500px" vspace="50px"/></p>

As the second sentiment graph shows, the most common negative words in the thread were *noise(s)*, *fake*, and *dead*, whereas the positive words that occurred most often were *quiet*, *pretty*, and *top*/*silent*/*audible*.
  
## Creating a word cloud

To finish this brief text analysis up, I created a slightly more advanced word cloud that contrasts the most common positive words with the most common negative ones.
  
```r
# Plot comparison cloud
png("wordcloud.png", width = 3.5, height = 3.5, units = 'in', res = 300)
comments_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 60)
dev.off()
```

<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/reddit-data/master/img/wordcloud.png" width="550px" height="500px" vspace="50px"/></p>


## Today I learned...

Any final guesses before I reveal the actual lesson that both the thread opener and we learned today?

3...

2...

1...

```r
# View title
comments[1, "title"]
```

> "TIL: CBS used to add bird songs to their golf broadcasts to get rid of awkward silences until they got caught by someone watching at home who knew the bird songs belonged to birds that didn't live in the region in which the golf tournament was being played."

Did you guess correctly? For more things to learn visit <a href="https://www.reddit.com/r/todayilearned/">Reddit</a>.
