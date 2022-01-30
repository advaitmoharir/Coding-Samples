#Downloading the packages

library(gutenbergr)
library(tidytext)
library(textdata)
library(janeaustenr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(wordcloud)
library(forcats)
library(topicmodels)

#Corpus of economics books

classics<-gutenberg_download(c(3300,30107,33310, 46423),
                             mirror="http://eremita.di.uminho.pt/gutenberg/",
                             meta_fields = "author")
data("stop_words")

classic_words <- classics %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  count(author, word, sort = TRUE)

#Adding own stop words

mystopwords <- tibble(word= c("2", "4", "100", "1000",
                               "8", "10", "720","1883", 
                               "1878", "1873", "1880",
                               "1879", "0", "1882", "1884",
                               "pp", "4_I", "4_l", "100_l",
                               "1000_l", "8_s", "720_s", "10_s",
                               "3_l", "720_l", "10,000_l",
                               "50_l", "_on", "metamorphosis", 
                               "tokens", "qrs", "5_l", "2000_l",
                               "5000_l", "whilst"))

classic_words<-anti_join(classic_words, mystopwords,
                         by="word")

################SENTIMENT ANALYSIS##############

#Classifying each book by chapter and setting rownumbers
tidy_econ<-classics%>%
  group_by(author) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#Creating an index and calculating net sentiment

classics_sentiment <- tidy_econ %>%
  inner_join(get_sentiments("bing")) %>%
  count(author, index = linenumber %/% 20, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#Plotting evolution of net sentiment for each book

ggplot(classics_sentiment, aes(index, sentiment, fill = author)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free_x")

#Finding which words contribute the most
bing_word_counts <- tidy_econ %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)






###################TF-IDF#######################

#Plotting freq by tf-idf

#Getting tf-idf and setting authors as factors
plot_econs <- classic_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(author = factor(author, levels = c("Smith, Adam",
                                            "Mill, John Stuart", 
                                            "Ricardo, David",
                                            "Marx, Karl")))

#Zipf's Law
#calculating frequency
book_words <- classics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(author) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

#Caculating rank

freq_by_rank <- book_words %>% 
  group_by(author) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

#Plotting Zipf's Law

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = author)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#Plotting 15 most important terms for each author


plot_econs %>% 
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~author, ncol = 2, scales = "free")

######################TOPIC MODELLING############

#Setting authors as factors
classic_words%>%
  mutate(author = factor(author, levels = c("Smith, Adam",
                                            "Mill, John Stuart", 
                                            "Ricardo, David",
                                            "Marx, Karl")))

#Casting df as DocumentTermMatrix
dtm<-classic_words%>% cast_dtm(author, word, n)

#Implementing the LDA

classic_lda<-LDA(dtm, k=2, control=set.seed(1234))

#Extracting 4 topics from LDA using tidy

classic_topics<-tidy(classic_lda, matrix="beta")

#Plotting top 10 terms from each topic
classic_top_terms <- classic_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = ) %>% 
  ungroup() %>%
  arrange(topic, -beta)

classic_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#Checking log-differences



