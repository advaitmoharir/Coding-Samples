### R code from vignette source 'Final.Rnw'

###################################################
### code chunk number 1: Final.Rnw:58-70
###################################################
#Downloading the packages

library(gutenbergr)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(wordcloud)
library(forcats)
library(topicmodels)


###################################################
### code chunk number 2: Final.Rnw:76-82
###################################################
#Corpus of economics books

classics<-gutenberg_download(c(3300,30107,33310, 46423),
                             mirror=
                "http://eremita.di.uminho.pt/gutenberg/",
                             meta_fields = "author")


###################################################
### code chunk number 3: Final.Rnw:93-99
###################################################

#Zipf's Law
#calculating frequency
bookwords <- classics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)


###################################################
### code chunk number 4: Final.Rnw:103-108
###################################################
total_words <- bookwords %>% 
  group_by(author) %>% 
  summarize(total = sum(n))

bookwords <- left_join(bookwords, total_words)


###################################################
### code chunk number 5: Final.Rnw:112-119
###################################################
#Calculating rank

freq_by_rank <- bookwords %>% 
  group_by(author) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()


###################################################
### code chunk number 6: Final.Rnw:124-135
###################################################

#Plotting Zipf's Law

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = author)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10(
    
  )+labs(caption="Figure 1: Zipf's Law for classical economics")+
  theme( plot.caption = element_text(hjust = 0))


###################################################
### code chunk number 7: Final.Rnw:143-153
###################################################
#Getting tf-idf and setting authors as factors
tf_idf_econ <- classics %>%
  unnest_tokens(word,text)%>%
  count(author, word, sort = TRUE)%>%
  bind_tf_idf(word, author, n) %>%
  mutate(author = factor(author, levels = c("Smith, Adam",
                                            "Mill, John Stuart", 
                                            "Ricardo, David",
                                            "Marx, Karl")))



###################################################
### code chunk number 8: Final.Rnw:158-178
###################################################
#Cleaning up df by removing stopwords
mystopwords <- tibble(word= c("2", "4", "100", "1000",
                               "8", "10", "720","1883", 
                               "1878", "1873", "1880",
                               "1879", "0", "1882", "1884",
                               "pp", "4_I", "4_l", "100_l",
                               "1000_l", "8_s", "720_s", "10_s",
                               "3_l", "720_l", "10,000_l",
                               "50_l", "_on", "metamorphosis", 
                               "tokens", "qrs", "5_l", "2000_l",
                               "5000_l", "whilst","laborers", "labourers", 
                              "8_d",
                              "g", "etc", "l", 
                              "20,000_l", "..a...", 
                              "1870", "8vo", "900_l",
                              "9_s", "4_s", "2_s",
                              "200_l", "1,000", "laboring", "laborer", "labourer"))

econ_cleaned<-anti_join(tf_idf_econ, mystopwords,
                         by="word")


###################################################
### code chunk number 9: Final.Rnw:182-194
###################################################
#Plotting 15 most important terms for each author
econ_cleaned %>% 
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~author, ncol = 2, scales = "free")+
  labs(caption="Figure 2: Top 15 words by tf-idf")+
  theme( plot.caption = element_text(hjust = 0))


###################################################
### code chunk number 10: Final.Rnw:205-211
###################################################
data("stop_words")

classic_words <- classics %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  count(author, word, sort = TRUE)


###################################################
### code chunk number 11: Final.Rnw:216-222
###################################################
#Setting authors as factors
classic_words%>%
  mutate(author = factor(author, levels = c("Smith, Adam",
                                            "Mill, John Stuart", 
                                            "Ricardo, David",
                                            "Marx, Karl")))


###################################################
### code chunk number 12: Final.Rnw:227-228
###################################################
dtm<-classic_words%>% cast_dtm(author, word, n)


###################################################
### code chunk number 13: Final.Rnw:235-242
###################################################
#Implementing the LDA

classic_lda<-LDA(dtm, k=4, control=set.seed(1234))

#Extracting 3 topics from LDA using tidy

classic_topics<-tidy(classic_lda, matrix="beta")


###################################################
### code chunk number 14: Final.Rnw:247-264
###################################################
#Plotting top 10 terms from each topic
classic_top_terms <- classic_topics %>%
  group_by(topic) %>%
  slice_max(beta, n =5 ) %>% 
  ungroup() %>%
  arrange(topic, -beta)

classic_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=2, scales = "free") +
  scale_y_reordered()+
  labs(caption="Figure 3: Top 5 words by beta per topic")+
  theme( plot.caption = element_text(hjust = 0))




