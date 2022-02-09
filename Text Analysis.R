#This code uses tf-idf and topic modelling to identify 
#common themes among four important texts written by
#classical economists. The code extracts the raw text
#from project Gutenberg, does the analysis and
#visualizes the results.

#Uploading the packages

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


#From the package gutenbergr, I first download the four
#texts namely, An Inquiry into the Nature and Causes of the Wealth of Nations by Adam
#Smith, A Contribution to the Critique of Political Economy by Karl Marx,  
#On the Principles of Political Economy and Taxation by David Ricardo, and
#Principles of Political Economy by John Stuart Mill.

classics<-gutenberg_download(c(3300,30107,33310, 46423),
                             mirror=
                "http://eremita.di.uminho.pt/gutenberg/",
                             meta_fields = "author")

#We check the texts if the adhere to 
#Zipf's Law, a power law principle

#calculating frequency of each word, sorted by author
bookwords <- classics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)

total_words <- bookwords %>% 
  group_by(author) %>% 
  summarize(total = sum(n))

bookwords <- left_join(bookwords, total_words)



#Calculating rank and term frequency.

freq_by_rank <- bookwords %>% 
  group_by(author) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()



#Plotting Zipf's Law

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = author)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10(
    
  )+labs(caption="Figure 1: Zipf's Law for classical economics")+
  theme( plot.caption = element_text(hjust = 0))


#Getting tf-idf and setting authors as factors
tf_idf_econ <- classics %>%
  unnest_tokens(word,text)%>%
  count(author, word, sort = TRUE)%>%
  bind_tf_idf(word, author, n) %>%
  mutate(author = factor(author, levels = c("Smith, Adam",
                                            "Mill, John Stuart", 
                                            "Ricardo, David",
                                            "Marx, Karl")))


#Cleaning up df by removing stopwords. Stopwords are unimportant words which
#dont convey useful info. I remove these from the corpus.
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

#Removing stop words
data("stop_words")

classic_words <- classics %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  count(author, word, sort = TRUE)


#Setting authors as factors
classic_words%>%
  mutate(author = factor(author, levels = c("Smith, Adam",
                                            "Mill, John Stuart", 
                                            "Ricardo, David",
                                            "Marx, Karl")))



dtm<-classic_words%>% cast_dtm(author, word, n)


#Implementing the LDA. This is an algorithm key to topic modelling.

classic_lda<-LDA(dtm, k=4, control=set.seed(1234))

#Extracting 3 topics from LDA using tidy

classic_topics<-tidy(classic_lda, matrix="beta")


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




