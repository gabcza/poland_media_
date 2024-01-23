#Create text data set
library(tidyverse)
library(foreign)
library(readxl)
library(stringr)
library(naniar)
library(scales)
library(qdapTools)
library(quanteda)
library(quanteda.textstats)

setwd("/Users/ashleyblum/Google Drive/Poland_Media/stimuli/word_versions")

#create list of text files
excerpt_files <- list.files()
excerpt_files <- excerpt_files[!excerpt_files == "Icon\r"]
excerpt_files

#loop through text files to create text data
text <- character(length = length(excerpt_files))
headline <- character(length = length(excerpt_files))

for(i in 1:length(excerpt_files)){
  document <- qdapTools::read_docx(excerpt_files[i])
  document <- document[-1]
  headline[i] <- document[1]
  text[i] <- paste(document, collapse = " ") #note: text also includes headline for easier searching
  print(i)
}

text_df <- tibble(data.frame(headline,text)) %>%
  mutate(excerpt = substr(excerpt_files, 1, 7),
         n_char = nchar(text),
         n_tok = ntoken(text))

setwd("/Users/ashleyblum/Google Drive/Poland_Media/analysis_code")

save(text_df, file = "text_df.RData")

#---- Analysis of article texts ----
library(tidytext)
library(ldatuning)
library(topicmodels)
library(deeplr)
library(ggwordcloud)
library(BTM)
load("text_df.RData")

# clean text data
text_df_tidy <- text_df %>% 
  dplyr::select(-headline) %>%
  mutate(text = gsub("in vitro", "invitro", text)) %>% 
  unnest_tokens(word, text, token = "words") 
nrow(text_df_tidy) # ~25k words

# load stop words 
# source: https://github.com/stopwords-iso/stopwords-pl?tab=readme-ov-file 
stop_words <- read.table("stop_words_polish.txt", encoding="UTF-8")
stop_words <- stop_words %>% rename(word = V1)

# remove words
text_df_tidy <- text_df_tidy %>% 
  anti_join(stop_words, by = "word")  %>% # remove stop words
  filter(!str_detect(word, "[:digit:]")) # remove digits
nrow(text_df_tidy) # ~17k words

# lemmatize words 
# source: http://morfeusz.sgjp.pl/download/
#dic <- read.csv("~/_PROJECTS/_bekker_2022/research/propaganda/data/polimorf-20240107.tab", 
#                sep = "\t", fileEncoding = "UTF-8", skip = 31, header = FALSE)
dic <- read.csv("~/_PROJECTS/_bekker_2022/research/propaganda/data/sgjp-20240107.tab", 
                             sep = "\t", fileEncoding = "UTF-8", skip = 28, header = FALSE)
dic <- dic %>% 
  dplyr::select(-c(V3, V4, V5)) %>%
  rename(word = V1, lemma = V2) %>% 
  group_by(word, lemma) %>% 
  #dplyr::mutate(n = n()) %>%
  slice(1) %>% 
  ungroup() #%>%
dic <- dic %>% anti_join(stop_words, by = "word")
text_df_tidy <- text_df_tidy %>% 
  left_join(dic, by = "word")

# create frequency data (per article)
text_frequency <- text_df_tidy %>%  
  group_by(excerpt, word) %>%
  dplyr::summarise(n = n()) #%>% spread(word, n) 
text_frequency_dtm <- text_frequency %>% cast_dtm(excerpt, word, n) 

# find number of topics 
# (analysis without lemmatization/stemming)
control_list <- list(iter = 500) 
topic_number <- FindTopicsNumber(
    text_frequency_dtm,
    topics = c(5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 80, 100),
    metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = control_list, # I use that many iteration because it should converge quickly (see Gryffiths & Stayers, 2004, PNAS)
    mc.cores = 8L,
    verbose = TRUE)
saveRDS(topic_number, "lda_tuning_t100.rds")
FindTopicsNumber_plot(topic_number)

# run LDA
lda30 <- LDA(text_frequency_dtm, 
              k = 30,
              method = "Gibbs", 
              control = control_list)
saveRDS(lda30, "lda30.rds")

# get top terms and translate to English
lda30_topics <- tidy(lda30, matrix = "beta") # topics
lda30_terms <- lda30_topics %>% 
  group_by(topic) %>% # top terms in topics
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)
# translate top terms
#my_key #= deepL API key here
#lda30_terms$term_EN <- translate2(text = lda30_terms$term, 
#           source_lang = "PL",
#           target_lang = "EN",
#           auth_key = my_key)
#View(lda30_terms)
#saveRDS(lda30_terms, "lda30_terms.rds")
lda30_terms <- readRDS("lda30_terms.rds")
# create wordclouds with top terms
lda30_terms_p <- lda30_terms %>% 
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% # slice further
  ungroup() %>%
  ggplot(aes(label = term_EN)) + # size = beta)) +
  geom_text_wordcloud() +
  facet_wrap(~topic) +
  ggtitle("Top words from 30 topics",
          subtitle = "TVP and TVN excerpts") +
  #scale_size_area(max_size = 20) + 
  theme_minimal()
lda30_terms_p
pdf("lda30_terms.pdf", height = 10, width = 10)
lda30_terms_p
dev.off()

# run BTM (better than LDA for shorter texts)

# prepare dataset for analysis 
# (a tokenised data frame containing one row per token with 2 columns)
text_frequency_btm <- text_frequency %>% 
  select(excerpt, word, n) %>%
  uncount(n) # repeat the rows based on the word count (that format is needed)

# run model
topics_btm30 <- BTM(text_frequency_btm, 
                    k = 30, # number of topics (8/07/2019: increased to 100)
                    alpha = 0.5, # default is 100/k, 
                    beta = 0.01, 
                    iter = 500, 
                    window = 15,
                    background = FALSE, # the first topic is set to a background topic that equals to the empirical word distribution. This can be used to filter out common words. Defaults to FALSE.
                    trace = TRUE) # print out evolution of the Gibbs sampling iterations
# save model
saveRDS(topics_btm30, "btm30.rds")

# get highest token probabilities for each topic
topics_btm30$theta
btm30_terms <- terms(topics_btm30, top_n = 10)
btm30_terms
# create df
btm30_terms.df <- do.call(rbind.data.frame, btm30_terms)
# add numbers of topics
btm30_terms.df <- btm30_terms.df %>% 
  mutate(row_num = row_number() - 1,# first number rows (and subtract 1, otherwise, the numbers for topics would be wrong)
         topic = (row_num %/% 10) + 1) %>% # add topic number (adding 1 makes topic numbering starting from 1)
  group_by(topic) %>%
  mutate(word_no = row_number()) %>% # add row number within a topic
  select(topic, word_no, token, probability, -row_num) %>% # change order and remove auxilary variable "row_num"
  ungroup()
# translate with deepL
#my_key #= deepL API key here
#btm30_terms.df$token_EN <- translate2(
#  text = btm30_terms.df$token, 
#  source_lang = "PL",
#  target_lang = "EN",
#  auth_key = my_key)
# save top terms
#saveRDS(btm30_terms.df, "btm30_terms.rds")
btm30_terms.df <- readRDS("btm30_terms.rds")
btm30_terms_p <- btm30_terms.df %>% 
  group_by(topic) %>%
  slice_max(probability, n = 5) %>% # slice further
  ungroup() %>%
  ggplot(aes(label = token_EN)) + # size = beta)) +
  geom_text_wordcloud() +
  facet_wrap(~topic) +
  ggtitle("Top words from 30 topics",
          subtitle = "TVP and TVN excerpts") +
  #scale_size_area(max_size = 20) + 
  theme_minimal()
btm30_terms_p
pdf("btm30_terms.pdf", height = 10, width = 10)
btm30_terms_p
dev.off()


