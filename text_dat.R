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
saveRDS(topic_number, "LDAtuning_t100.rds")
FindTopicsNumber_plot(topic_number)

# run LDA
lda30 <- LDA(text_frequency_dtm, 
              k = 30,
              method = "Gibbs", 
              control = control_list)
saveRDS(lda30, "LDA_t30.rds")

# get top terms and translate to English
#lda30_topics <- tidy(lda30, matrix = "beta") # topics
#lda30_terms <- lda30_topics %>% 
#  group_by(topic) %>% # top terms in topics
#  slice_max(beta, n = 10) %>%
#  ungroup() %>%
#  arrange(topic, -beta)
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
