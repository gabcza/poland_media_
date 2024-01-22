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