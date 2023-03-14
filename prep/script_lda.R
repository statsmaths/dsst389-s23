library(tidyverse)
library(stringi)
library(cleanNLP)
library(xml2)

# git clone https://github.com/tdhopper/topic-modeling-datasets
# pyenv activate ml
# python3 -m venv env
# source env/bin/activate
# pip install cleannlp
# pip install spacy
# in python=> import spacy; spacy.cli.download("en")

reticulate::use_virtualenv("/Users/admin/Desktop/env", required = TRUE)
cnlp_init_spacy("en_core_web_sm")

############################################################################
# 1. AP dataset
dt <- read_html("topic-modeling-datasets/data/lda-c/blei-ap/ap.txt")
doc_id <- stri_trim(xml_text(xml_find_all(dt, "..//doc//docno")))
txt <- stri_trim(xml_text(xml_find_all(dt, "..//doc//text")))
df <- tibble(doc_id = doc_id, text = txt, train_id = "train")
df <- filter(df, stri_length(text) > 100)

anno <- cnlp_annotate(df)$token
write_csv(df, "ap.csv.bz2")
write_csv(anno, "ap_tokens.csv.bz2")

############################################################################
# 2. Nematoad
dt <- read_lines("topic-modeling-datasets/data/raw/Nematode\ biology\ abstracts/cgcbib.txt")

df <- tibble(start = stri_sub(dt, 1L, 11L), end = stri_sub(dt, 14L, -1L))
df$start <- stri_trim(df$start)
df$end <- stri_trim(df$end)
df$start[df$start == ""] <- NA_character_
df <- tidyr::fill(df, start)
df$doc_id <- sprintf("doc%05d", cumsum(df$start == "Key"))
df <- filter(df, end != "-------------------")
df$start <- stri_trans_tolower(df$start)

df <- df %>%
  group_by(doc_id, start) %>%
  summarize(end = paste(end, collapse = " ")) %>%
  ungroup() %>%
  pivot_wider(values_from = "end", names_from = "start")

df <- filter(df, validEnc(abstract))
df <- filter(df, stri_length(abstract) > 100)
df <- rename(df, text = abstract)
df$train_id <- "train"

anno <- cnlp_annotate(df)$token
write_csv(df, "nematode_abs.csv.bz2")
write_csv(anno, "nematode_abs_tokens.csv.bz2")


