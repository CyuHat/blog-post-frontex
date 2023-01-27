# Goal: Import and clean Annual Risk Analysis of frontex
# Libraries----
library(readtext)
library(dplyr)
library(tidytext)
library(stringr)
library(purrr)

# Importing personnal functions----
source("Scripts/functions_import.r")
# Importing data----
a <- list.files(path = "Data/", pattern = "*.pdf", full.names = TRUE)[-7]

ara_raw <-
	readtext(a) %>% 
	tibble() %>% 
	mutate(doc_id = str_replace_all(doc_id, ".*(\\d{4})\\.pdf", "ara_\\1"))

save(ara_raw, file = "MyData/ara_raw.rda")

# Exporting data for manual cleaning
ara.export(ara_raw)

# Cleaning
ara_clean <- 
	ara_raw %>% 
		mutate(text = str_trim(text),
			   text = str_replace_all(text, "\\n", " "))

save(ara_clean, file = "MyData/ara_clean.rda")

# Tokens
stop <- tibble(words = stopwords::data_stopwords_stopwordsiso$en)

ara_tokens <- 
	ara_clean %>% 
		unnest_tokens("words", text) %>% 
		anti_join(stop) %>% 
		filter(!str_detect(words, "\\d+")) %>% 
		count(doc_id, words)

rm(stop)

save(ara_tokens, file = "MyData/ara_tokens.rda")

# tf_idf
ara_tf_idf <- 
	ara_tokens %>% 
		bind_tf_idf(words, doc_id, n) %>% 
		select(doc_id, words, tf_idf)

save(ara_tf_idf, file = "MyData/ara_tf_idf.rda")

# dfm
ara_dfm <- cast_dfm(ara_tf_idf, doc_id, words, tf_idf)

save(ara_dfm, file = "MyData/ara_dfm.rda")

# Save into database
library("RSQLite")

# Set up database
con <- dbConnect(SQLite(), dbname = "test.db")
dbWriteTable(con, "data1", as.data.frame(data1))
dbDisconnect(con)

