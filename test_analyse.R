# Librairies----
library(readtext)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
library(factoextra)


# Données----
text <- 
  readtext("MyData/clean/*.txt") %>% 
  tibble::tibble() %>% 
  slice(-1:-2)

save(text, file = "MyData/text.Rda")

corp <- corpus(text)

save(corp, file = "MyData/corp.Rda")

tok1 <- 
  tokens(corp, remove_punct = TRUE)

tok2 <- 
  tokens(corp, remove_punct = TRUE) %>% 
  tokens_wordstem("english")

save(tok1, tok3, file = "MyData/quanteda_tokens.Rda")

mon_dfm <- 
  dfm(tok2) %>% 
  dfm_remove(stopwords("en", source = "stopwords-iso")) %>% 
  dfm_remove("\\d+", valuetype = "regex") %>% 
  dfm_remove("https?.*", valuetype = "regex") %>% 
  dfm_trim(min_termfreq = 50)

save(mon_dfm, file = "MyData/dfm.Rda")

# Analyse----
# Summary of all corpus
summary(corp)

# Look up for specific words
kwic(tok1, "\\bcultur", valuetype = "regex")

# Top words
topfeatures(mon_dfm, 10)

# Wordcloud
textplot_wordcloud(mon_dfm,
                   min_count = 6,
                   random_order = FALSE,
                   rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


# Word similarity
textstat_simil(mon_dfm) %>% 
  as.list() %>% 
  .$ara_2015 %>% 
  dotchart()

# Arbre hiérarchique de distance
clust <- 
  textstat_dist(mon_dfm) %>% 
  as.dist() %>% 
  hclust()

clust$labels <- docnames(mon_dfm)

plot(clust)

# Analyse de correspondance
res <- textmodel_ca(mon_dfm)

fviz_ca_row(res)

fviz_ca_col(res)

fviz_contrib(res, choice = "col", axes = 1, top = 10)

# Choisis 2015 / 2018 / 2022

mon_dfm[c(4, 7, 11),] %>% 
  textplot_wordcloud(comparison = TRUE)

# À ma manière----
library(tidytext)
library(stringr)
library(dplyr)
library(ggplot2)

tok3 <- 
  text %>% 
  mutate(text = str_remove_all(text, "\\d+|https?.*")) %>% 
  unnest_tokens("words", text) %>%
  mutate(doc_id = str_remove_all(doc_id, "\\.txt"),
         year = str_extract(doc_id, "\\d{4}")) %>% 
  filter(!(words %in% stopwords("en", "stopwords-iso")))

save(tok3, file = "MyData/tidytext_tokens.Rda")

tok3 %>%
  group_by(doc_id) %>% 
  count(words) %>% 
  slice_max(n, n=10) %>% 
  ggplot(aes(n, reorder_within(words, n, doc_id), fill = doc_id)) +
  geom_col() +
  facet_wrap(~doc_id, scales = "free") +
  scale_y_reordered() +
  theme(legend.position = "none")

tok3 %>% 
  group_by(doc_id) %>% 
  count(words) %>% 
  bind_tf_idf(words, doc_id, n) %>% 
  slice_max(tf_idf, n = 10) %>% 
  ggplot(aes(tf_idf, reorder_within(words, tf_idf, doc_id), fill = doc_id)) +
  geom_col() +
  facet_wrap(~doc_id, scales = "free") +
  scale_y_reordered() +
  theme(legend.position = "none")

tok3 %>% 
  filter(str_detect(words, "human")) %>% 
  mutate(year = as.integer(str_extract(doc_id, "\\d{4}"))) %>% 
  group_by(year) %>% 
  count(words) %>% 
  ggplot(aes(year, n, color = words)) +
  geom_line() +
  scale_x_continuous(breaks = 2012:2022)
  