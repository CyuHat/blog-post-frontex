# librairies----
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stopwords)
library(widyr)
library(tidygraph)
library(ggraph)
library(ggwordcloud)
library(stringr)

# Données---
load("MyData/text_pos.Rda")
load("MyData/text.Rda")

pos_clean <- 
  text_pos %>%
  filter(!(lemma %in% c(stopwords(), "e", "'s", "l", "I", "h",
                        "g", "b")),
         !(upos %in% c("PUNCT", "NUM", "SYM", "X", "PART",
                       "CCONJ")))
  
# pos_clean %>% 
#   filter(lemma == "also")

my_terms <- c("security", "migrant", "refugee", 
              "asylum", "border", "life", "live",
              "protection", "crisis", "woman",
              "threat", "pressure", "crime",
              "health", "violence", "fraud",
              "exploitation", "mass", "nationality",
              "origin", "economic", "control",
              "false", "claimed", "mediterranean",
              "vulnerable", "sexual", "smuggler")

# Paramètres----
theme_set(theme_bw())

# Wordcloud----
pos_clean %>% 
  count(word = lemma, name = "freq") %>% 
  filter(freq > 15) %>% 
  wordcloud2()

# Analyses----
## Fréquence général----
### Total
pos_clean %>% 
  count(lemma, sort = TRUE) %>% 
  slice_max(n, n = 10) %>% 
  ggplot(aes(reorder(lemma, n), n)) +
  geom_col() +
  geom_label(aes(label = n)) +
  labs(title = "Most frequent words in all reports",
       x = NULL, y = NULL) +
  coord_flip()

# Par rapport
pos_clean %>% 
  group_by(year) %>% 
  count(lemma, sort = TRUE) %>% 
  slice_max(n, n = 10) %>%
  ungroup() %>% 
  ggplot(aes(reorder_within(lemma, n, year), n, fill = factor(year))) +
  geom_col() +
  labs(title = "Most frequent words in all reports",
       x = NULL, y = NULL) +
  coord_flip() +
  facet_wrap(~year, scales = "free") + 
  scale_x_reordered() +
  theme(legend.position = "none")

## Fréquence adjectifs----
### Total
pos_clean %>% 
  filter(upos == "ADJ") %>% 
  count(lemma, sort = TRUE) %>% 
  slice_max(n, n= 10) %>% 
  ggplot(aes(reorder(lemma, n), n)) +
  geom_col() +
  geom_label(aes(label = n)) +
  labs(title = "Most frequent words in all reports",
       x = NULL, y = NULL) +
  coord_flip()

### Par rapports
pos_clean %>% 
  filter(upos == "ADJ") %>% 
  group_by(year) %>% 
  count(lemma, sort = TRUE) %>% 
  slice_max(n, n = 10) %>%
  ungroup() %>% 
  ggplot(aes(reorder_within(lemma, n, year), n, fill = factor(year))) +
  geom_col() +
  labs(title = "Most frequent words in all reports",
       x = NULL, y = NULL) +
  coord_flip() +
  facet_wrap(~year, scales = "free") + 
  scale_x_reordered() +
  theme(legend.position = "none")

### Tf_idf par rapport (bof)
pos_clean %>% 
  filter(upos == "ADJ") %>% 
  group_by(year) %>% 
  count(lemma, sort = TRUE) %>% 
  bind_tf_idf(lemma, year, n) %>% 
  slice_max(tf_idf, n = 3) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(lemma, tf_idf, year), tf_idf, fill = factor(year))) +
  geom_col() +
  labs(title = "Most frequent words in all reports",
       x = NULL, y = NULL) +
  coord_flip() +
  facet_wrap(~year, scales = "free") + 
  scale_x_reordered() +
  theme(legend.position = "none")

# Correlations----
## Termes les plus corrélés----
pos_corr <- 
  pos_clean %>% 
  count(year, lemma) %>% 
  filter(n >= 50) %>% 
  pairwise_cor(lemma, year, n)

autre_corr <- 
  pos_corr %>% 
  filter(item1 %in% c("security", "threat"),
         correlation >= 0.7 | correlation <= -0.7)

autre_corr %>% 
  spread(item1, correlation) %>% 
  mutate(across(is.numeric, ~replace_na(.x, 0))) %>% 
  tibble::column_to_rownames("item2") %>% 
  ggcorrplot::ggcorrplot()

tbl_graph(edges = autre_corr) %>% 
  ggraph(layout = "kk") +
  geom_edge_link() +
  geom_node_label(aes(label = name))

## Adjectifs les plus corrélés
adj_corr <- 
  pos_clean %>% 
  filter(upos == "ADJ") %>% 
  count(year, lemma) %>% 
  filter(n >= 15) %>% 
  pairwise_cor(lemma, year, n)

adj_sim <- 
  pos_clean %>% 
  filter(upos == "ADJ") %>% 
  count(year, lemma) %>% 
  filter(n >= 15) %>% 
  pairwise_similarity(lemma, year, n)

adj_corr %>% 
  arrange(desc(correlation)) %>% 
  filter(item1 == "hostile")

adj_sim %>% 
  arrange(desc(similarity))%>% 
  filter(item1 == "hostile")

## Matrice de correlation
adj_corr %>% 
  filter(correlation >= 0.7 | correlation <= -0.7,
         item2 %in% c("criminal", "external", "foreign",
                      "fundamental", "hostile", "illegal",
                      "irregular", "large", "major",
                      "regular", "serious", "several",
                      "strong", "terrorist", "various")) %>% 
  spread(item2, correlation) %>% 
  mutate(across(is.numeric, ~replace_na(.x, 0))) %>% 
  tibble::column_to_rownames("item1") %>% 
  ggcorrplot::ggcorrplot(hc.order = TRUE)

# adjectif migrant refugee
mig_corr <- 
  pos_clean %>% 
  filter(upos == "ADJ" | lemma %in% c("migrant", "refugee")) %>% 
  count(year, lemma) %>% 
  filter(n >= 5) %>%
  pairwise_cor(lemma, year, n)

mig_corr %>% 
  filter(correlation <= -0.7 | correlation >= 0.7) %>% 
  filter(item1 %in% c("migrant", "refugee")) %>% 
  spread(item2, correlation) %>% 
  mutate(across(is.numeric, ~replace_na(.x, 0))) %>% 
  tibble::column_to_rownames("item1") %>% 
  ggcorrplot::ggcorrplot()



# Word2Vec (Marche pô)----
big_text <-
  text %>% 
  mutate(text = str_remove_all(text, "[:symbol:]")) %>% 
  summarise(text = paste(text, collapse = " ")) %>% 
  pull(text)
