# Librairies----
library(readtext)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
library(factoextra)

# Données----
text <-
  readtext("MyData/to_clean/*.txt") %>% 
  tibble() %>% 
  slice(-1:-3) %>%
  mutate(year = as.numeric(str_extract(doc_id, "\\d{4}")),
         text = str_to_lower(text),
         text = str_replace_all(text, "\\s+", " "),
         text = str_replace_all(text, "([A-Za-z]+)\\-\\s", "\\1"),
         text = str_remove_all(text, "\xad"),
         text = str_remove_all(text, "(annual\\s)?risk\\sanalysis(\\s(for|of))?\\s\\d{4}"),
         text = str_remove_all(text, "\\d+\\sof\\s\\d+"),
         text = if_else(year %in% c(2013:2015, 2022),
                        str_remove(text, "^.+(?=executive\\ssummary(?!\\s+\\#))"),
                        str_remove(text, "^.+(?=\\d\\.\\s+summary(?!\\s+\\#))")),
         text = if_else(year < 2022,
                        str_remove(text, "\\d{1,2}(\\.)?\\s+statistical\\sannex.+$"),
                        str_remove(text, "annex\\s+\\-\\s+methodological\\snote.+$")),
         text = str_replace_all(text, "(bor)\\s(ders?)", "\\1\\2"),
         text = str_replace_all(text, "([a-z]{2,})\\s(ing)", "\\1\\2"),
         text = str_replace_all(text, "([a-z]{4,})\\s(ities?|ment)", "\\1\\2"),
         text = str_replace_all(text, "(ac)\\s([a-z]{4,})", "\\1\\2"),
         text = str_replace_all(text, "(indica|fac|facilita)\\s(tors?)", "\\1\\2"),
         text = str_replace_all(text, "(re)\\s(ported)", "\\1\\2"),
         text = str_replace_all(text, "([a-z]{2,})(frontex)?\\s(tions?)", "\\1\\3"),
         text = str_replace_all(text, "(sit)\\s(uation)", "\\1\\2"),
         text = str_replace_all(text, "(mem|num)\\s(bers?)", "\\1\\2"),
         text = str_replace_all(text, "(pos)\\s(sible)", "\\1\\2"),
         text = str_replace_all(text, "(pas)\\s(sengers)", "\\1\\2"),
         text = str_replace_all(text, "(mo)\\s(rocco)", "\\1\\2"),
         text = str_replace_all(text, "(pub)\\s(lic|lished)", "\\1\\2"),
         text = str_replace_all(text, "(infec)\\s(tious)", "\\1\\2"),
         text = str_replace_all(text, "(indi)\\s(cators?|cating|vidual|cates?)", "\\1\\2"),
         text = str_replace_all(text, "(mi)\\s(frontex\\s)?(grants?|gration|gratory)", "\\1\\3"),
         text = str_replace_all(text, "(de)(frontex)?\\s(tections?)", "\\1\\3"),
         text = str_replace_all(text, "(secu)\\s(rity)", "\\1\\2"),
         text = str_replace_all(text, "(schen)\\s(gen)", "\\1\\2"),
         text = str_replace_all(text, "(exter)\\s(nal)", "\\1\\2"),
         text = str_replace_all(text, "(af)\\s(ricans?)", "\\1\\2"),
         text = str_replace_all(text, "(consid|recov)\\s(ered)", "\\1\\2"),
         text = str_replace_all(text, "(differ|consist)\\s(ent)", "\\1\\2"),
         text = str_remove_all(text, "aa|tur|n\\.a|grc|ju"),
         text = str_remove_all(text, "n\\sm\\sar\\sm\\say\\sju\\sl\\sse\\sp\\sn\\sov"),
         text = str_remove_all(text, "fig\\.?\\s+\\d{1,2}")
  ) %>% 
  select(doc_id, year, text)

# Rapid----
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

tok3 <- 
  text %>% 
  mutate(text = str_remove_all(text, "\\d+|https?.*")) %>% 
  unnest_tokens("words", text) %>%
  mutate(doc_id = str_remove_all(doc_id, "\\.txt"),
         year = str_extract(doc_id, "\\d{4}")) %>% 
  filter(!(words %in% stopwords("en", "stopwords-iso")))

save(tok3, file = "MyData/tidytext_tokens.Rda")

# test ----
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

kwic(tok1, "\\bent\\b", valuetype = "regex")

a <- c("mi grant", "mi grants", "mi gration", " mi frontex", "mi gratory", "mi frontex grant")

str_replace(a, "(mi)\\s(frontex\\s)?(grants?|gration|gratory)", "\\1\\3")

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
fviz_contrib(res, choice = "col", axes = 2, top = 10)

# Choisis 2015 / 2018 / 2022

mon_dfm[c(3, 6, 10),] %>% 
  textplot_wordcloud(comparison = TRUE)

# À ma manière----
# tok3 %>%
#   group_by(doc_id) %>% 
#   count(words) %>% 
#   slice_max(n, n=10) %>% 
#   ggplot(aes(n, reorder_within(words, n, doc_id), fill = doc_id)) +
#   geom_col() +
#   facet_wrap(~doc_id, scales = "free") +
#   scale_y_reordered() +
#   theme(legend.position = "none")

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

kwic(tok1, "\\bment\\b", valuetype = "regex")



tok3 %>% 
  filter(str_detect(words, "human")) %>% 
  mutate(year = as.integer(str_extract(doc_id, "\\d{4}"))) %>% 
  group_by(year) %>% 
  count(words) %>% 
  ggplot(aes(year, n, color = words)) +
  geom_line() +
  scale_x_continuous(breaks = 2012:2022)
  