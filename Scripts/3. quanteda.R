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
  filter(str_detect(words, "humanitarian")) %>% 
  mutate(year = as.integer(str_extract(doc_id, "\\d{4}"))) %>% 
  group_by(year) %>% 
  count(words) %>% 
  ggplot(aes(year, n, color = words)) +
  geom_line() +
  scale_x_continuous(breaks = 2012:2022)

# Wordcloud
textplot_wordcloud(mon_dfm,
                   min_count = 6,
                   random_order = FALSE,
                   rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))