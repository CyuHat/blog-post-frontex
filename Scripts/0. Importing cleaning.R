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
         text = str_replace_all(text, "\\bren\\b", "return"),
         text = str_remove_all(text, "\\baa\\b|\\btur\\b|\\bn\\.a\\b|\\bgrc\\b|\\bju\\b"),
         text = str_remove_all(text, "n\\sm\\sar\\sm\\say\\sju\\sl\\sse\\sp\\sn\\sov"),
         text = str_remove_all(text, "\\b\\-(?=[a-z]+)"),
         text = str_remove_all(text, "fig\\.?\\s+\\d{1,2}")) %>% 
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

# Control----
# Summary of all corpus
summary(corp)

# Look up for specific words
kwic(tok1, "\\bcultur", valuetype = "regex")


text %>% 
  mutate(year = as.numeric(str_extract(doc_id,"\\d{4}"))) %>%
  unnest_tokens("words", text) %>% 
  filter(str_detect(words, "human")) %>% 
  group_by(year) %>% 
  count(words) %>% 
  ggplot(aes(year, n, color = words)) +
  geom_line()

# collocation de crisis
coloc <- 
  tok1 %>% 
  textstat_collocations()

coloc %>% 
  filter(str_detect(collocation, "crisis")) %>% 
  arrange(desc(count))


text %>% 
  unnest_ngrams("bigram", text, n = 2) %>% 
  mutate(period = ifelse(year < 2022, "Before 2022", "In 2022")) %>% 
  filter(str_detect(bigram, "crisis")) %>% 
  group_by(period) %>% 
  count(bigram, sort = TRUE) %>% 
  slice_max(n, n = 3) %>% 
  ggplot(aes(reorder_within(bigram, n, period), n, fill = period)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~period, scale = "free") +
  scale_x_reordered() +
  labs(title = "Colocation of the term 'crisis' with other terms by period",
       x = NULL, y = NULL) +
  theme(legend.position = "none")

# GGwordcloud----
library(ggwordcloud)

tok3 %>% 
  filter(!(words %in% c("aa", "i.e", "thb", "'s", "e.g"))) %>% 
  count(words, sort = TRUE) %>% 
  slice_max(n, n = 500) %>% 
  ggplot(aes(label = words, size = n)) +
  geom_text_wordcloud(
    mask = png::readPNG("MyData/flag.png"),
    rm_outside = TRUE, color = "white"
  ) +
  scale_size_area(max_size = 8) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "red"))
