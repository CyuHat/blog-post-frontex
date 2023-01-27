# Libraries----
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda)
library(dplyr)
library(LSAfun)
library(stringr)
library(widyr)
library(ggplot2)

# Importing data----
load("./MyData/ara_dfm.rda")

# LSA Model----
ara_lsa <- textmodel_lsa(ara_dfm)

# Sementic similarity
neighbors("border", n = 10, tvectors = ara_lsa$features)
neighbors("migrants", n = 10, tvectors = ara_lsa$features)

# Sementic neighborhood
plot_neighbors("border", n = 10, tvectors = ara_lsa$features)
plot_neighbors("migrants", n = 10, tvectors = ara_lsa$features)

# Prochaines étapes
# 1. Nettoyer manuellement les ARA
# 2. Faire le POS de ces documents pour avoir des STEM
# 3. Tester de nouveau ce code
# 4. Faire une analyse descriptive (freq, corr, tf_idf)
# 5. Décider si je garde les éléments par text ou par partie par exemple
# 6. S'inspirer des critical discourses analysis

load("./MyData/ara_tokens.rda")

library(tidytext)
library(ggplot2)
library(stringr)

ara_tokens %>% 
    mutate(year = as.numeric(str_extract(doc_id, "\\d{4}"))) %>% 
    group_by(doc_id, year) %>% 
    summarise(n = sum(n)) %>% 
    ggplot(aes(year, n)) +
    geom_line(size = 2)

ara_tokens %>%
    mutate(year = as.numeric(str_extract(doc_id, "\\d{4}"))) %>% 
    filter(words %in% c("migrant", "immigrant", "asylum", "refugee", "illegal")) %>% 
    ggplot(aes(year, n, color = words)) +
    geom_line(size = 2)

ara_tokens %>%
    mutate(year = as.numeric(str_extract(doc_id, "\\d{4}"))) %>% 
    filter(words %in% c("frontier", "border")) %>% 
    ggplot(aes(year, n, color = words)) +
    geom_line(size = 2)

ara_tokens %>%
    mutate(year = as.numeric(str_extract(doc_id, "\\d{4}"))) %>% 
    filter(str_detect(words, "afg")) %>% 
    ggplot(aes(year, n, color = words)) +
    geom_line(size = 2)

# Test collocation
load("./MyData/ara_clean.rda")

col_res <-
    ara_clean %>% 
        corpus() %>% 
        textstat_collocations(min_count = 100)

col_res %>%
    filter(str_detect(collocation, "migrant"))

ara_tokens %>% 
    filter(n > 50) %>% 
    ggplot(aes(n)) +
    geom_histogram()

ara_corr <- 
    ara_tokens %>% 
        filter(n > 50) %>% 
        pairwise_cor(words, doc_id, n)

ara_corr %>% 
    filter(item1 == "border") %>% 
    arrange(desc(correlation))


textstat_simil(ara_dfm, margin = "features")

ara_dfm[,str_detect(featnames(ara_dfm), "(im)?migrants?|borders?|asylum|refugees?|frontiers?")] %>% 
    textstat_simil(margin = "features") %>% 
    as.matrix() %>% 
    corrplot::corrplot()

ara_dfm[,str_detect(featnames(ara_dfm), "(im)?migrants?|refugees?|afghan")] %>% 
    textstat_simil(margin = "features") %>% 
    as.matrix() %>% 
    corrplot::corrplot()

ara_dfm[,str_detect(featnames(ara_dfm), "\\bborder\\b|\\brisk\\b")] %>% 
    textstat_simil(margin = "features") %>% 
    as.matrix() 
    corrplot::corrplot()
