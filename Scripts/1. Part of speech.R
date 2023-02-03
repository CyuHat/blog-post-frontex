# Librairies----
library(udpipe)
library(dplyr)
library(purrr)
library(tidytext)
library(tidyr)

# Données----
load("MyData/text.Rda")

text

# module de langue----
# importer un module
udpipe_download_model("english-ewt", model_dir = "MyData/")

# charger le module
en <- udpipe_load_model("MyData/udpipe/english-ewt-ud-2.5-191206.udpipe")

pos <- function(text){
  tab <- 
    udpipe_annotate(en, text) %>% 
    as_tibble() %>% 
    select(token, lemma, upos)
  
  return(tab)
}

pos("I am very happy now")

# text = str_conv(text, "UTF-8"),

text_pos <- 
  text %>% 
  mutate(pos = map(text, pos)) %>% 
  tidyr::unnest(pos) %>% 
  select(-text)

save(text_pos, file = "MyData/text_pos.Rda")
