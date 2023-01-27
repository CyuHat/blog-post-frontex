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
en <- udpipe_load_model("MyData/english-ewt-ud-2.5-191206.udpipe")

pos <- function(text){
  tab <- 
    udpipe_annotate(en, text) %>% 
    as_tibble()
  
  return(tab)
}

text_pos <- 
  text %>% 
  mutate(text = str_conv(text, "UTF-8"),
         pos = map(text, pos))

text_pos %>% 
  unlist(pos)
