library(tabulizer)
library(stringr)
library(purrr)
library(readtext)

get.all.pdf <- function(){
  liste <- list.files("Data/", ".pdf", full.names = TRUE)
  
  name <- paste0("MyData/ara_",
                str_extract(liste, "\\d{4}"),
                ".txt")
  
  text <- map(liste, extract_text)
  
  map2(text, name, writeLines)
  
}

get.all.pdf()


readtext("MyData/*.txt")
