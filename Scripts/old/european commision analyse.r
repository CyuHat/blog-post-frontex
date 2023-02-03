library(readr)
library(rvest)

d1 <- read_delim("./RegisterOfCommissionDocument_exported_result_10.12.2022(1).csv", delim = ";")
d2 <- read_delim("./RegisterOfCommissionDocument_exported_result_10.12.2022.csv", delim = ";")

# Connection
url <- "https://ec.europa.eu/transparency/documents-register/search?query=eyJjYXRlZ29yeU9iamVjdHMiOltdLCJjYXRlZ29yaWVzIjpbXSwidHlwZU9iamVjdHMiOlt7ImlkIjoiTUlOVVRFU19DT01fTUVFVElORyIsImxhbmd1YWdlIjoiZnIiLCJ2YWx1ZSI6IlByb2PDqHMtdmVyYmFsIGRlIGxhIHLDqXVuaW9uIGRlIGxhIENvbW1pc3Npb24iLCJ0eXBlIjoiVFlQRSJ9LHsiaWQiOiJKT0lOVF9URVhUIiwibGFuZ3VhZ2UiOiJmciIsInZhbHVlIjoiVGV4dGUgY29tbXVuIiwidHlwZSI6IlRZUEUifV0sInR5cGVzIjpbIk1JTlVURVNfQ09NX01FRVRJTkciLCJKT0lOVF9URVhUIl0sImRlcGFydG1lbnRPYmplY3RzIjpbXSwiZGVwYXJ0bWVudHMiOltdLCJsYW5ndWFnZSI6ImVuIiwia2V5d29yZHNTZWFyY2hUeXBlIjoiQVRfTEVBU1RfT05FIiwidGFyZ2V0IjoiVElUTEVfQU5EX0NPTlRFTlQiLCJzb3J0QnkiOiJET0NVTUVOVF9EQVRFX0RFU0MiLCJpc1JlZ3VsYXIiOnRydWUsImtleXdvcmRzIjoiYm9yZGVyIiwicmVmZXJlbmNlIjoiIiwiZGF0ZUZyb20iOiIyMDE1LTAxLTAxVDA4OjAwOjAwLjAwMFoiLCJkYXRlVG8iOiIyMDIyLTEyLTEwVDA4OjAwOjAwLjAwMFoiLCJwYWdlIjoxfQ%3D%3D"

page <- read_html(url)

page  %>% 
    html_elements("ecl-link")


## Read text
library(dplyr)
library(readtext)


doc <- readtext("./Annual_Risk_Analysis_2015.pdf")

tibble(doc)

writeLines()