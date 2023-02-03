# Function to export data into multipèle text file for cleaning
# To save the file
ara.write <- function(name, content){
	name <- paste0("MyData/to_clean/",name, ".txt")

	writeLines(content, name)
}

# Wrapper for readtext dataframe
ara.export <- function(data){
	map2(data$doc_id, data$text, ara.write)
}