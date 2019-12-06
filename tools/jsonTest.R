# JSON testing

setwd("/Users/SanjiBhavsar/Box Sync/!!!WashU/Research/500 - Inscripta Project/rnaseq_database/")
# json <- fromJSON("queries/QueryT15.json")

names <- names(json)
test <- c("Hello my name is Sanji", "15", "20", "90", "-1", "15", "-1")
# names <- c(names(json))
# filled <- character(length(json))
json <- read_json("queries/QueryT15.json")
for(i in 1:length(json)){
  if(json[[i]][1] == "NULL"){
    json[[i]][1] <- "[a-zA-Z0-9-]*"
  }
  else{
    entry <- ""
    for(j in json[[i]]){
      entry <- paste(entry, j, sep = "||")
      entry <- gsub("^\|\|", "", entry)
      entry <- gsub("\|\|", "\\|\\|", entry)
    }
    json[[i]] <- entry
  }
}

json$timePoint
regex <- "15\\|\\|-1"
str_view(test, json$timePoint)
str_view(test, regex)


















