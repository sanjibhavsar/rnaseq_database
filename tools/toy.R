# toy example
# read in csv as full database
setwd("/Users/SanjiBhavsar/Box Sync/!!!WashU/Research/500 - Inscripta Project/rnaseq_database")
# database <- read_csv("data/toy.csv", col_types = cols(timePoint = col_character()))
# json <- read_json("queries/QueryT15.json", simplifyVector = TRUE)
# json <- read_json("queries/QueryT15.json")
json <- fromJSON(file = "QueryT15.json", simplify = TRUE)
# query <- database
# temp <- query$timePoint
# temp
# query <- database
# for(i in names(json)){
#   query <- query %>% filter(i %in% json$i)
# }
# 
# class(json)
json <- fromJSON(file = "QueryT15.json")

# stackOverflow method
json_df <- fromJSON(txt = "query2.json")
df <- unnest(json_df, cols = everything())

my_filter <- function(df, cols, conds){     
  fp <- map2(cols, conds, function(x, y) quo((!!(as.name(x))) %in% !!y))
  filter(df, !!!fp)
}

my_filter(database, cols = as.list(names(df)), conds = as.list(df))
