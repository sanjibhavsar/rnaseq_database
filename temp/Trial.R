# print("The number of rows in database after adding is:")
# print(nrow(database))
# print("starting join")

# database <- database %>% left_join(newBioData)

# database <- database %>% right_join(newBioData)
# database <- database %>% full_join(newBioData)

# na.omit(database, database$experimentDesign)
# print(database)

db <- read_csv("/Users/SanjiBhavsar/Box Sync/!!!WashU/Research/500 - Inscripta Project/Database/Practice/Data/database.csv", col_types = 
                 cols(
                   harvestDate = col_character(),
                   harvester = col_character(),
                   biosampleNumber = col_integer(),
                   experimentDesign = col_character(),
                   experimentObservations = col_character(),
                   strain = col_character(),
                   genotype = col_character(),
                   floodmedia = col_character(),
                   inductionDelay = col_integer(),
                   treatment = col_character(),
                   timePoint = col_integer(),
                   replicate = col_integer()
                 ))

coalesce_by_column <- function(df) {
  return(coalesce(df[1], df[2]))
}

# df <- data.frame(A=c(1,1,2,2),B=c(NA,2,NA,4),C=c(3,NA,NA,5),D=c(NA,2,3,NA),E=c(5,NA,NA,4))
newDB <- filter(db, (is.na(harvestDate) && !is.na(rnaDate)) || (!is.na(harvestDate) && is.na(rnaDate)))
newDB <- db %>%
  group_by(sampleNumber) %>%
  summarise_all(coalesce_by_column)
