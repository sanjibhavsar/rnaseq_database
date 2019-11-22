# This is the add_row method


# if(nrow(database) == 0){
#  
#   # database <- add_row(database, 
#   #                     harvestDate = newBioData$harvestDate, 
#   #                     harvester = newBioData$harvester, 
#   #                     biosampleNumber = newBioData$biosampleNumber
#   #                     # experimentDesign = newBioData$experimentDesign,
#   #                     # experimentObservations = newBioData$experimentObservations,
#   #                     # strain = newBioData$strain,
#   #                     # genotype = newBioData$genotype,
#   #                     # floodmedia = newBioData$floodmedia,
#   #                     # inductionDelay = newBioData$inductionDelay,
#   #                     # treatment = newBioData$treatment,
#   #                     # timePoint = newBioData$timePoint,
#   #                     # replicate = newBioData$replicate
#   # 
#   # )
#   database <- database %>% right_join(newBioData)
#   print(database)
#   return(database)
# } else {
#   database <- add_row(database, 
#                       harvestDate = newBioData$harvestDate, 
#                       harvester = newBioData$harvester, 
#                       biosampleNumber = newBioData$biosampleNumber
#                       # experimentDesign = newBioData$experimentDesign,
#                       # experimentObservations = newBioData$experimentObservations,
#                       # strain = newBioData$strain,
#                       # genotype = newBioData$genotype,
#                       # floodmedia = newBioData$floodmedia,
#                       # inductionDelay = newBioData$inductionDelay,
#                       # treatment = newBioData$treatment,
#                       # timePoint = newBioData$timePoint,
#                       # replicate = newBioData$replicate
#                       
#   )
#   # This full join keeps data from both a left and right join
#   database <- database %>% full_join(newBioData)
#   # This drops all observations which are NA for experimentDesign
#   database <- (database %>% drop_na(timePoint))
#   print(database)
#   return(database)
# }