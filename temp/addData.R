# Sanji Bhavsar
# Adding New Data to Existing Database

suppressMessages(library(optparse))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))

# Parser to read arguments from Command Line Arguments
# parse_arguments <- function() {
#   option_list <- list(
#     make_option(c('-t', '--type'),
#                 help='Type of file. Allowed arguments: bio, rna, s1, s2, lib, fq'))
#     make_option(c('-i', '--input_file_path'),
#                 help='Path to the file to be added'),
#     make_option(c('-n', '--database_path'),
#                 help = 'Database to which data must be added')
#   args <- parse_args(OptionParser(option_list=option_list))
#   return(args)
# }


# function which adds the data to the existing database based on what kind of data is being inputted into the script
add_data <- function(type, database, filepath){
  if(type == 'bio'){
    # print("The number of rows in database before adding is:")
    # print(nrow(database))
    
    newBioData <- read_csv(filepath, col_types =
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
    database <- rbind(database, newBioData)
    # print(newBioData)
    
  }
  else if(type == 'rna') {
    # print("reached rna")
    
    newRNAData <- read_csv(filepath, col_types =
                            cols(biosampleNumber = col_integer(),
                                  rnaSampleNumber = col_integer()
                                  # RIN = col_character()
                             ))
    database <- database %>% full_join(newRNAData)
    
    print(database)
    
    # database <- database %>% drop_na()
    return(database)
    
  # } else if(type == 's1') {
  #   newS1Data  <- read_csv(filepath, col_types =
  #                          cols(s1cDNASampleNumber = col_integer(),
  #                               rnaSampleNumber = col_integer()
  #                          ))
  #   database <- database %>% left_join(newS1Data)
  #   return(database)
  # } else if(type == 's2') {
  #   newS2Data <- read_csv(filepath, col_types =
  #                          cols(s1cDNASampleNumber = col_integer(),
  #                               s2cDNASampleNumber = col_integer()
  #                          ))
  #   database <- database %>% left_join(newS2Data)
  #   return(database)
  # } else if(type == 'lib') {
  #   newLibData <- read_csv(filepath, col_types =
  #                           cols(s2cDNASampleNumber = col_integer(),
  #                                librarySampleNumber = col_integer(),
  #                                index1Name = col_integer()
  #                           ))
  #   database <- database %>% left_join(newLibData)
  #   return(database)
  # } else if(type == 'fq') {
  #   newFQData <- read_csv(filepath, col_types =
  #                             cols(librarySampleNumber = col_integer(),
  #                                  runNumber = col_integer(),
  #                                  laneNumber = col_integer()
  #                             ))
  #   database <- database %>% left_join(newFQData)
  #   return(database)
  } else{
  return(database)
  }
}


# Main Code

# parses arguments inputted in command line
# parsed <- parse_arguments()
args <- commandArgs(trailingOnly = TRUE)
# print(args)

# read in old database which data needs to be appended to
# database <- read_csv(parsed$database_path, col_types = 
  db <- read_csv(args[4], col_types = 
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
  # print(db)
# add_data(type = parsed$type, database = database, filepath = parsed$input_file_path)

# write_excel_csv(database, path = parsed$database_path)

  db <- add_data(type = args[2], database = db, filepath = args[3])
  # print(db)
  write_excel_csv(db, path = args[4])




