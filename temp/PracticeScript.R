# install tidyverse package via "install.packages(tidyverse)"

library(tidyverse)
library(dplyr)
#set variables

# Yiming's code which parses arguments in the unix command

    # parse_arguments <- function() {
    #   option_list <- list(
    #     make_option(c('-i', '--input_file_path'),
    #                 help='Path to the file to be added'),
    #     make_option(c('-t', '--type'),
    #                 help='Type of file'))
    #     make_option(c('-d', '--database'),
    #                 help = 'Database to which data must be added')
    #   args <- parse_args(OptionParser(option_list=option_list))
    #   return(args)
    # }


# set working directory to proper directory before executing.
setwd("/Users/SanjiBhavsar/Box Sync/!!!WashU/Research/500 - Inscripta Project/Database/Practice")

# Want to automate this through some sort input in a command

bsPath <- "Data/bioSample_J.PLAGGENBERG_07.24.19.csv"
rnaPath <- "Data/rnaSample_J.PLAGGENBERG_08.08.19.csv"
s1Path <- "Data/s1CDNASample_J.PLAGGENBERG_08.21.19.csv"
s2Path <- "Data/s2CDNASample_J.PLAGGENBERG_08.21.19.csv"
libPath <- "Data/library_J.PLAGGENBERG_08.26.19.csv"
fastqPath <- "Data/fastq_J.PLAGGENBERG_09.19.19.csv"

# need to change variables to correct types and remove NAs if possible.
bioSample <- read_csv(bsPath, col_types =
  cols(biosampleNumber = col_integer(),
       inductionDelay = col_integer(),
       experimentObservations = col_character(),
       strain = col_character(),
       replicate = col_integer()
  ))



rnaSample <- read_csv(rnaPath, col_types =
  cols(biosampleNumber = col_integer(),
       rnaSampleNumber = col_integer(),
       RIN = col_character()
       ))

s1Sample <- read_csv(s1Path, col_types =
  cols(s1cDNASampleNumber = col_integer(),
       rnaSampleNumber = col_integer()
       ))

s2Sample <- read_csv(s2Path, col_types =
  cols(s1cDNASampleNumber = col_integer(),
       s2cDNASampleNumber = col_integer()
       ))

libSample <- read_csv(libPath, col_types =
  cols(s2cDNASampleNumber = col_integer(),
       librarySampleNumber = col_integer(),
       index1Name = col_integer()
       ))

fastqSample <- read_csv(fastqPath, col_types =
  cols(librarySampleNumber = col_integer(),
       runNumber = col_integer(),
       laneNumber = col_integer()
       ))
# database <- tibble(d = 1:10, a = 3, c = d^2 + a)

database <- read_csv("Data/database.csv", col_types = 
  cols(biosampleNumber = col_integer(),
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
                     
database <- add_row(database, harvestDate = bioSample$harvestDate, harvester = bioSample$harvester, biosampleNumber = bioSample$biosampleNumber)
database <- database %>% right_join(bioSample)

# database <- database %>% rbind(bioSample)
database <- database %>% left_join(rnaSample)
database <- database %>% left_join(s1Sample)

database <- database %>% left_join(s2Sample)
database <- database %>% left_join(libSample)
database <- database %>% left_join(fastqSample)

# write_excel_csv(database, path = "Data/database.csv")

# for(i in c(2))
# {
#   print(bioSample[i])
# }





# removes all observations with NA - if a whole column is empty and filled with NA, will remove all observations.
# database2 <- database1 %>% drop_na()

