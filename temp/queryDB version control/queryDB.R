# Query database of csv files for specific files
suppressMessages(library(readxl))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(optparse))
# setwd("/Users/SanjiBhavsar/Box Sync/!!!WashU/Research/500 - Inscripta Project/Database/Practice")


# read in options from command line code
parse_arguments <- function() {
  option_list <- list(
    make_option(c('-i', '--input_directory'), default = NULL,
                help='Path to main directory for database files'),
    make_option(c('-o', '--output'), default = NULL,
                help='Filepath or directory path for newly created metadata sheet'),
    make_option(c('-t', '--timepoint'), default = "-?[0-9]*",
                help='query for timepoint'),
    make_option(c('-r', '--treatment'), default = "[A-z]*",
                help='query for treatment'),
    make_option(c('-p', '--purpose'), default = "[A-z]*",
                help='query for purpose'),
    make_option(c('-n', '--genotype'), default = "[A-z]*",
                help='query for genotype'),
    make_option(c('-d', '--inductionDelay'), default = "[0-9]*",
                help='query for induction delay'),
    make_option(c('-s', '--strain'), default = "[A-z]*",
                help='query for strain'),
    make_option(c('-f', '--floodmedia'), default = "[A-z]*",
                help='query for floodmedia'),
    make_option(c('-g', '--group'), default = "NULL",
                help='group ID')
  )
      
  args <- parse_args(OptionParser(option_list=option_list))
  # print(args)
  
  # check if input and output are provided
  if (is.null(args$input_directory) || is.null(args$output) || is.null(args$group)) {
    stop("input, output, and group arguments must be provided.")
  }
  else{
    # print("reached else statement")
    return(args)
  }
}



# creates a database by reading in all excel files from their proper directories and 
# returns a filled and joined tibble data frame which is filled out with all respective
# fastQ files. This function drops all observations which have empty fastQ file paths.
create_db <- function(fqFiles, libFiles, s2Files, s1Files, rnaFiles, bioFiles){
  
  print("reading in files")
  # read in all excel files

  tblFastQ <- sapply(fqFiles, read_excel, simplify=FALSE)  %>%
    bind_rows(.id = "id") %>% select(-id)
  # print(tblFastQ, n = nrow(tblFastQ))
  # print(tblFastQ)
  tblFastQ <- tblFastQ %>% drop_na(fastqFileName)
  # write.csv(tblFastQ, "Practice/tblFQ.csv")
  
  tblLib <- sapply(libFiles, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% select(-id)
  # print(tblLib)
  # write.csv(tblLib, "Practice/tblLib.csv")
  
  tblS2 <- sapply(s2Files, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% select(-id)
  # print(tblS2)
  # write.csv(tblS2, "Practice/tblS2.csv")
  
  tblS1 <- sapply(s1Files, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% select(-id)
  # print(tblS1)
  # write.csv(tblS1, "Practice/tblS1.csv")
  
  tblRNA <- sapply(rnaFiles, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% select(-id)
  # print(tblRNA)
  # write.csv(tblRNA, "Practice/tblRNA.csv")
  
  tblBio <- sapply(bioFiles, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% select(-id)
  # print(tblBio)
  # write.csv(tblBio, "Practice/tblBio.csv")
  
  # perform all joins
  db <- tblFastQ %>%
        left_join(tblLib) %>%
        right_join(tblS2) %>%
        right_join(tblS1) %>%
        right_join(tblRNA) %>%
        right_join(tblBio)

  # drops all observations which do not have a corresponding FastQ filepath.
  db <- db %>% drop_na(fastqFileName)
  # db <- tibble(x = c(1), y = c(2))
  # print(db, n = nrow(db))
  write.csv(db, "Practice/output.csv")
  return(db)
}
# HELP
# Stuck on how to specify optional arguments in this function. I need default to match to any entry.
# maybe figured it out?? -- needs to be tested with command line arguments, requires separate option for each query term. 
# Default value is the set of all possible values for the term shown below.

query_db <- function(database, tp, tr, id, fm, gt, prp, str){
                     # tp = "-?[0-9]*",
                     # tr = "[A-z]*",
                     # id = "[0-9]*",
                     # fm = "[A-z]*",
                     # gt = "[A-z]*",
                     # prp = "[A-z]*",
                     # str = "[A-z]*"){
  
  # works but trying regex
                     # tp = alltp <- c(-1, 10,15,20,90),
                     # tr = alltr <- c("EtoH", "Estradiol"),
                     # id = allid <- c(1,15,30),
                     # fm = allfm <- c("PBS", "SCGal", "SCGlu"),
                     # gt = allgt <- c("BY4741", "CBF1"),
                     # prp = allprp <- c("spikein", "fullRNASeq")){
  
  
  query <- database %>% filter(str_detect(as.character(timePoint), as.character(tp))) %>%
    filter(str_detect(as.character(purpose), as.character(prp))) %>%
    filter(str_detect(as.character(treatment), as.character(tr))) %>%
    filter(str_detect(as.character(inductionDelay), as.character(id))) %>%
    filter(str_detect(as.character(genotype), as.character(gt))) %>%
    filter(str_detect(as.character(floodmedia), as.character(fm)))
  
  # Works but trying regex
# query <- database %>% filter(timePoint %in% tp) %>%
#                       filter(treatment %in% tr) %>%
#                       filter(inductionDelay %in% id) %>%
#                       filter(floodmedia %in% fm) %>%
#                       filter(genotype %in% gt) %>%
#                       filter(purpose %in% prp)
  
  
                      # filter(grepl(tr, treatment, ignore.case = TRUE)) %>%
                      # filter(grepl(id, inductionDelay)) %>%
                      # filter(grepl(gt, genotype, ignore.case = TRUE)) %>%
                      # filter(grepl(fm, floodmedia, ignore.case = TRUE)) %>%
                      # filter(grepl(stn, strain, ignore.case = TRUE)) %>%
                      # filter(grepl(prp, purpose, ignore.case = TRUE))

# print(timepoint)
# print(treatment)
# print(inductionDelay)
# print(floodmedia)
# print(genotype)
# print(strain)
# print(purpose)

return(query)
}

write_summary <- function(query){
  # columns to select from query: 
  # GENOTYPE	
  # STRAIN	
  # SAMPLE	
  # INDUCTION	
  # LIBRARY	
  # REPLICATE	
  # INDEX	
  # RUN_NUMBER	
  # FILE	
  # GROUP	
  # TIME_POINT	
  # MEDIA
  
  # query %>% select(genotype, strain, induction, timePoint)
  # create empty tibble for qc columns
  qc_col <- tibble(
    ST_PIPE <- c(""),
    ST_TOTAL_READS	<- c(""),
    ST_ALIGN_PCT	<- c(""),
    ST_MUT_FOW	<- c(""),
    ST_RC_FOM	<- c(""),
    ST_COV_MED	<- c(""),
    AUTO_AUDIT	<- c(""),
    MANUAL_AUDIT	<- c(""),
    USER	<- c(""),
    NOTE <- c("")
  )
  sum <- query %>% select(genotype, strain, inductionDelay, library, replicate, )
  
  
  # write file to csv at specified filepath
  return()
}

# Main Method
# This method with create a database from the inputted excel files, query the database, and return a sample_summary sheet ready for Quality Assessment inth 

parsed <- parse_arguments()
print(parsed)

# Right now, can only handle one person's directory of files. Need to add this functionality later.
name <- "J.Plaggenberg"

basePath <- as.character(parsed$input_directory)
# basePath <- "/Users/SanjiBhavsar/Box Sync/!!!WashU/Research/500 - Inscripta Project/Database/Practice"
types <- c("fastqFiles", "library", "s2cDNASample", "s1cDNASample", "rnaSample", "bioSample")
# fqFiles <- files <- list.files(path = basePath, pattern = regex("fastq.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# libFiles <- files <- list.files(path = basePath, pattern = regex("library.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# s2Files <- files <- list.files(path = basePath, pattern = regex("s2cdna.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# s1Files <- files <- list.files(path = basePath, pattern = regex("s1cdna.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# rnaFiles <- files <- list.files(path = basePath, pattern = regex("rnaSample.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# bioFiles <- files <- list.files(path = basePath, pattern = regex("bioSample.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)

fqFiles <- files <- list.files(path = basePath, pattern = ".astq.*.xlsx", full.names = T, recursive = TRUE)
libFiles <- files <- list.files(path = basePath, pattern = ".ibrary.*.xlsx", full.names = T, recursive = TRUE)
s2Files <- files <- list.files(path = basePath, pattern = ".2cDNA.*.xlsx", full.names = T, recursive = TRUE)
s1Files <- files <- list.files(path = basePath, pattern = ".1cDNA.*.xlsx", full.names = T, recursive = TRUE)
rnaFiles <- files <- list.files(path = basePath, pattern = ".naSample.*.xlsx", full.names = T, recursive = TRUE)
bioFiles <- files <- list.files(path = basePath, pattern = ".ioSample.*.xlsx", full.names = T, recursive = TRUE)


db <- create_db(fqFiles, libFiles, s2Files, s1Files, rnaFiles, bioFiles)
# query <- query_db(db, tp = parsed$timepoint,
#                       tr = parsed$treatment,
#                       prp = parsed$purpose,
#                       str = parsed$strain,
#                       gt = parsed$genotype,
#                       fm = parsed$floodmedia,
#                       id = parsed$inductionDelay)
# print(parsed)
# print(query)
