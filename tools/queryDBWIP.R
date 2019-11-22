# Query database of csv files for specific files
suppressMessages(library(readxl))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(optparse))
suppressMessages(library(stringr))
# suppressMessages(library(xlsx))
# suppressMessages(library(RSAGA))
# setwd("/Users/SanjiBhavsar/Box Sync/!!!WashU/Research/500 - Inscripta Project/rnaseq_database/")


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
    make_option(c('-q', '--query'), default = "query",
                help='some meaningful name for the query')
  )
      
  args <- parse_args(OptionParser(option_list=option_list))
  # print(args)
  
  # check if input and output are provided
  if (is.null(args$input_directory) || is.null(args$output)) {
    stop("input and output must be provided.")
  }
  else{
    # print("reached else statement")
    return(args)
  }
}



# creates a database by reading in all excel files from their proper directories and 
# returns a filled and joined tibble data frame which is filled out with all respective
# fastQ files. This function drops all observations which have empty fastQ file paths.
create_db <- function(fqFiles, libFiles, s2Files, s1Files, rnaFiles, bioFiles, output){
  
  # print("reading in files")
  # read in all excel files

  tblFastQ <- sapply(fqFiles, read_excel, simplify=FALSE)  %>%
    bind_rows(.id = "id") %>% 
    select(-id)

  tblFastQ <- tblFastQ %>% drop_na(fastqFileName)
  print(tblFastQ)
  
  tblLib <- sapply(libFiles, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% 
    select(-id) %>% print()
  
  
  tblS2 <- sapply(s2Files, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% 
    select(-id) %>% print()
  
  tblS1 <- sapply(s1Files, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% 
    select(-id) %>% print()


  tblRNA <- sapply(rnaFiles, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% 
    select(-id) %>% print()


  tblBio <- sapply(bioFiles, read_excel, simplify=FALSE) %>%
    bind_rows(.id = "id") %>% select(-id)

  
  # perform all joins
  print("joining")
  db <- tblFastQ %>%
        left_join(tblLib) %>%
        left_join(tblS2) %>%
        left_join(tblS1) %>%
        left_join(tblRNA) %>%
        left_join(tblBio)

  # drops all observations which do not have a corresponding FastQ filepath.
  db <- db %>% drop_na(fastqFileName)
  db <- db %>% mutate(fastqFilePath = paste("sequence/run_", runNumber, "_samples", fastqFileName, sep = ""))
  db <- db %>% mutate(tsvFilePath = gsub(".fastq.gz", "_read_count.tsv", fastqFilePath))
 
  # prints database to csv file - code validation
  write_excel_csv(db, paste(output, "database.csv"))
  return(db)
}
# HELP
# Stuck on how to specify optional arguments in this function. I need default to match to any entry.
# maybe figured it out?? -- needs to be tested with command line arguments, requires separate option for each query term. 
# Default value is the set of all possible values for the term shown below.

query_db <- function(database, tp, tr, id, fm, gt, prp, str){

  query <- database %>% filter(str_detect(as.character(timePoint), as.character(tp))) %>%
    filter(str_detect(as.character(purpose), as.character(prp))) %>%
    filter(str_detect(as.character(treatment), as.character(tr))) %>%
    filter(str_detect(as.character(inductionDelay), as.character(id))) %>%
    filter(str_detect(as.character(genotype), as.character(gt))) %>%
    filter(str_detect(as.character(floodmedia), as.character(fm)))

return(query)
}

write_summary <- function(query, output){

  selectQ <- query %>% select(genotype, strain, inductionDelay, libraryDate, harvestDate, replicate, runNumber, index1Sequence, index2Sequence, fastqFileName, timePoint, floodmedia)
  # print(selectQ)
  summary <- mutate(selectQ,  ST_PIPE = "",
                   ST_TOTAL_READS	= "",
                   ST_ALIGN_PCT	= "",
                   ST_MUT_FOW	= "",
                   ST_RC_FOM	= "",
                   ST_COV_MED	= "",
                   AUTO_AUDIT	= "",
                   MANUAL_AUDIT	= "",
                   USER	= "",
                   NOTE = "")
  
  # write file to csv at specified filepath
  write_excel_csv(summary, paste(output, "sample_summary.csv"))
  return()
}

write_lookup <- function(query, output){
  lookupDB <- tibble(query$tsvFilePath)
  write.table(lookupDB, paste(output, "query1.expr.lookup.txt"), row.names = TRUE, col.names = FALSE, sep = "\t", quote = FALSE)
  write.table(lookupDB, paste(output, "query1.fastq.lookup.txt"), row.names = TRUE, col.names = FALSE, sep = "\t", quote = FALSE)
}

# Main Method
# This method with create a database from the inputted excel files, query the database, and return a sample_summary sheet ready for Quality Assessment inth 

parsed <- parse_arguments()
# print(parsed)

basePath <- as.character(parsed$input_directory)
# basePath <- "/Users/SanjiBhavsar/Box Sync/!!!WashU/Research/500 - Inscripta Project/Database/Practice"

# fqFiles <- list.files(path = basePath, pattern = regex("fastq.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# libFiles <- list.files(path = basePath, pattern = regex("library.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# s2Files <- list.files(path = basePath, pattern = regex("s2cdna.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# s1Files <- list.files(path = basePath, pattern = regex("s1cdna.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# rnaFiles <- list.files(path = basePath, pattern = regex("rnaSample.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)
# bioFiles <- list.files(path = basePath, pattern = regex("bioSample.*.xlsx", ignore_case = TRUE), full.names = T, recursive = TRUE)


fqFiles <- list.files(path = basePath, pattern = ".astq.*.xlsx", full.names = T, recursive = TRUE)
libFiles <-  list.files(path = basePath, pattern = ".ibrary.*.xlsx", full.names = T, recursive = TRUE)
s2Files <-  list.files(path = basePath, pattern = ".2.DNA.*.xlsx", full.names = T, recursive = TRUE)
s1Files <-  list.files(path = basePath, pattern = ".1.DNA.*.xlsx", full.names = T, recursive = TRUE)
rnaFiles <- list.files(path = basePath, pattern = ".naSample.*.xlsx", full.names = T, recursive = TRUE)
bioFiles <- list.files(path = basePath, pattern = ".ioSample.*.xlsx", full.names = T, recursive = TRUE)

# # check to see if any of lists above are empty - will throw unary operator error if empty
types <- c(fqFiles, libFiles, s2Files, s1Files, rnaFiles, bioFiles)
for(i in types){
  if(is.null(i)){
    print(paste(i, " is empty. Check Files building code"))
    return()
  }
}

db <- create_db(fqFiles, libFiles, s2Files, s1Files, rnaFiles, bioFiles, parsed$output)
# print(db)
# print(db$timePoint)
query <- query_db(db, tp = parsed$timepoint,
                      tr = parsed$treatment,
                      prp = parsed$purpose,
                      str = parsed$strain,
                      gt = parsed$genotype,
                      fm = parsed$floodmedia,
                      id = parsed$inductionDelay)
write_summary(query, parsed$output)
write_lookup(query, parsed$output)
# print(parsed)
# print(query)