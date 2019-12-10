# Query database of csv files for specific files
suppressMessages(library(readxl))
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(optparse))
suppressMessages(library(stringr))
suppressMessages(library(jsonlite))
suppressMessages(library(xlsx))

# read in options from command line code
parse_arguments <- function() {
  option_list <- list(
    make_option(c('-i', '--input_directory'), default = NULL,
                help='Path to main directory for database files'),
    make_option(c('-o', '--output'), default = NULL,
                help='Filepath or directory path for newly created metadata sheet'),
    make_option(c('-j', '--json'), default = NULL,
                help='filepath to JSON query'),
    make_option(c('-q', '--query'), default = "query",
                help='some meaningful name for the query')
  )
  args <- parse_args(OptionParser(option_list=option_list))
  
  # check if input and output are provided
  if (is.null(args$input_directory) || is.null(args$output) || is.null(args$json)) {
    stop("input, output, and json filepath must be provided.")
  }
  else{
    return(args)
  }
  # check if output directory exists. If not, creates output directory at specified path
  if(!dir.exists(args$output))
  {
    dir.create(args$output)
  }
}

# creates a database by reading in all excel files from their proper directories and 
# returns a filled and joined tibble data frame which is filled out with all respective
# fastQ files. This function drops all observations which have empty fastQ file paths.
create_db <- function(fqFiles, libFiles, s2Files, s1Files, rnaFiles, bioFiles, output){
  
  tblFastQ <- sapply(fqFiles, read_excel, simplify=FALSE)  %>%
      bind_rows(.id = "id") %>% 
      select(-id)
  # drops all observations which do not have a corresponding FastQ filepath.
    tblFastQ <- tblFastQ %>% drop_na(fastqFileName)
  tblLib <- sapply(libFiles, read_excel, simplify=FALSE) %>%
      bind_rows(.id = "id") %>% 
      select(-id) 
  tblS2 <- sapply(s2Files, read_excel, simplify=FALSE) %>%
      bind_rows(.id = "id") %>% 
      select(-id) 
  tblS1 <- sapply(s1Files, read_excel, simplify=FALSE) %>%
      bind_rows(.id = "id") %>% 
      select(-id) 
  tblRNA <- sapply(rnaFiles, read_excel, simplify=FALSE) %>%
      bind_rows(.id = "id") %>% 
      select(-id) 
  tblBio <- sapply(bioFiles, read_excel, simplify=FALSE) %>%
      bind_rows(.id = "id") %>% 
      select(-id)
  
  # perform all joins
  db <- tblFastQ %>%
        left_join(tblLib) %>%
        left_join(tblS2) %>%
        left_join(tblS1) %>%
        left_join(tblRNA) %>%
        left_join(tblBio)
  
  # adds vectors holding path to fastq and tsv files
  db <- db %>% mutate(fastqFilePath = paste("sequence/run_", runNumber, "_samples", fastqFileName, sep = ""))
  db <- db %>% mutate(tsvFilePath = gsub(".fastq.gz", "_read_count.tsv", fastqFilePath))
 
  # prints database to csv file - code validation
  # write_excel_csv(db, paste(output, "database.csv"))
  return(db)
}

# This function takes conditions as arguments and returns the observations in the dataframe which match the conditions provided
# This code was written by https://stackoverflow.com/users/3460670/ben
query_db <- function(df, cols, conds){
  fp <- map2(cols, conds, function(x, y) quo((!!(as.name(x))) %in% !!y))
  filter(df, !!!fp)
}

write_summary <- function(query, qname, output){

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
  # write_excel_csv(summary, paste(output, qname, "sample_summary.csv", sep = "/"))
  write.xlsx(x = as.data.frame(summary), file = paste(output, qname, "sample_summary.xlsx", sep = "/"), row.names = FALSE)
  
  return()
}

write_lookup <- function(query, qname, output){
  lookupTSV <- tibble(query$tsvFilePath)
  lookupFQ <- tibble(query$fastqFilePath)
  write.table(lookupTSV, paste(output, "/" , qname, "/" , qname, ".expr.lookup.txt", sep = ""), row.names = TRUE, col.names = FALSE, sep = "\t", quote = FALSE)
  write.table(lookupFQ, paste(output, "/" , qname, "/" , qname, ".fastq.lookup.txt", sep = ""), row.names = TRUE, col.names = FALSE, sep = "\t", quote = FALSE)
}

# Main Method
# This method with create a database from the inputted excel files, query the database, and return a sample_summary sheet, a lookup file, and 
parsed <- parse_arguments()

# creates directory for query specific outputs within output directory
dir.create(file.path(parsed$output, parsed$query, fsep = "/"))
basePath <- as.character(parsed$input_directory)

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

json_df <- fromJSON(txt = parsed$json)
df <- unnest(json_df, cols = everything())

query <- query_db(db, cols = as.list(names(df)), conds = as.list(df))

# write output files
# write_excel_csv(query, paste(parsed$output, parsed$query, "queriedDB.csv", sep = "/"))
write.xlsx(x = as.data.frame(query), file = paste(parsed$output, parsed$query, "queriedDB.xlsx", sep = "/"), row.names = FALSE)
write_summary(query, qname = parsed$query, output = parsed$output)
write_lookup(query, qname = parsed$query, output = parsed$output)

# print(query)
# write_excel_csv(query, paste(parsed$output, "queriedDB.csv"))