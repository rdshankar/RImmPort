## ------------------------------------------------------------------------
library(RImmPort)
library(DBI)
library(sqldf)
library(plyr)


## ----eval=FALSE----------------------------------------------------------
#  
#  # provide appropriate connection parameters
#  mysql_conn <- dbConnect(MySQL(), user="username", password="password",
#                     dbname="database",host="host")
#  

## ----eval=FALSE----------------------------------------------------------
#  setImmPortDataSource(mysql_conn)

## ------------------------------------------------------------------------
# get the directory where ImmPort sample data is stored in the directory structure of RImmPort package
studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")

# set tab_dir to the folder where the zip files are located
tab_dir <- file.path(studies_dir, "Tab")
list.files(tab_dir)

## ------------------------------------------------------------------------
# set db_dir to the folder where the database file 'ImmPort.sqlite' should be stored
db_dir <- file.path(studies_dir, "Db")

## ----eval=FALSE----------------------------------------------------------
#  # build a new ImmPort SQLite database with the data in the downloaded zip files
#  buildNewSqliteDb(tab_dir, db_dir)

## ------------------------------------------------------------------------
list.files(db_dir)

## ------------------------------------------------------------------------

# connect to the private instance of the ImmPort database
sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))

## ------------------------------------------------------------------------
setImmPortDataSource(sqlite_conn)

## ------------------------------------------------------------------------
getListOfStudies()

## ------------------------------------------------------------------------
?Study

# load all the data of study: `SDY139`
study_id <- 'SDY139'
sdy139 <- getStudy(study_id)

# access Demographics data of SDY139
dm_df <- sdy139$special_purpose$dm_l$dm_df
head(dm_df)

# access Concomitant Medications data of SDY139
cm_df <- sdy139$interventions$cm_l$cm_df
head(cm_df)

# get Trial Title from Trial Summary
ts_df <- sdy139$trial_design$ts_l$ts_df
title <- ts_df$TSVAL[ts_df$TSPARMCD== "TITLE"]
title

## ------------------------------------------------------------------------

# get the list of names of all supported Domains
getListOfDomains()

?"Demographics Domain"


## ------------------------------------------------------------------------
# get list of studies with Cellular Quantification data
domain_name <- "Cellular Quantification"
study_ids_l <- getStudiesWithSpecificDomainData(domain_name)
study_ids_l

## ------------------------------------------------------------------------
# get Cellular Quantification data of studies `SDY139` and `SDY208`

# get domain code of Cellular Quantification domain
domain_name <- "Cellular Quantification"
getDomainCode(domain_name)

study_ids <- c("SDY139", "SDY208")
domain_name <- "Cellular Quantification"
zb_l <- getDomainDataOfStudies(domain_name, study_ids)
if (length(zb_l) > 0) 
  names(zb_l)
head(zb_l$zb_df)

## ------------------------------------------------------------------------
getListOfAssayTypes()

## ------------------------------------------------------------------------

# get 'ELISPOT' data of study `SDY139`
assay_type <- "ELISPOT"
study_id = "SDY139"
elispot_l <- getAssayDataOfStudies(study_id, assay_type)
if (length(elispot_l) > 0)
  names(elispot_l)
head(elispot_l$zb_df)


## ----eval=FALSE----------------------------------------------------------
#  
#  # serialize all of the data of studies `SDY139` and `SDY208'
#  study_ids <- c('SDY139', 'SDY208')
#  
#  # the folder where the .rds files will be stored
#  rds_dir <- file.path(studies_dir, "Rds")
#  
#  serialzeStudyData(study_ids, rds_dir)
#  list.files(rds_dir)
#  

## ------------------------------------------------------------------------
# get the directory where ImmPort sample data is stored in the directory structure of RImmPort package
studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")

# the folder where the .rds files will be stored
rds_dir <- file.path(studies_dir, "Rds")

# list the studies that have been serialized
list.files(rds_dir)

# load the serialized data of study `SDY208` 
study_id <- 'SDY208'
dm_l <- loadSerializedStudyData(rds_dir, study_id, "Demographics")
head(dm_l[[1]])


