## ------------------------------------------------------------------------
library(RImmPort)
library(DBI)
library(sqldf)
library(plyr)


# OPTION1: MySQL-formatted files as ImmPort data source
#
#  # provide appropriate connection parameters
#  mysql_conn <- dbConnect(MySQL(), user="username", password="password",
#                     dbname="database",host="host")
#  

# connection parameters for ImmPort MySQL database on the Amazon cloud
dbType = "MySQL"
dbUser = 'lab_user'
dbPassword = 'big_data'
dbHost = 'exxport-mysql.celiaramktyo.us-west-2.rds.amazonaws.com'
dbDatabase = 'proj_study_ALLSTUDIES'
dbPort= 3306

# connect to the database
conn <- dbConnect(dbDriver(dbType),user=dbUser,password=dbPassword,dbname=dbDatabase, host=dbHost,port=dbPort)

setImmPortDataSource(mysql_conn)

# OPTION2: Tab-formatted files as ImmPort data source

# get the directory where ImmPort sample data is stored in the directory structure of RImmPort package
studies_dir <-"~/ImmPortStudies"

# set tab_dir to the folder where the zip files are located
tab_dir <- file.path(studies_dir, "Tab")
list.files(tab_dir)

# set db_dir to the folder where the database file 'ImmPort.sqlite' should be stored
db_dir <- file.path(studies_dir, "Db")

# build a new ImmPort SQLite database with the data in the downloaded zip files
buildNewSqliteDb(tab_dir, db_dir)

list.files(db_dir)

# connect to the private instance of the ImmPort database
sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))

setImmPortDataSource(sqlite_conn)

# OPTION3: R-formatted files as ImmPort data source

# get the directory where ImmPort sample data is stored in the directory structure of RImmPort package
studies_dir <-"~/ImmPortStudies"

# the folder where the .rds files will be stored
rds_dir <- file.path(studies_dir, "Rds")

# list the studies that have been serialized
list.files(rds_dir)


setImmPortDataSource(rds_dir)


# Get different types of study data

# get list of studies in the data soutce
getListOfStudies()

# view help on Study 
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
zb_df <- zb_l$zb_df
head(zb_df)

?"Cellular Quantification Domain"

## ------------------------------------------------------------------------
getListOfAssayTypes()

## ------------------------------------------------------------------------

# get 'ELISPOT' data of study `SDY139`
assay_type <- "ELISPOT"
study_id = "SDY139"
elispot_l <- getAssayDataOfStudies(study_id, assay_type)
if (length(elispot_l) > 0)
  names(elispot_l)
zb_df <- elispot_l$zb_df
head(zb_df)


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




