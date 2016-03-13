#' Demographics Domain
#' 
#' @name Demographics Domain
#' @description The Demographics data of an ImmPort study is reformated to the CDISC SDTM Demographics (DM) 
#' domain model, and is a list of 2 data frames containing 1) Demographics data \code{\link{DM}} 
#' and 2) any supplemental Demographics data \code{\link{SUPP}}
NULL
#> NULL 

# Get Demographics data of a specific study
# 
# The function \code{getDemographics} queries the ImmPort database for Demographics data and 
# reformats it to the CDISC SDTM Demographics (DM) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Demographics data \code{\link{DM}} and 2) any supplemental 
#   Demographics data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getDemographics(data_src, "SDY1")
# }
getDemographics <- function(data_src,study_id) {
  cat("loading Demographics data....")

  dm_cols <- c("STUDYID", "DOMAIN","USUBJID", 
               "AGE", "AGEU", "SEX",
               "RACE", "ETHNIC",
               "SPECIES", "STRAIN", "SBSTRAIN",
               "ARMCD", "ARM")
  
  sql_stmt <- paste("
                                  SELECT distinct
                                  a2.study_accession,
                                  \"DM\" as domain,
                                  s1.subject_accession,
                                  s1.age_reported,
                                  s1.age_unit,
                                  s1.gender,
                                  s1.race,
                                  s1.ethnicity,
                                  s1.species,
                                  s1.strain,
                                  s1.strain_characteristics,
                                  a2.arm_accession,
                                  a2.name as arm_name
                                  FROM subject s1,
                                  arm_2_subject a1,
                                  arm_or_cohort a2
                                  WHERE s1.subject_accession = a1.subject_accession
                                  AND a1.arm_accession = a2.arm_accession
                                  AND a2.study_accession in (\'", study_id,"\')
                                  ORDER BY s1.subject_accession",sep="")
  
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    dm_df <- dbGetQuery(data_src,statement=sql_stmt)
    colnames(dm_df) <- dm_cols 
    suppdm_df <- data.frame()
  
#     if (nrow(dm_df) > 0) {
#     
#       if ('Homo sapiens' %in% dm_df$SPECIES) {
#         #remove "SPECIES", "STRAIN", "SBSTRAIN" columns
#         dm_df[,c("SPECIES", "STRAIN", "SBSTRAIN")] <- list(NULL)
#       } else {
#         #remove "RACE", "ETHNIC" columns
#         dm_df[,c("RACE", "ETHNIC")] <- list(NULL)    
#       }
#     }
  } else {
    l <- loadSerializedStudyData(data_src, study_id, "Demographics")
    dm_df <- l[[1]]
    suppdm_df <- l[[2]]
  }
  cat("done", "\n")
  
  dm_l <- list()
  if (nrow(dm_df) > 0)
    dm_l <- list(dm_df=dm_df, suppdm_df=suppdm_df)
  
  dm_l
}


# Get count of Demographics data of a specific study
# 
# The function \code{getCountOfDemographics} queries the ImmPort database for count 
# of Demographics data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Demographics data 
# @examples
# \dontrun{
#   # get count of study SDY1's Demographics data
#   count <- getCountOfDemographics(conn, "SDY1")
# }
getCountOfDemographics <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM subject s1,
                    arm_2_subject a1,
                    arm_or_cohort a2
                    WHERE s1.subject_accession = a1.subject_accession
                    AND a1.arm_accession = a2.arm_accession
                    AND a2.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}

##' Demographics Domain Variables
##' @name DM
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     AGE \tab Age \cr
##'     AGEU \tab Age Units \cr
##'     SEX \tab Sex \cr
##'     RACE \tab Race (only for human data) \cr
##'     ETHNIC \tab Ethnicity (only for human data) \cr
##'     SPECIES \tab Species (only for non-human data) \cr
##'     STRAIN \tab Strain/Substrain (only for non-human data) \cr
##'     SBSTRAIN \tab Strain/Substrain Details (only for non-human data) \cr
##'     ARMCD \tab Planned Arm Code \cr
##'     ARM \tab Description of Planned Arm
##'   }
##' }
NULL
#> NULL

##' Supplemental Variables
##' @name SUPP
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name} \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     RDOMAIN  \tab Related Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     IDVAR \tab Identifying Variable \cr
##'     IDVARVAL \tab Identifying Variable Value \cr
##'     QNAM \tab Qualifier Variable Name \cr
##'     QLABEL \tab Qualifier Variable Label \cr
##'     QVAL \tab Data Value
##'   }
##' }
NULL
#> NULL
