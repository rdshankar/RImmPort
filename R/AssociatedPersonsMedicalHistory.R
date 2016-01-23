#' Associated Persons Medical History Domain
#' 
#' @name Associated Persons Medical History Domain
#' @description The Associated Persons Medical History data of an ImmPort study is reformated to the CDISC 
#' SDTM AAssociated Persons Medical History (APMH) domain model, and is a list of 2 data frames 
#' containing 1) Associated Persons Medical History data \code{\link{APMH}} and 2) any supplemental 
#' Associated Persons Medical History data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("APID", "MHSEQ", "RSUBJID", "SREL"))

# medical_history_column_names <- c('study_id', 'domain', 'subject_id', 'reported_term_for_the_medical_history',
# 'category_for_medical_history', 'body_system_or_organ_class', 'age_at_onset', 'age_at_onset_unit',
# 'date_time_of_collection', 'study_day_of_collection')

# Get Associated Persons Medical History data of a specific study
# 
# The function \code{getAssociatedPersonsMedicalHistory} queries the ImmPort database for Associated Persons Medical History data and 
# reformats it to the CDISC SDTM Associated Persons Medical History (APMH) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Associated Persons Medical History data \code{\link{APMH}} and 2) any supplemental 
#   Associated Persons Medical History data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getAssociatedPersonsMedicalHistory(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom plyr .
#' @importFrom data.table as.data.table is.data.table := .N
getAssociatedPersonsMedicalHistory <- function(data_src, study_id) {
  cat("loading Associated Persons Medical History data....")

  apmh_cols <- c("STUDYID", "DOMAIN", "APID", "MHSEQ", "RSUBJID", "SREL", "MHTERM", "MHCAT", "MHBODSYS", "MHDY")
  
  suppapmh_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
  sql_stmt <- paste("SELECT distinct
                      asm.study_accession,
                      \"MH\" as domain,
                      cast(0 as UNSIGNED INTEGER) as apid,
                      cast(0 as UNSIGNED INTEGER) as seq,
                      asm.subject_accession,
                      asm.who_is_assessed,
                      asm.component_name_reported,
                      asm.panel_name_reported,
                      asm.organ_or_body_system_reported,
                      asm.study_day                    
                    FROM  assessment asm
                    WHERE asm.study_accession in ('", study_id, "') AND 
                      asm.assessment_type='Family History'
                    ORDER BY asm.subject_accession", sep = "")
  
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    apmh_df <- dbGetQuery(data_src, statement = sql_stmt)
    colnames(apmh_df) <- apmh_cols
  
    suppapmh_df <- data.frame()
  
    if (nrow(apmh_df) > 0) {
      apmh_df <- transform(apmh_df, APID = as.integer(APID))
      apmh_df <- transform(apmh_df, MHSEQ = as.integer(MHSEQ))
      apmh_dt <- as.data.table(apmh_df)
      if (is.data.table(apmh_dt) == TRUE) {
        apmh_dt[, `:=`(MHSEQ, seq_len(.N)), by = .(RSUBJID,SREL)]
      }
      if (is.data.table(apmh_dt) == TRUE) {
        apmh_dt[, `:=`(APID, seq_len(.N)), by = "MHSEQ"]
      }
      apmh_df <- as.data.frame(apmh_dt)
    }
  } else {
    l <- loadSerializedStudyData(data_src, study_id, "Associated Persons Medical History")
    apmh_df <- l[[1]]
    suppapmh_df <- l[[2]]
  }
  
  
  
  cat("done", "\n")
  
  apmh_l <- list()
  if (nrow(apmh_df) > 0)
    apmh_l <- list(apmh_df=apmh_df, suppapmh_df=suppapmh_df)
  
  apmh_l
}

# Get count of Associated Persons Medical History data of a specific study
# 
# The function \code{getCountOfAssociatedPersonsMedicalHistory} queries the ImmPort database for count 
# of Associated Persons Medical History data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Associated Persons Medical History data 
# @examples
# \dontrun{
#   # get count of study SDY1's Associated Persons Medical History data
#   count <- getCountOfAssociatedPersonsMedicalHistory(conn, "SDY1")
# }
getCountOfAssociatedPersonsMedicalHistory <- function(conn, study_id) {
  sql_stmt <- paste("SELECT count(*)
                    FROM  assessment asm
                    WHERE asm.study_accession in ('", study_id, "') AND 
                    asm.assessment_type='Family History'", sep = "")
  
  count <- dbGetQuery(conn, statement = sql_stmt)
  
  count[1, 1]
} 

##' Associated Persons Medical History Domain Variables
##' @name APMH
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     APID \tab Associated Persons Identifier \cr
##'     MHSEQ \tab Sequence Number \cr
##'     RSUBJID \tab Related Subject \cr
##'     SREL \tab Subject Relationship \cr
##'     MHTERM \tab Reported Term for the Medical History \cr
##'     MHCAT \tab Category for Medical History \cr
##'     MHBODSYS \tab Body System or Organ Class \cr
##'     MHDTC \tab Date/Time of History Collection \cr
##'     MHDY \tab RStudy Day of History Collection
##'   }
##' }
NULL
#> NULL 
