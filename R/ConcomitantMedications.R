#' Concomitant Medications Domain
#' 
#' @name Concomitant Medications Domain
#' @description The Concomitant Medications data of an ImmPort study is reformated to the CDISC SDTM 
#' Concomitant Medications (CM) domain model, and is a list of 2 data frames containing 
#' 1) Concomitant Medications data \code{\link{CM}} and 2) any supplemental Concomitant 
#' Medications data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("CMSEQ"))

# Get Concomitant Medications data of a specific study
# 
# The function \code{getConcomitantMedications} queries the ImmPort database for Concomitant Medications data and 
# reformats it to the CDISC SDTM Concomitant Medications (CM) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Concomitant Medications data \code{\link{CM}} and 2) any supplemental 
#   Concomitant Medications data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getConcomitantMedications(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
getConcomitantMedications <- function(data_src, study_id) {
    cat("loading Concomitant Medications data....")
    
    cm_cols <- c("STUDYID", "DOMAIN", "USUBJID", "CMSEQ", "CMTRT", "CMCAT", "CMDOSE", "CMDOSTXT", 
               "CMDOSU", "CMDOSFREQ", "CMROUTE", "CMSTDTC", "CMENDTC", "CMSTDY", "CMENDY")
 
  
    sql_stmt <- paste("SELECT distinct
                        sub.study_accession,
                        \"CM\" as domain,
                        sub.subject_accession,
                        cast(0 as UNSIGNED INTEGER) as seq,
                        sub.compound_name_reported,
                        sub.merge_name_reported,
                        sub.dose,
                        sub.dose_reported,
                        sub.dose_units,
                        sub.dose_freq_per_interval,
                        sub.route_of_admin_reported,
                        sub.start_time,
                        sub.end_time,
                        sub.start_day,                    
                        sub.end_day
                      FROM  substance_merge sub
                      WHERE sub.study_accession in ('", study_id, "') AND 
                        sub.compound_role='Concomitant Medication'
                      ORDER BY sub.subject_accession", sep = "")

    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      cm_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(cm_df) <- cm_cols
      suppcm_df <- data.frame()
  
      if (nrow(cm_df) > 0) {
        cm_df <- transform(cm_df, CMSEQ = as.integer(CMSEQ))
        cm_dt <- as.data.table(cm_df)
        if (is.data.table(cm_dt) == TRUE) {
          cm_dt[, `:=`(CMSEQ, seq_len(.N)), by = "USUBJID"]
        }
        cm_df <- as.data.frame(cm_dt)
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Concomitant Medications")
      cm_df <- l[[1]]
      suppcm_df <- l[[2]]
    }  
    
    cat("done", "\n")
    
    cm_l <- list()
    if (nrow(cm_df) > 0)
      cm_l <- list(cm_df=cm_df, suppcm_df=suppcm_df)
    
    cm_l
}

# Get count of Concomitant Medications data of a specific study
# 
# The function \code{getCountOfConcomitantMedications} queries the ImmPort database for count 
# of Concomitant Medications data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Concomitant Medications data 
# @examples
# \dontrun{
#   # get count of study SDY1's Concomitant Medications data
#   count <- getCountOfConcomitantMedications(conn, "SDY1")
# }
getCountOfConcomitantMedications <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)                    
                      FROM  substance_merge sub                    
                      WHERE sub.study_accession in ('", study_id, "') AND 
                      sub.compound_role='Concomitant Medication'", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Concomitant Medications Domain Variables
##' @name CM
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     CMSEQ \tab Sequence Number \cr
##'     CMTRT \tab Reported Name of Drug, Med, or Therapy \cr
##'     CMCAT \tab Category for Medication \cr
##'     CMDOSE \tab Dose per Administration \cr
##'     CMDOSTXT \tab Dose Description \cr
##'     CMDOSU \tab Dose Units \cr
##'     CMDOSFREQ \tab Dosing Frequency per Interval  \cr
##'     CMROUTE \tab Route of Administration \cr
##'     CMSTDTC \tab Start Date/Time of Medication \cr
##'     CMENDTC \tab End Date/Time of Medication \cr
##'     CMSTDY \tab Study Day of Start of Medication \cr
##'     CMENDY \tab Study Day of End of Medication
##'   }
##' }
NULL
#> NULL

