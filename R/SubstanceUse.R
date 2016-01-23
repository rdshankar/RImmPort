#' Substance Use Domain
#' 
#' @name Substance Use Domain
#' @description The Substance Use data of an ImmPort study is reformated to the CDISC SDTM Substance Use (SU) 
#' domain model, and is a list of 2 data frames containing 1) Substance Use data \code{\link{SU}} 
#' and 2) any supplemental Substance Use data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("SUSEQ"))

# Get Substance Use data of a specific study
# 
# The function \code{getSubstanceUse} queries the ImmPort database for getSubstanceUse data and 
# reformats it to the CDISC SDTM Substance Use (SU) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Substance Use data \code{\link{SU}} and 2) any supplemental 
#   Substance Use data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getSubstanceUse(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
getSubstanceUse <- function(data_src, study_id) {
    cat("loading Substance Use data....")

    su_cols <- c("STUDYID", "DOMAIN", "USUBJID", "SUSEQ", "SUTRT", "SUCAT", "SUDOSE", "SUDOSTXT", "SUDOSU", 
                "SUDOSFREQ", "SUROUTE", "SUSTDTC", "SUENDTC", "SUSTDY", "SUENDY")
  
  
    sql_stmt <- paste("SELECT distinct
                        sub.study_accession,
                        \"SU\" as domain,
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
                        sub.compound_role='Substance Use'
                      ORDER BY sub.subject_accession", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      su_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(su_df) <- su_cols
      suppsu_df <- data.frame()
      if (nrow(su_df) > 0) {
        su_df <- transform(su_df, SUSEQ = as.integer(SUSEQ))
        su_dt <- as.data.table(su_df)
        if (is.data.table(su_dt) == TRUE) {
          su_dt[, `:=`(SUSEQ, seq_len(.N)), by = "USUBJID"]
        }
        su_df <- as.data.frame(su_dt)
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Substance Use")
      su_df <- l[[1]]
      suppsu_df <- l[[2]]
    }

    cat("done", "\n")  
    
    su_l <- list()
    if (nrow(su_df) > 0)
      su_l <- list(su_df=su_df, suppsu_df=suppsu_df)
    
    su_l
}

# Get count of Substance Use data of a specific study
# 
# The function \code{getCountOfSubstanceUse} queries the ImmPort database for count 
# of Substance Use data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Substance Use data 
# @examples
# \dontrun{
#   # get count of study SDY1's Substance Use data
#   count <- getCountOfSubstanceUse(conn, "SDY1")
# }
getCountOfSubstanceUse <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM  substance_merge sub
                      WHERE sub.study_accession in ('", study_id, "') AND 
                        sub.compound_role='Substance Use'", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Substance Use Domain Variables
##' @name SU
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     SUSEQ \tab Sequence Number \cr
##'     SUTRT \tab Reported Name of Substance \cr
##'     SUCAT \tab Category of Substance Use \cr
##'     SUDOSE \tab Substance Use Consumption \cr
##'     SUDOSTXT \tab Substance Use Consumption Text \cr
##'     SUDOSU \tab Consumption Units \cr
##'     SUDOSFRQ \tab Use Frequency per Interval \cr
##'     SUROUTE \tab Route of Administration \cr
##'     SUSTDTC \tab Start Date/Time of Substance Use \cr
##'     SUENDTC \tab End Date/Time of Substance Use \cr
##'     SUSTDY \tab Study Day of Start of Substance Use \cr
##'     SUENDY \tab Study Day of End of Substance Use
##'   }
##' }
NULL
#> NULL 
