#' Exposure Domain
#' 
#' @name Exposure Domain
#' @description The Exposure data of an ImmPort study is reformated to the CDISC SDTM Exposure (EX) 
#' domain model, and is a list of 2 data frames containing 1) Exposure data \code{\link{EX}} 
#' and 2) any supplemental Exposure data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("EXSEQ"))

# Get Exposure data of a specific study
# 
# The function \code{getExposure} queries the ImmPort database for Exposure data and 
# reformats it to the CDISC SDTM Exposure (EX) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Exposure data \code{\link{EX}} and 2) any supplemental 
#   Exposure data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getExposure(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
getExposure <- function(data_src, study_id) {
    cat("loading Exposure data....")
    
    ex_cols <- c("STUDYID", "DOMAIN", "USUBJID", "EXSEQ", "EXTRT", "EXCAT", "EXDOSE", "EXDOSTXT", "EXDOSU", "EXDOSFRQ", 
               "EXROUTE", "EXSTDTC", "EXENDTC", "EXSTDY", "EXENDY")
  
  
    sql_stmt <- paste("SELECT distinct
                        sub.study_accession,
                        \"EX\" as domain,
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
                        sub.compound_role='Intervention'                    
                      ORDER BY sub.subject_accession", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      ex_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(ex_df) <- ex_cols
      suppex_df <- data.frame()
      
      if (nrow(ex_df) > 0) {
        ex_df <- transform(ex_df, EXSEQ = as.integer(EXSEQ))
        ex_dt <- as.data.table(ex_df)
        if (is.data.table(ex_dt) == TRUE) {
          ex_dt[, `:=`(EXSEQ, seq_len(.N)), by = "USUBJID"]
        }
        ex_df <- as.data.frame(ex_dt)
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Exposure")
      ex_df <- l[[1]]
      suppex_df <- l[[2]]
    }

    cat("done", "\n")
    
    ex_l <- list()
    if (nrow(ex_df) > 0)
      ex_l <- list(ex_df=ex_df, suppex_df=suppex_df)
    
    ex_l
}

# Get count of Exposure data of a specific study
# 
# The function \code{getCountOfExposure} queries the ImmPort database for count 
# of Exposure data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Exposure data 
# @examples
# \dontrun{
#   # get count of study SDY1's Exposure data
#   count <- getCountOfExposure(conn, "SDY1")
# }
getCountOfExposure <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                       FROM  substance_merge sub
                       WHERE sub.study_accession in ('", study_id, "') AND 
                         sub.compound_role='Intervention'", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Exposure Domain Variables
##' @name EX
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     EXSEQ \tab Sequence Number \cr
##'     EXTRT \tab Name of Treatment \cr
##'     EXCAT \tab Category of Treatment \cr
##'     EXDOSE \tab Dose \cr
##'     EXDOSTXT \tab Dose Description \cr
##'     EXDOSU \tab Dose Units \cr
##'     EXDOSFRQ \tab Dosing Frequency per Interval \cr
##'     EXROUTE \tab Route of Administration \cr
##'     EXSTDTC \tab Start Date/Time of Treatment \cr
##'     EXENDTC \tab End Date/Time of Treatment \cr
##'     EXSTDY \tab Study Day of Start of Treatment \cr
##'     EXENDY \tab Study Day of End of Treatment
##'   }
##' }
NULL
#> NULL 
