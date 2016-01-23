#' Adverse Events Domain
#' 
#' @name Adverse Events Domain
#' @description The Adverse Events data of an ImmPort study is reformated to the CDISC SDTM Adverse 
#' Events (AE) domain model, and is a list of 2 data frames containing 1) Adverse Events data \code{\link{AE}} 
#' and 2) any supplemental Adverse Events data \code{\link{SUPP}}
NULL
#> NULL 


# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("AESEQ"))

# Get Adverse Events data of a specific study
# 
# The function \code{getAdverseEvents} queries the ImmPort database for Adverse Events data and 
# reformats it to the CDISC SDTM Adverse Events (AE) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Adverse Events data \code{\link{AE}} and 2) any supplemental 
#   Adverse Events data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getAdverseEvents(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
getAdverseEvents <- function(data_src, study_id) {
    cat("loading Adverse Events data....")

    ae_cols <- c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AESPID", "AETERM", "AEMODIFY", "AEBODYSYS", "AELOC", "AESEV", 
               "AESER", "AEACN", "AEACNOTH", "AEREL", "AERELNST", "AEOUT", "AESTDY", "AEENDY")
  
    sql_stmt <- paste("SELECT distinct ae.study_accession,
                        \"AE\" as domain,                    
                        ae.subject_accession,                    
                        cast(0 as UNSIGNED INTEGER) as seq,
                        ae.adverse_event_accession,                    
                        ae.name_reported,                    
                        ae.name_preferred,                    
                        ae.organ_or_body_system_reported,                    
                        ae.location_of_reaction_reported,                    
                        ae.severity_reported,                    
                        ae.is_serious_event,                    
                        ae.study_treatment_action_taken,                    
                        ae.other_action_taken,                    
                        ae.relation_to_study_treatment,                    
                        ae.relation_to_nonstudy_treatment,                    
                        ae.outcome_reported,                    
                        ae.start_study_day,                    
                        ae.end_study_day                    
                      FROM adverse_event ae                    
                      WHERE ae.study_accession in ('", study_id, "')                    
                      ORDER BY ae.subject_accession", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      ae_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(ae_df) <- ae_cols
      suppae_df <- data.frame()
      if (nrow(ae_df) > 0) {
        ae_df <- transform(ae_df, AESEQ = as.integer(AESEQ))
        ae_dt <- as.data.table(ae_df)
        if (is.data.table(ae_dt) == TRUE) ae_dt[, `:=`(AESEQ, seq_len(.N)), by = "USUBJID"]
    
        ae_df <- as.data.frame(ae_dt)
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Adverse Events")
      ae_df <- l[[1]]
      suppae_df <- l[[2]]
    }
        

    cat("done", "\n")
    
    ae_l <- list()
    if (nrow(ae_df) > 0)
      ae_l <- list(ae_df=ae_df, suppae_df=suppae_df)
    
    ae_l
}

# Get count of Adverse Events data of a specific study
# 
# The function \code{getCountOfAdverseEvents} queries the ImmPort database for count 
# of Adverse Events data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Adverse Events data 
# @examples
# \dontrun{
#   # get count of study SDY1's Adverse Events data
#   count <- getCountOfAdverseEvents(conn, "SDY1")
# }
getCountOfAdverseEvents <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM adverse_event ae
                      WHERE ae.study_accession in ('", study_id, "')", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Adverse Events Domain Variables
##' @name AE
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     AESEQ \tab Sequence Number \cr
##'     AESPID \tab Sponsor-Defined Identifier \cr
##'     AETERM \tab Reported Term for the Adverse Event \cr
##'     AEMODIFY \tab Modified Reported Term \cr
##'     AEBODYSYS \tab Body System or Organ Class \cr
##'     AELOC \tab Location of Event \cr
##'     AESEV \tab Severity/Intensity \cr
##'     AESER \tab Serious Event \cr
##'     AEACN \tab Action Taken with Study Treatment \cr
##'     AEACNOTH \tab Other Action Taken \cr
##'     AEREL \tab Causality \cr
##'     AERELNST \tab Relationship to Non-Study Treatment \cr
##'     AEOUT \tab Outcome of Adverse Event \cr
##'     AESTDY \tab Study Day of Start of Adverse Event \cr
##'     AEENDY \tab Study Day of End of Adverse Event
##'   }
##' }
NULL
#> NULL
