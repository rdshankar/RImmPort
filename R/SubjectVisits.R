#' Subject Visits Domain
#' 
#' @name Subject Visits Domain
#' @description The Demographics data of an ImmPort study is reformated to the CDISC SDTM Subject Visits (SV) 
#' domain model, and is a list of 2 data frames containing 1) Subject Visits data \code{\link{SV}} 
#' and 2) any supplemental Subject Visits data \code{\link{SUPP}}
NULL
#> NULL 

# Get Subject Visits data of a specific study
# 
# The function \code{getSubjectVisits} queries the ImmPort database for Subject Visits data and 
# reformats it to the CDISC SDTM Subject Visits (SV) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Subject Visits data \code{\link{SV}} and 2) any supplemental 
#   Subject Visits data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getSubjectVisits(data_src, "SDY1")
# }
getSubjectVisits <- function(data_src, study_id) {
    cat("loading Subject Visits data....")

    sv_cols <- c("STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "SVSTDY")
  
    subjectVisitsSQL <- paste("SELECT distinct
                                av.study_accession,
                                \"SV\" as domain,
                                av.subject_accession,
                                pv.order_number,
                                pv.visit_name,
                                av.study_day_visit_starts
                              FROM actual_visit av,
                                planned_visit pv
                              WHERE av.planned_visit_accession = pv.planned_visit_accession AND 
                                av.study_accession in ('", study_id, "')
                              ORDER BY av.subject_accession", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      sv_df <- dbGetQuery(data_src, statement = subjectVisitsSQL)
      colnames(sv_df) <- sv_cols
      suppsv_df <- data.frame()
      if (nrow(sv_df) > 0) {
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Subject Visits")
      sv_df <- l[[1]]
      suppsv_df <- l[[2]]
    }
    
    cat("done", "\n")
    
    sv_l <- list()
    if (nrow(sv_df) > 0)
      sv_l <- list(sv_df=sv_df, suppsv_df=suppsv_df)
    
    sv_l
}

# Get count of Subject Visits data of a specific study
# 
# The function \code{getCountOfSubjectVisits} queries the ImmPort database for count 
# of Subject Visits data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Subject Visits data 
# @examples
# \dontrun{
#   # get count of study SDY1's Subject Visits data
#   count <- getCountOfSubjectVisits(conn, "SDY1")
# }
getCountOfSubjectVisits <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM actual_visit av,
                        planned_visit pv
                      WHERE av.planned_visit_accession = pv.planned_visit_accession
                        AND av.study_accession in ('", study_id, "')", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Subject Visits Domain Variables
##' @name SV
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     SVSTDY \tab Study Day of Start of Visit
##'   }
##' }
NULL
#> NULL 

