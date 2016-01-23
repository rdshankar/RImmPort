#' Skin Response Domain
#' 
#' @name Skin Response Domain
#' @description The Skin Response data of an ImmPort study is reformated to the CDISC SDTM Skin Response (SR) 
#' domain model, and is a list of 2 data frames containing 1) Skin Response data \code{\link{SR}} 
#' and 2) any supplemental Skin Response data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("SRSEQ"))

# Get Skin Response data of a specific study
# 
# The function \code{getSkinResponse} queries the ImmPort database for Skin Response data and 
# reformats it to the CDISC SDTM Skin Response (SR) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Skin Response data \code{\link{SR}} and 2) any supplemental 
#   Skin Response data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getSkinResponse(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
getSkinResponse <- function(data_src, study_id) {
  cat("loading Skin Response data....")

  sr_cols <- c("STUDYID", "DOMAIN", "USUBJID", "SRSEQ", "SRTEST", "SRCAT", "SROBJ", 
               "SRORRES", "SRORRESU", "SRLOC", "VISITNUM", "VISIT", "SRDY")
  
  sql_stmt <- paste("SELECT distinct
                    asm.study_accession,
                    \"SR\" as domain,
                    asm.subject_accession,
                    cast(0 as UNSIGNED INTEGER) as seq,
                    asm.component_name_reported,
                    asm.panel_name_reported,
                    asm.organ_or_body_system_reported,
                    asm.result_value_reported,
                    asm.result_unit_reported,
                    asm.location_of_finding_reported,
                    pv.order_number,
                    pv.visit_name,
                    asm.study_day                    
                    FROM  assessment asm, planned_visit pv
                    WHERE (asm.study_accession in ('", study_id, "')) AND 
                        (asm.assessment_type='Skin Assessment') AND
                        (asm.planned_visit_accession = pv.planned_visit_accession)
                    ORDER BY asm.subject_accession", sep = "")
  
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    sr_df <- dbGetQuery(data_src, statement = sql_stmt)
    colnames(sr_df) <- sr_cols
    suppsr_df <- data.frame()
    if (nrow(sr_df) > 0) {
      sr_df <- transform(sr_df, SRSEQ = as.integer(SRSEQ))
      sr_dt <- as.data.table(sr_df)
      if (is.data.table(sr_dt) == TRUE) {
        sr_dt[, `:=`(SRSEQ, seq_len(.N)), by = "USUBJID"]
      }
      sr_df <- as.data.frame(sr_dt)
    }
  } else {
    l <- loadSerializedStudyData(data_src, study_id, "Skin Response")
    sr_df <- l[[1]]
    suppsr_df <- l[[2]]
  }

  cat("done", "\n")
  
  sr_l <- list()
  if (nrow(sr_df) > 0)
    sr_l <- list(sr_df=sr_df, suppsr_df=suppsr_df)
  
  sr_l
}

# Get count of Skin Response data of a specific study
# 
# The function \code{getCountOfSkinResponse} queries the ImmPort database for count 
# of Skin Response data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Skin Response data 
# @examples
# \dontrun{
#   # get count of study SDY1's Skin Response data
#   count <- getCountOfSkinResponse(conn, "SDY1")
# }
getCountOfSkinResponse <- function(conn, study_id) {
  sql_stmt <- paste("SELECT count(*)
                    FROM  assessment asm
                    WHERE asm.study_accession in ('", study_id, "') AND 
                    asm.assessment_type='Skin Assessment'", sep = "")
  
  count <- dbGetQuery(conn, statement = sql_stmt)
  
  count[1, 1]
} 

##' Skin Response Domain Variables
##' @name SR
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     SRSEQ \tab Sequence Number \cr
##'     SRTEST \tab Skin Response Test or Examination Name \cr
##'     SROBJ \tab Object of the Observation \cr
##'     SRCAT \tab Category for Test \cr
##'     SRORRES \tab Results or Findings in Original Units \cr
##'     SRORRESU \tab Original Units \cr
##'     SRLOC \tab Location used for Measurement \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     SRDY \tab Study Day of Visit/Collection/Exam
##'   }
##' }
NULL
#> NULL 

