#' Questionnaires Domain
#' 
#' @name Questionnaires Domain
#' @description The Questionnaires data of an ImmPort study is reformated to the CDISC SDTM 
#' Questionnaires (QS) domain model, and is a list of 2 data frames containing 1) Questionnaires 
#' data \code{\link{QS}} and 2) any supplemental Questionnaires data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("QSSEQ"))

# Get Questionnaires data of a specific study
# 
# The function \code{getQuestionnaires} queries the ImmPort database for Questionnaires data and 
# reformats it to the CDISC SDTM Questionnaires (QS) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Questionnaires data \code{\link{QS}} and 2) any supplemental 
#   Questionnaires data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getQuestionnaires(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
getQuestionnaires <- function(data_src, study_id) {
  cat("loading Questionnaires data....")
  
  qs_cols <- c("STUDYID", "DOMAIN", "USUBJID", "QSSEQ", "QSTEST", "QSCAT", 
               "QSORRES", "QSORRESU", "VISITNUM", "VISIT", "QSDY")
  
  
  sql_stmt <- paste("SELECT distinct
                    asm.study_accession,
                    \"QS\" as domain,
                    asm.subject_accession,
                    cast(0 as UNSIGNED INTEGER) as seq,
                    asm.component_name_reported,
                    asm.panel_name_reported,
                    asm.result_value_reported,
                    asm.result_unit_reported,
                    pv.order_number,
                    pv.visit_name,
                    asm.study_day                    
                    FROM  assessment asm, planned_visit pv
                    WHERE asm.study_accession in ('", study_id, "') AND 
                      (asm.assessment_type='Questionnaire' OR asm.assessment_type='Questionaire') AND
                      (asm.planned_visit_accession = pv.planned_visit_accession) 
                    ORDER BY asm.subject_accession", sep = "")
  
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    qs_df <- dbGetQuery(data_src, statement = sql_stmt)
    colnames(qs_df) <- qs_cols
    suppqs_df <- data.frame()
    if (nrow(qs_df) > 0) {
      qs_df <- transform(qs_df, QSSEQ = as.integer(QSSEQ))
      qs_dt <- as.data.table(qs_df)
      if (is.data.table(qs_dt) == TRUE) {
        qs_dt[, `:=`(QSSEQ, seq_len(.N)), by = "USUBJID"]
      }
      qs_df <- as.data.frame(qs_dt)
    }
  } else {
    l <- loadSerializedStudyData(data_src, study_id, "Questionnaires")
    qs_df <- l[[1]]
    suppqs_df <- l[[2]]
  }
  
  cat("done", "\n")
  
  qs_l <- list()
  if (nrow(qs_df) > 0)
    qs_l <- list(qs_df=qs_df, suppqs_df=suppqs_df)
  
  qs_l
}

# Get count of Questionnaires data of a specific study
# 
# The function \code{getCountOfQuestionnaires} queries the ImmPort database for count 
# of Trial Arms data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Questionnaires data 
# @examples
# \dontrun{
#   # get count of study SDY1's Questionnaires data
#   count <- getCountOfQuestionnaires(conn, "SDY1")
# }
getCountOfQuestionnaires <- function(conn, study_id) {
  sql_stmt <- paste("SELECT count(*)
                    FROM  assessment asm
                    WHERE asm.study_accession in ('", study_id, "') AND 
                      (asm.assessment_type='Questionnaire' OR asm.assessment_type='Questionaire')", sep = "")
  
  count <- dbGetQuery(conn, statement = sql_stmt)
  
  count[1, 1]
} 

##' Questionnaires Domain Variables
##' @name QS
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     QSSEQ \tab Sequence Number \cr
##'     QSTEST \tab Questionnaires Test Name \cr
##'     QSCAT \tab Category for Questionnaires \cr
##'     QSORRES \tab Results or Findings in Original Units \cr
##'     QSORRESU \tab Original Units \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     QSDY \tab Study Day of Finding
##'   }
##' }
NULL
#> NULL 

