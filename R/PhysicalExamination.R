#' Physical Examination Domain
#' 
#' @name Physical Examination Domain
#' @description The Physical Examination data of an ImmPort study is reformated to the CDISC SDTM 
#' Physical Examination (PE) domain model, and is a list of 2 data frames containing 1) Physical 
#' Examination data \code{\link{PE}} and 2) any supplemental Physical Examination data \code{\link{SUPPPE}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("PESEQ", "QNAM", "QVAL", "PETOD"))

# Get Physical Examination data of a specific study
# 
# The function \code{getPhysicalExamination} queries the ImmPort database for Physical Examination data and 
# reformats it to the CDISC SDTM Physical Examination (PE) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Physical Examination data \code{\link{PE}} and 2) any supplemental 
#   Physical Examination data \code{\link{SUPPPE}}
# @author Ravi Shankar
# @examples
# \dontrun{
#   getPhysicalExamination(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom plyr rename
#' @importFrom data.table as.data.table is.data.table .N :=
getPhysicalExamination <- function(data_src, study_id) {
    cat("loading Physical Examination data....")

    pe_cols <- c("STUDYID", "DOMAIN", "USUBJID", "PESEQ", "PETEST", "PECAT", "PEBODSYS", "PEORRES", "PEORRESU", "PELOC",
                "PETOD", "VISITNUM", "VISIT", "PEDY")
    supppe_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
  
    sql_stmt <- paste("SELECT distinct
                        asm.study_accession,
                        \"PE\" as domain,
                        asm.subject_accession,
                        cast(0 as UNSIGNED INTEGER) as seq,
                        asm.component_name_reported,
                        asm.panel_name_reported,
                        asm.organ_or_body_system_reported,
                        asm.result_value_reported,
                        asm.result_unit_reported,
                        asm.location_of_finding_reported,
                        asm.time_of_day,
                        pv.order_number,
                        pv.visit_name,
                        asm.study_day                    
                      FROM  assessment asm, planned_visit pv
                      WHERE (asm.study_accession in ('", study_id, "')) AND 
                        (asm.assessment_type='Physical Exam') AND
                        ((asm.component_name_reported!='Heart Rate') AND
                        (asm.component_name_reported!='Diastolic Blood Pressure') AND
                        (asm.component_name_reported!='Systolic Blood Pressure') AND
                        (asm.component_name_reported!='Height') AND
                        (asm.component_name_reported!='Weight') AND
                        (asm.component_name_reported!='Respiration Rate') AND
                        (asm.component_name_reported!='Temperature')) AND 
                        (asm.planned_visit_accession = pv.planned_visit_accession)
                      ORDER BY asm.subject_accession", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      pe_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(pe_df) <- pe_cols
      supppe_df <- data.frame()
      if (nrow(pe_df) > 0) {
        pe_df <- transform(pe_df, PESEQ = as.integer(PESEQ))
        pe_dt <- as.data.table(pe_df)
        if (is.data.table(pe_dt) == TRUE) {
          pe_dt[, `:=`(PESEQ, seq_len(.N)), by = "USUBJID"]
        }
        pe_df <- as.data.frame(pe_dt)
  
        qnam_values = c("PETOD")
        qlabel_values= c("Time of Day")
        
        supppe_df <- reshape2::melt(pe_df, id = c("STUDYID", "DOMAIN", "USUBJID", "PESEQ"), 
                                    measure = qnam_values, 
                                    variable.name = "QNAM", 
                                    value.name = "QVAL")
        supppe_df <- transform(supppe_df, QLABEL = unlist(qlabel_values[QNAM]))
        supppe_df <- plyr::rename(supppe_df, c("DOMAIN" = "RDOMAIN", "PESEQ" = "IDVARVAL"))
        supppe_df$IDVAR <- "PESEQ"
        
        supppe_df <- supppe_df[supppe_cols]
        
        # remove rows that have empty QVAL values
        supppe_df <- subset(supppe_df,QVAL!="")      
        
        pe_df <- subset(pe_df, select = -c(PETOD))
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Physical Examination")
      pe_df <- l[[1]]
      supppe_df <- l[[2]]
    }

    cat("done", "\n")
    
    pe_l <- list()
    if (nrow(pe_df) > 0)
      pe_l <- list(pe_df=pe_df, supppe_df=supppe_df)
    
    pe_l
}

# Get count of Physical Examination data of a specific study
# 
# The function \code{getCountOfPhysicalExamination} queries the ImmPort database for count 
# of Physical Examination data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Physical Examination data 
# @examples
# \dontrun{
#   # get count of study SDY1's Physical Examination data
#   count <- getCountOfPhysicalExamination(conn, "SDY1")
# }
getCountOfPhysicalExamination <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM  assessment asm
                      WHERE (asm.study_accession in ('", study_id, "')) AND 
                        (asm.assessment_type='Physical Exam') AND
                        ((asm.component_name_reported!='Heart Rate') AND
                         (asm.component_name_reported!='Diastolic Blood Pressure') AND
                         (asm.component_name_reported!='Systolic Blood Pressure') AND
                         (asm.component_name_reported!='Height') AND
                         (asm.component_name_reported!='Weight') AND
                         (asm.component_name_reported!='Respiration Rate') AND
                         (asm.component_name_reported!='Temperature'))", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Physical Examination Domain Variables
##' @name PE
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     PESEQ \tab Sequence Number \cr
##'     PETEST \tab Body System Examined \cr
##'     PECAT \tab Category for Examination \cr
##'     PEBODSYS \tab Body System or Organ Class \cr
##'     PEORRES \tab Verbatim Examination Finding \cr
##'     PEORRESU \tab Original Units \cr
##'     PELOC \tab Location of Physical Exam Finding \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     PEDY \tab Study Day of Examination
##'   }
##' }
NULL
#> NULL 

##' Physical Examination Domain Supplemental Variables
##' @name SUPPPE
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name} \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     RDOMAIN  \tab Related Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     PESEQ \tab Sequence Number \cr
##'     IDVAR \tab Identifying Variable \cr
##'     IDVARVAL \tab Identifying Variable Value \cr
##'     QNAM \tab Qualifier Variable Name \cr
##'     QLABEL \tab Qualifier Variable Label \cr
##'     QVAL \tab Data Value
##'   }
##' }
##' @note The following table enumerates the values in QNAM and QLABEL variables {
##'   \tabular{ll}{
##'     \strong{QNAM} \tab \strong{QLABEL} \cr
##'     PETOD \tab Time of Day \cr
##'   }
##' }
NULL
#> NULL
