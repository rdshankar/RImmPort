#' Vital Signs Domain
#' 
#' @name Vital Signs Domain
#' @description The Vital Signs data of an ImmPort study is reformated to the CDISC SDTM Vital Signs (VS) 
#' domain model, and is a list of 2 data frames containing 1) Vital Signs data \code{\link{VS}} 
#' and 2) any supplemental Vital Signs data \code{\link{SUPPVS}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("VSSEQ", "QNAM", "QVAL", "VSTOD"))

# Get Vital Signs data of a specific study
# 
# The function \code{getVitalSigns} queries the ImmPort database for Vital Signs data and 
# reformats it to the CDISC SDTM Vital Signs (VS) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Vital Signs data \code{\link{VS}} and 2) any supplemental 
#   Vital Signs data \code{\link{SUPPVS}}
# @examples
# \dontrun{
#   getVitalSigns(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
getVitalSigns <- function(data_src, study_id) {
    cat("loading Vital Signs data....")

    vs_cols <- c("STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VSTEST", "VSCAT", "VSORRES", "VSORRESU", "VSLOC", "VSTOD", "VISITNUM", "VISIT", "VSDY")
  
    suppvs_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
    sql_stmt <- paste("SELECT distinct
                        asm.study_accession,
                        \"VS\" as domain,
                        asm.subject_accession,
                        cast(0 as UNSIGNED INTEGER) as seq,
                        asm.component_name_reported,
                        asm.panel_name_reported,
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
                        ((asm.component_name_reported='Heart Rate') OR 
                         (asm.component_name_reported='Diastolic Blood Pressure') OR
                         (asm.component_name_reported='Systolic Blood Pressure') OR
                         (asm.component_name_reported='Height') OR
                         (asm.component_name_reported='Weight') OR
                         (asm.component_name_reported='Respiration Rate') OR
                         (asm.component_name_reported='Temperature')) AND
                        (asm.planned_visit_accession = pv.planned_visit_accession)
                      ORDER BY asm.subject_accession", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      vs_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(vs_df) <- vs_cols
      suppvs_df <- data.frame()
      
      if (nrow(vs_df) > 0) {
        vs_df <- transform(vs_df, VSSEQ = as.integer(VSSEQ))
        vs_dt <- as.data.table(vs_df)
        if (is.data.table(vs_dt) == TRUE) {
          vs_dt[, `:=`(VSSEQ, seq_len(.N)), by = "USUBJID"]
        }
        vs_df <- as.data.frame(vs_dt)
        
        qnam_values = c("VSTOD")
        qlabel_values= c("Time of Day")
        
        suppvs_df <- reshape2::melt(vs_df, id = c("STUDYID", "DOMAIN", "USUBJID", "VSSEQ"), 
                                    measure = qnam_values, 
                                    variable.name = "QNAM", 
                                    value.name = "QVAL")
        suppvs_df <- transform(suppvs_df, QLABEL = unlist(qlabel_values[QNAM]))
        suppvs_df <- plyr::rename(suppvs_df, c("DOMAIN" = "RDOMAIN", "VSSEQ" = "IDVARVAL"))
        suppvs_df$IDVAR <- "VSSEQ"
        
        suppvs_df <- suppvs_df[suppvs_cols]
        
        # remove rows that have empty QVAL values
        suppvs_df <- subset(suppvs_df,QVAL!="")      
        
        vs_df <- subset(vs_df, select = -c(VSTOD))
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Vital Signs")
      vs_df <- l[[1]]
      suppvs_df <- l[[2]]
    }
    
    cat("done", "\n")
    
    vs_l <- list()
    if (nrow(vs_df) > 0)
      vs_l <- list(vs_df=vs_df, suppvs_df=suppvs_df)
    
    vs_l
}

# Get count of Vital Signs data of a specific study
# 
# The function \code{getCountOfVitalSigns} queries the ImmPort database for count 
# of Vital Signs data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Vital Signs data 
# @examples
# \dontrun{
#   # get count of study SDY1's Vital Signs data
#   count <- getCountOfVitalSigns(conn, "SDY1")
# }
getCountOfVitalSigns <- function(conn, study_id) {
  sql_stmt <- paste("SELECT count(*)
                      FROM  assessment asm
                      WHERE (asm.study_accession in ('", study_id, "')) AND 
                        (asm.assessment_type='Physical Exam') AND
                        ((asm.component_name_reported='Heart Rate') OR 
                        (asm.component_name_reported='Diastolic Blood Pressure') OR
                        (asm.component_name_reported='Systolic Blood Pressure') OR
                        (asm.component_name_reported='Height') OR
                        (asm.component_name_reported='Weight') OR
                        (asm.component_name_reported='Respiration Rate') OR
                        (asm.component_name_reported='Temperature'))", sep = "")
                    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Vital Signs Domain Variables
##' @name VS
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     VSSEQ \tab Sequence Number \cr
##'     VSTEST \tab Vital Signs Test Name \cr
##'     VSCAT \tab Category for Vital Signs \cr
##'     VSORRES \tab Result or Finding in Original Units \cr
##'     VSORRESU \tab Original Units \cr
##'     VSLOC \tab Location of Vital Signs Measurement \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     VSDY \tab Study Day of Vital Signs
##'   }
##' }
NULL
#> NULL 

##' Vital Signs Domain Supplemental Variables
##' @name SUPPVS
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name} \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     RDOMAIN  \tab Related Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     VSSEQ \tab Sequence Number \cr
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
##'     VSTOD \tab Time of Day \cr
##'   }
##' }
NULL
#> NULL

