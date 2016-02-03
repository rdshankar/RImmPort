#' Laboratory Test Results Domain
#' 
#' @name Laboratory Test Results Domain
#' @description The Laboratory Test Results data of an ImmPort study is reformated to the CDISC SDTM 
#' Laboratory Test Results (LB) domain model, and is a list of 2 data frames containing 1) Laboratory 
#' Test Results data \code{\link{LB}} and 2) any supplemental Laboratory Test Results data \code{\link{SUPPLB}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("LBSEQ", "QNAM", "QVAL", "LBSPECSB", "VISITMIN", "VISITMAX", "STUDYID", "DOMAIN", "USUBJID",
                  "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))

# Get Laboratory Test Results data of a specific study
# 
# The function \code{Laboratory Test Results} queries the ImmPort database for Laboratory Test Results data and 
# reformats it to the CDISC SDTM Laboratory Test Results (DM) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Laboratory Test Results data \code{\link{LB}} and 2) any supplemental 
#   Laboratory Test Results data \code{\link{SUPPLB}}
# @examples
# \dontrun{
#   getLaboratoryTestResults(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
#' @importFrom plyr rename
#' @importFrom plyr ddply
getLaboratoryTestResults <- function(data_src, study_id) {
    cat("loading Laboratory Test Results data....")
  
    lb.cols <- c("STUDYID", "DOMAIN", "USUBJID", "LBSEQ", "LBTEST", "LBCAT", "LBORRES", "LBORRESU", "LBSPEC", "LBSPECSB",
                "VISITNUM", "VISIT", "VISITMIN", "VISITMAX", "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                "study_time_t0_event", "study_time_t0_event_specify", "LBREFID")
  
    supplb.cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
  
    
    sql_stmt <- paste("SELECT distinct
                        lt.study_accession,
                        \"LB\" as domain,
                        bs.subject_accession,
                        cast(0 as UNSIGNED INTEGER) as seq,
                        lt.name_reported,
                        lt.panel_name_reported,
                        lt.result_value_reported,
                        lt.result_unit_reported,
                        bs.type,
                        bs.subtype,
                        pv.order_number,
                        pv.visit_name,
                        pv.min_start_day,
                        pv.max_start_day,
                        bs.study_time_collected,
                        bs.study_time_collected_unit,
                        bs.study_time_t0_event,
                        bs.study_time_t0_event_specify,
                        bs.biosample_accession
                      FROM  lab_test lt,
                            biosample bs,                         
                            planned_visit pv
                      WHERE lt.study_accession in ('", study_id, "') AND
                            lt.biosample_accession=bs.biosample_accession AND
                            bs.planned_visit_accession=pv.planned_visit_accession
                      ORDER BY bs.subject_accession", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      lb_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(lb_df) <- lb.cols
      supplb_df <- data.frame()
      
      if (nrow(lb_df) > 0) {
          lb_df <- transform(lb_df, LBSEQ = as.integer(LBSEQ))
          lb_dt <- as.data.table(lb_df)
          if (is.data.table(lb_dt) == TRUE) {
            lb_dt[, `:=`(LBSEQ, seq_len(.N)), by = "USUBJID"]
          }
          lb_df <- as.data.frame(lb_dt)

          lb_df <- ddply(lb_df, .(STUDYID, DOMAIN, USUBJID, LBSEQ), mutate, LBELTM = 
                            covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
                                                              unit_of_study_time_of_specimen_collection))
          
          lb_df <- ddply(lb_df, .(STUDYID, DOMAIN, USUBJID, LBSEQ), mutate, LBTPTREF = 
                            getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
          
   
          
          qnam_values = c("LBSPECSB", "VISITMIN", "VISITMAX")
          qlabel_values= c("Specimen Subtype", "Planned Visit Minimum Start Day", "Planned Visit Maximum Start Day")
          
          supplb_df <- reshape2::melt(lb_df, 
                                      id = c("STUDYID", "DOMAIN", "USUBJID", "LBSEQ"), 
                                      measure = qnam_values, 
                                      variable.name = "QNAM", 
                                      value.name = "QVAL")
          supplb_df <- transform(supplb_df, QLABEL = unlist(qlabel_values[QNAM]))
          supplb_df <- plyr::rename(supplb_df, c("DOMAIN" = "RDOMAIN", "LBSEQ" = "IDVARVAL"))
          supplb_df$IDVAR <- "LBSEQ"
          
          
          supplb_df <- supplb_df[supplb.cols]
          
          # remove rows that have empty QVAL values
          supplb_df <- subset(supplb_df,QVAL!="")      
  
          lb_df <- subset(lb_df, select = -c(LBSPECSB, VISITMIN, VISITMAX, study_time_of_specimen_collection, 
                                             unit_of_study_time_of_specimen_collection,
                                             study_time_t0_event, study_time_t0_event_specify))
          
  #         lb_dt[, `:=`(LBDTCU, NULL)]
  #         lb_dt[, `:=`(LBTPT0, NULL)]
  #         lb_dt[, `:=`(LBTPT0SP, NULL)]
  #         lb_dt[, `:=`(LBSPECID, NULL)]
  #         lb_df <- as.data.frame(lb_dt)
  #         supplb_df <- as.data.frame(slb_dt)
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Laboratory Test Results")
      lb_df <- l[[1]]
      supplb_df <- l[[2]]
    }

    
    cat("done", "\n")
    
    lb_l <- list()
    if (nrow(lb_df) > 0)
      lb_l <- list(lb_df=lb_df, supplb_df=supplb_df)
    
    lb_l
}

# Get count of Laboratory Test Results data of a specific study
# 
# The function \code{getCountOfLaboratoryTestResults} queries the ImmPort database for count 
# of Laboratory Test Results data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Laboratory Test Results data 
# @examples
# \dontrun{
#   # get count of study SDY1's Laboratory Test Results data
#   count <- getCountOfLaboratoryTestResults(conn, "SDY1")
# }
getCountOfLaboratoryTestResults <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM  lab_test lt,
                            biosample bs
                      WHERE lt.study_accession in ('", study_id, "') AND 
                            lt.biosample_accession=bs.biosample_accession", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Laboratory Test Results Domain Variables
##' @name LB
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     LBSEQ \tab Sequence Number \cr
##'     LBTEST \tab Lab Test or Examination Name \cr
##'     LBCAT \tab Category for Lab Test \cr
##'     LBORRES \tab Result or Finding in Original Units \cr
##'     LBORRESU \tab Original Units \cr
##'     LBSPEC \tab Specimen Type \cr
##'     LBREFID \tab Specimen Identifier \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     LBELTM \tab Planned Elapsed Time from Time Point Ref \cr
##'     LBTPTREF \tab Time Point Reference
##'   }
##' }
NULL
#> NULL

##' Laboratory Test Results Domain Supplemental Variables
##' @name SUPPLB
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name} \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     RDOMAIN  \tab Related Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
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
##'     LBSPECSB \tab Specimen Subtype \cr
##'     VISITMIN \tab Planned Visit Minimum Start Day \cr
##'     VISITMAX \tab Planned Visit Maximum Start Day
##'   }
##' }
NULL
#> NULL
