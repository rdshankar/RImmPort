#' Medical History Domain
#' 
#' @name Medical History Domain
#' @description The Medical History data of an ImmPort study is reformated to the CDISC SDTM Medical 
#' History (MH) domain model, and is a list of 2 data frames containing 1) Medical History 
#' data \code{\link{MH}} and 2) any supplemental Medical History data \code{\link{SUPPMH}}
NULL
#> NULL 



# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("MHSEQ", "QNAM", "QVAL", "MHAGE", "MHAGEU", "MHTOD"))

# Get Medical History data of a specific study
# 
# The function \code{getMedicalHistory} queries the ImmPort database for Medical History data and 
# reformats it to the CDISC SDTM Medical History (MH) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Medical History data \code{\link{MH}} and 2) any supplemental 
#   Medical History data \code{\link{SUPPMH}}
# @examples
# \dontrun{
#   getMedicalHistory(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom plyr rename
#' @importFrom data.table as.data.table is.data.table .N :=
getMedicalHistory <- function(data_src, study_id) {
    cat("loading Medical History data....")

    mh_cols <- c("STUDYID", "DOMAIN", "USUBJID","MHSEQ", "MHTERM", "MHCAT", "MHBODSYS", "MHAGE", "MHAGEU", 
               "MHTOD", "MHDY")
  
    suppmh_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
    # Note these cols should go to suppmh: MHAGE, MHAGEU and MHTOD
      
    sql_stmt <- paste("SELECT distinct
                        asm.study_accession,
                        \"MH\" as domain,
                        asm.subject_accession,
                        cast(0 as UNSIGNED INTEGER) as seq,
                        asm.component_name_reported,
                        asm.panel_name_reported,
                        asm.organ_or_body_system_reported,
                        asm.age_at_onset_reported,
                        asm.age_at_onset_reported_unit,
                        asm.time_of_day,
                        asm.study_day                    
                      FROM  assessment asm
                      WHERE asm.study_accession in ('", study_id, "') AND 
                        asm.assessment_type='Medical History'
                      ORDER BY asm.subject_accession", sep = "")
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      mh_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(mh_df) <- mh_cols
      suppmh_df <- data.frame()
      if (nrow(mh_df) > 0) {
        mh_df <- transform(mh_df, MHSEQ = as.integer(MHSEQ))
        mh_dt <- as.data.table(mh_df)
        if (is.data.table(mh_dt) == TRUE) {
          mh_dt[, `:=`(MHSEQ, seq_len(.N)), by = "USUBJID"]
        }
        mh_df <- as.data.frame(mh_dt)
  
        qnam_values = c("MHAGE", "MHAGEU", "MHTOD")
        qlabel_values= c("Age at Onset", "Age at Onset Units", "Time of Day")
      
        suppmh_df <- reshape2::melt(mh_df, id = c("STUDYID", "DOMAIN", "USUBJID", "MHSEQ"), 
                                    measure = qnam_values, 
                                    variable.name = "QNAM", 
                                    value.name = "QVAL")
        suppmh_df <- transform(suppmh_df, QLABEL = unlist(qlabel_values[QNAM]))
        suppmh_df <- plyr::rename(suppmh_df, c("DOMAIN" = "RDOMAIN", "MHSEQ" = "IDVARVAL"))
        suppmh_df$IDVAR <- "MHSEQ"
      
        suppmh_df <- suppmh_df[suppmh_cols]
        
        # remove rows that have empty QVAL values
        suppmh_df <- subset(suppmh_df,QVAL!="")      
        
        mh_df <- subset(mh_df, select = -c(MHAGE, MHAGEU, MHTOD))
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Medical History")
      mh_df <- l[[1]]
      suppmh_df <- l[[2]]
    }
    
    cat("done", "\n")
    
    mh_l <- list()
    if (nrow(mh_df) > 0)
      mh_l <- list(mh_df=mh_df, suppmh_df=suppmh_df)
    
    mh_l
}

# Get count of Medical History data of a specific study
# 
# The function \code{getCountOfMedicalHistory} queries the ImmPort database for count 
# of Medical History data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Medical History data 
# @examples
# \dontrun{
#   # get count of study SDY1's Medical History data
#   count <- getCountOfMedicalHistory(conn, "SDY1")
# }
getCountOfMedicalHistory <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM  assessment asm
                      WHERE asm.study_accession in ('", study_id, "') AND 
                        asm.assessment_type='Medical History'", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Medical History Domain Variables
##' @name MH
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     MHSEQ \tab Sequence Number \cr
##'     MHTERM \tab Reported Term for the Medical History \cr
##'     MHCAT \tab Category for Medical History \cr
##'     MHBODSYS \tab Body System or Organ Class \cr
##'     MHDY \tab Study Day of History Collection
##'   }
##' }
NULL
#> NULL 

##' Medical History Domain Supplemental Variables
##' @name SUPPMH
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name} \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     RDOMAIN  \tab Related Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     MHSEQ \tab Sequence Number \cr
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
##'     MHAGE \tab Age at Onset \cr
##'     MHAGEU \tab Age at Onset Units \cr
##'     MHTOD \tab Time of Day
##'   }
##' }
NULL
#> NULL
