#' Protocol Deviations Domain
#' 
#' @name Protocol Deviations Domain
#' @description The Protocol Deviations data of an ImmPort study is reformated to the CDISC SDTM 
#' Protocol Deviations (DV) domain model, and is a list of 2 data frames containing 1) Protocol 
#' Deviations data \code{\link{DV}} and 2) any supplemental Protocol Deviations data \code{\link{SUPPDV}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("DVSEQ", "QNAM", "QVAL", "DVRELAE", "DVREASON", "DVRESOL", 
                  "DVCONT", "DVSTDY", "DVENDY"))

# Get Protocol Deviations data of a specific study
# 
# The function \code{getProtocolDeviations} queries the ImmPort database for Protocol Deviations data and 
# reformats it to the CDISC SDTM Protocol Deviations (DV) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Protocol Deviations data \code{\link{DV}} and 2) any supplemental 
#   Protocol Deviations data \code{\link{SUPPDV}}
# @examples
# \dontrun{
#   getProtocolDeviations(data_src, "SDY1")
# }
#' @importFrom DBI dbGetQuery
#' @importFrom data.table as.data.table is.data.table .N :=
getProtocolDeviations <- function(data_src, study_id) {
    cat("loading Protocol Deviations data....")
    
    dv_cols <- c("STUDYID", "DOMAIN", "USUBJID", "DVSEQ", "DVTERM", "DVRELAE", "DVREASON", "DVRESOL", 
               "DVCONT", "DVSTDY", "DVENDY")
  
    suppdv_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
  
    sql_stmt <- paste("SELECT distinct
                        pd.study_accession,
                        \"DV\" as domain,
                        pd.subject_accession,
                        cast(0 as UNSIGNED INTEGER) as seq,
                        pd.deviation_description,
                        pd.is_adverse_event_related,
                        pd.reason_for_deviation,
                        pd.resolution_of_deviation,
                        pd.subject_continued_study,
                        pd.deviation_study_start_day,
                        pd.deviation_study_end_day
                      FROM  protocol_deviation pd
                      WHERE pd.study_accession in ('", study_id, "')
                      ORDER BY pd.subject_accession", sep = "")

    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      dv_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(dv_df) <- dv_cols
      suppdv_df <- data.frame()
      if (nrow(dv_df) > 0) {
        dv_df <- transform(dv_df, DVSEQ = as.integer(DVSEQ))
        dv_dt <- as.data.table(dv_df)
        if (is.data.table(dv_dt) == TRUE) {
          dv_dt[, `:=`(DVSEQ, seq_len(.N)), by = "USUBJID"]
        }
        dv_df <- as.data.frame(dv_dt)
      
      
      
        qnam_values = c("DVRELAE", "DVREASON", "DVRESOL", "DVCONT", "DVSTDY", "DVENDY")
  
        qlabel_values= c("Is Deviation Related to an Adverse Event?", "Reason for Deviation", 
                         "Resulotion of Deviation", "Did Subject continued in Study?",
                         "Study Day of Start of Deviation", "Study Day of End of Deviation")
      
        suppdv_df <- reshape2::melt(dv_df, id = c("STUDYID", "DOMAIN", "USUBJID", "DVSEQ"), 
                                    measure = qnam_values, 
                                    variable.name = "QNAM", 
                                    value.name = "QVAL")
        suppdv_df <- transform(suppdv_df, QLABEL = unlist(qlabel_values[QNAM]))
        suppdv_df <- plyr::rename(suppdv_df, c("DOMAIN" = "RDOMAIN", "DVSEQ" = "IDVARVAL"))
        suppdv_df$IDVAR <- "DVSEQ"
      
        suppdv_df <- suppdv_df[suppdv_cols]
        
        # remove rows that have empty QVAL values
        suppdv_df <- subset(suppdv_df,QVAL!="")      
        
        dv_df <- subset(dv_df, select = -c(DVRELAE, DVREASON, DVRESOL, DVCONT, DVSTDY, DVENDY))
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Protocol Deviations")
      dv_df <- l[[1]]
      suppdv_df <- l[[2]]
    }
    cat("done", "\n")

    dv_l <- list()
    if (nrow(dv_df) > 0)
      dv_l <- list(dv_df=dv_df, suppdv_df=suppdv_df)
    
    dv_l
}

# Get count of Protocol Deviations data of a specific study
# 
# The function \code{getCountOfProtocolDeviations} queries the ImmPort database for count 
# of Protocol Deviations data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Protocol Deviations data 
# @examples
# \dontrun{
#   # get count of study SDY1's Protocol Deviations data
#   count <- getCountOfProtocolDeviations(conn, "SDY1")
# }
getCountOfProtocolDeviations <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM  protocol_deviation pd
                      WHERE pd.study_accession in ('", study_id, "')", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Protocol Deviations Domain Variables
##' @name DV
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     DVSEQ \tab Sequence Number \cr
##'     DVTERM \tab Protocol Deviation Term
##'   }
##' }
NULL
#> NULL 

##' Protocol Deviations Domain Supplemental Variables
##' @name SUPPDV
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
##'     DVRELAE \tab Is Deviation Related to an Adverse Event? \cr
##'     DVREASON \tab Reason for Deviation \cr
##'     DVRESOL \tab Resulotion of Deviation \cr
##'     DVCONT \tab Did Subject continued in Study? \cr
##'     DVSTDY \tab Study Day of Start of Deviation \cr
##'     DVENDY \tab Study Day of End of Deviation
##'   }
##' }
NULL
#> NULL
