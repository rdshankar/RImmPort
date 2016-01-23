#' Trial Summary Domain
#' 
#' @name Trial Summary Domain
#' @description The Trial Summary data of an ImmPort study is reformated to the CDISC SDTM Trial Summary (TS) 
#' domain model, and is a list of 2 data frames containing 1) Trial Summary data \code{\link{TS}} 
#' and 2) any supplemental Trial Summary data \code{\link{SUPP}}
NULL
#> NULL 


# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("TSPARMCD"))

# Get Trial Summary data of a specific study
# 
# The function \code{getTrialSummary} queries the ImmPort database for Trial Summary data and 
# reformats it to the CDISC SDTM getTrialSummary (TS) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Trial Summary data \code{\link{TS}} and 2) any supplemental 
#   Trial Summary data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getTrialSummary(data_src, "SDY1")
# }
getTrialSummary <- function(data_src,study_id) {
  cat("loading TrialSummary data....", study_id, " ")

  ts_cols  <- c("STUDYID", "DOMAIN","TSSEQ",
                "TSPARMCD", "TSPARM", "TSVAL")
  
  ts_col_codes <- c("STUDYID", "DOMAIN", "TSSEQ", "TITLE", "DESCR", "INDIC", 
                    "TRT", "HYPOTHS", "SSTDTC", 
                    "SENDTC", "PLANSUB", "ACTSUB", 
                    "AGEMAX", "AGEMIN", "AGEU", 
                    "SEXPOP", "SPONSOR", "PUBRLDAT", "STYPE", 
                    "ISTRIAL","RESFOCUS")
  
  ts_col_labels <- c("Study Identifier", "Domain", "Sequence", "Trial Title", "Trial Description", "Trial Indication", 
                     "Investigational Therapy or Treatment", "Trial Hypotheses", "Study Start Date", 
                     "Study End Date", "Planned Number of Subjects", "Actual Number of Subjects", 
                     "Planned Maximum Age of Subjects", "Planned Minimum Age of Subjects", "Age Units", 
                     "Sex of Participants", "Clinical Study Sponsor", "Public Release Date", "Study Type", 
                     "Is Clinical Trial?", "Trial Research Focus")
  code_label.l <- as.list(ts_col_labels)
  names(code_label.l) <- ts_col_codes
  
  sql_stmt <- paste("
                    SELECT distinct
                    st.study_accession,
                    \"TS\" as domain,
                    cast(1 as UNSIGNED INTEGER) as seq,
                    st.brief_title,
                    st.brief_description,
                    st.condition_studied,
                    st.intervention_agent,
                    st.hypothesis,
                    st.actual_start_date,
                    st.actual_completion_date,
                    st.target_enrollment,
                    st.actual_enrollment,
                    st.maximum_age,
                    st.minimum_age,
                    st.age_unit,
                    st.gender_included,
                    st.sponsoring_organization,
                    st.final_public_release_date,
                    st.type,
                    st.clinical_trial,
                    \"\"                 
                    FROM study st, study_categorization sc
                    WHERE st.study_accession = sc.study_accession AND
                          st.study_accession in (\'", study_id,"\')", sep="")
  
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    temp_ts_df <- dbGetQuery(data_src,statement=sql_stmt)
    ts_df <- data.frame()
    suppts_df <- data.frame()
    
    if (nrow(temp_ts_df) > 0) {
      colnames(temp_ts_df) <- ts_col_codes   
    
      sql_stmt <- paste("
                      SELECT distinct
                      sc.research_focus
                      FROM  study_categorization sc
                      WHERE sc.study_accession in (\'", study_id,"\')", sep="")
    
      rf <- dbGetQuery(data_src,statement=sql_stmt)
      if (nrow(rf) > 0) {
        temp_ts_df$RESFOCUS <- paste(rf[,1], collapse = "|")
      }
  
      ts_df <- reshape2::melt(temp_ts_df, id = c("STUDYID", "DOMAIN", "TSSEQ"), 
                            measure = c("TITLE", "DESCR", "INDIC", 
                                        "TRT", "HYPOTHS", "SSTDTC", 
                                        "SENDTC", "PLANSUB", "ACTSUB", 
                                        "AGEMAX", "AGEMIN", "AGEU", 
                                        "SEXPOP", "SPONSOR", "PUBRLDAT", "STYPE", 
                                        "ISTRIAL","RESFOCUS"), 
                            variable.name = "TSPARMCD", 
                            value.name = "TSVAL")
    
      code_label.l <-  code_label.l[-c(1, 2, 3)]
      ts_df <- transform(ts_df, TSPARM = unlist(code_label.l[TSPARMCD]))
      row.names(ts_df) <- NULL
    
      #ts_df$TSPARM <- code_label.l[[ts_df$TSPARMCD]]
    
      ts_df <- ts_df[ts_cols]
    }
  } else {
    l <- loadSerializedStudyData(data_src, study_id, "Trial Summary")
    ts_df <- l[[1]]
    suppts_df <- l[[2]]
  }
  
  cat("done", "\n")
  
  ts_l <- list()
  if (nrow(ts_df) > 0)
    ts_l <- list(ts_df=ts_df, suppts_df=suppts_df)
  
  ts_l
}

# Get count of Trial Summary data of a specific study
# 
# The function \code{getCountOfTrialSummary} queries the ImmPort database for count 
# of Trial Summary data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Trial Summary data 
# @examples
# \dontrun{
#   # get count of study SDY1's Trial Summary data
#   count <- getCountOfTrialSummary(conn, "SDY1")
# }
getCountOfTrialSummary <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM study st
                    WHERE st.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}

##' Trial Summary Domain Variables
##' @name TS
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name} \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     TSSEQ \tab Sequence Number \cr
##'     TSPARMCD \tab Trial Summary Parameter Short Name \cr
##'     TSPARM \tab Trial Summary Parameter \cr
##'     TSVAL \tab Parameter Value
##'   }
##' }
##' @note The following table enumerates the values in TSPARMCD and TSPARM variables {
##'   \tabular{ll}{
##'     \strong{TSPARMCD} \tab \strong{TSPARM} \cr
##'     TITLE \tab Trial Title \cr
##'     DESCR \tab Trial Description \cr
##'     INDIC \tab Trial Indication \cr
##'     TRT \tab Investigational Therapy or Treatment \cr
##'     HYPOTHS \tab Trial Hypotheses \cr
##'     SSTDTC \tab Study Start Date \cr
##'     SENDTC \tab Study End Date \cr
##'     PLANSUB \tab Planned Number of Subjects \cr
##'     ACTSUB \tab Actual Number of Subjects \cr
##'     AGEMAX \tab Planned Maximum Age of Subjects \cr
##'     AGEMIN \tab Planned Minimum Age of Subjects \cr
##'     AGEU \tab Age Units \cr
##'     SEXPOP \tab Sex of Participants \cr
##'     SPONSOR \tab Clinical Study Sponsor \cr
##'     PUBRLDAT \tab Public Release Date \cr
##'     ISTRIAL \tab Study Type \cr
##'     RESFOCUS \tab Trial Research Focus 
##'   }
##' }
NULL
#> NULL

