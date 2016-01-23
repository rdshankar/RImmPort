#' Trial Arms Domain
#' 
#' @name Trial Arms Domain
#' @description The Trial Arms data of an ImmPort study is reformated to the CDISC SDTM Trial Arms (TA) 
#' domain model, and is a list of 2 data frames containing 1) Trial Arms data \code{\link{TA}} 
#' and 2) any supplemental Trial Arms data \code{\link{SUPP}}
NULL
#> NULL 

# Get Trial Arms data of a specific study
# 
# The function \code{getTrialArms} queries the ImmPort database for Trial Arms data and 
# reformats it to the CDISC SDTM Trial Arms (TA) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Trial Arms data \code{\link{TA}} and 2) any supplemental 
#   Trial Arms data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getTrialArms(data_src, "SDY1")
# }
getTrialArms <- function(data_src, study_id) {
    cat("loading TrialArms data....")

    ta_cols <- c("STUDYID", "DOMAIN", "ARMCD", "ARM", "ARMDESC", "ARMRULE")
  
    sql_stmt <- paste("SELECT distinct
                        ac.study_accession,
                        \"TA\" as domain,
                        ac.arm_accession,
                        ac.name as arm_name,
                        ac.description as arm_description,
                        ac.population_selection_rule as population_selection_rule
                      FROM arm_or_cohort ac
                      WHERE ac.study_accession in ('", study_id, "')
                      ORDER BY ac.sort_order", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      ta_df <- dbGetQuery(data_src, statement = sql_stmt)
      suppta_df <- data.frame()
      colnames(ta_df) <- ta_cols
      if (nrow(ta_df) > 0) {
        
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Trial Arms")
      ta_df <- l[[1]]
      suppta_df <- l[[2]]
    }
    
    cat("done", "\n")
    
    ta_l <- list()
    if (nrow(ta_df) > 0)
      ta_l <- list(ta_df=ta_df, suppta_df=suppta_df)
    
    ta_l
    
    
    
}

# Get count of Trial Arms data of a specific study
# 
# The function \code{getCountOfTrialArms} queries the ImmPort database for count 
# of Trial Arms data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Trial Arms data 
# @examples
# \dontrun{
#   # get count of study SDY1's Trial Arms data
#   count <- getCountOfTrialArms(conn, "SDY1")
# }
getCountOfTrialArms <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM arm_or_cohort ac
                      WHERE ac.study_accession in ('", study_id, "')", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 

##' Trial Arms Domain Variables
##' @name TA
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     ARMCD \tab Planned Arm Code \cr
##'     ARM \tab Name of Planned Arm \cr
##'     ARMDESC \tab Description of Planned Arm \cr
##'     ARMRULE \tab Population Selection Rule
##'   }
##' }
NULL
#> NULL
