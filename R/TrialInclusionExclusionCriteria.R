#' Trial Inclusion Exclusion Criteria Domain
#' 
#' @name Trial Inclusion Exclusion Criteria Domain
#' @description The Trial Inclusion Exclusion Criteria data of an ImmPort study is reformated to the 
#' CDISC SDTM Trial Inclusion Exclusion Criteria (TI) domain model, and is a list of 2 data frames 
#' containing 1) Trial Inclusion Exclusion Criteria data \code{\link{TI}} 
#' and 2) any supplemental Trial Inclusion Exclusion Criteria data \code{\link{SUPP}}
NULL
#> NULL 

# Get Trial Inclusion Exclusion Criteria data of a specific study
# 
# The function \code{getTrialInclusionExclusionCriteria} queries the ImmPort database for Trial Inclusion Exclusion Criteria  data and 
# reformats it to the CDISC SDTM Trial Inclusion Exclusion Criteria (TI) domain model 
# 
# @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Trial Inclusion Exclusion Criteria data \code{\link{TI}} and 2) any supplemental 
#   Trial Inclusion Exclusion Criteria data \code{\link{SUPP}}
# @examples
# \dontrun{
#   getTrialInclusionExclusionCriteria(data_src, "SDY1")
# }
getTrialInclusionExclusionCriteria <- function(data_src, study_id) {
    cat("loading  TrialInclusionExclusionCriteria data....")
    
    ti_cols <- c("STUDYID", "DOMAIN", "IETEST", "IECAT")
  
    sql_stmt <- paste("SELECT distinct
                         ie.study_accession,
                         \"TI\" as domain,
                         ie.criterion,
                         ie.criterion_category
                       FROM inclusion_exclusion ie
                       WHERE ie.study_accession in ('", study_id, "')
                       ORDER BY ie.criterion_accession", sep = "")
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      ti_df <- dbGetQuery(data_src, statement = sql_stmt)
      colnames(ti_df) <- ti_cols
      suppti_df <- data.frame()
      if (nrow(ti_df) > 0) {
      
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Trial Inclusion Exclusion Criteria")
      ti_df <- l[[1]]
      suppti_df <- l[[2]]
    }

    cat("done", "\n")
    
    ti_l <- list()
    if (nrow(ti_df) > 0)
      ti_l <- list(ti_df=ti_df, suppti_df=suppti_df)
    
    ti_l
}

# Get count of Trial Inclusion Exclusion Criteria data of a specific study
# 
# The function \code{getCountOfTrialInclusionExclusionCriteria} queries the ImmPort database for count 
# of Trial Inclusion Exclusion Criteria data 
# 
# @param conn A connection handle to ImmPort database instance
# @param study_id Identifier of a specific study
# @return a count of Trial Inclusion Exclusion Criteria data 
# @examples
# \dontrun{
#   # get count of study SDY1's Trial Inclusion Exclusion Criteria data
#   count <- getCountOfTrialInclusionExclusionCriteria(conn, "SDY1")
# }
getCountOfTrialInclusionExclusionCriteria <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM inclusion_exclusion ie
                      WHERE ie.study_accession in ('", study_id, "')", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count
} 

##' Trial Inclusion Exclusion Criteria Domain Variables
##' @name TI
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     IETEST \tab Inclusion/Exclusion Criterion \cr
##'     IECAT \tab Inclusion/Exclusion Category
##'   }
##' }
NULL
#> NULL 
