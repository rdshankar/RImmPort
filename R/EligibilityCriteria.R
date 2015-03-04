
elig_column_names <- c("study_id", "criterion_category", "criterion")

getEligibilityCriteria <- function(conn,study_id) {
  cat("loading Study Eligibility Criteria data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    ie.study_accession,
                    ie.criterion_category,
                    ie.criterion
                    FROM inclusion_exclusion ie
                    WHERE ie.study_accession in (\'", study_id,"\')
                    ORDER BY ie.criterion_accession", sep="")
  
  eligibility_criteria <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(eligibility_criteria) > 0)
    colnames(eligibility_criteria) <- elig_column_names 
  cat("done", "\n")
  
  eligibility_criteria
}