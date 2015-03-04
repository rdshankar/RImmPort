
arms_column_names <- c("study_id", "arm_code", "arm", "sort_order")

getArms <- function(conn,study_id) {
  cat("loading Study Arms data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    a1.study_accession,
                    a1.arm_accession,
                    a1.name as arm_name,
                    a1.sort_order
                    FROM arm_or_cohort a1
                    WHERE a1.study_accession in (\'", study_id,"\')
                    ORDER BY a1.sort_order", sep="")
  
  arms <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(arms) > 0)
    colnames(arms) <- arms_column_names 
  cat("done", "\n")
  
  arms
}