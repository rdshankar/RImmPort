
demographics_column_names <- c("study_id", "subject_id","age", "age_unit", "sex", "race", "ethnicity", "arm_code", "arm")

getDemographics <- function(conn,study_id) {
  cat("loading Demographics data....")
  
  sql_stmt <- paste("
                                  SELECT distinct
                                  a2.study_accession,
                                  s1.subject_accession,
                                  s1.age_reported,
                                  s1.age_unit,
                                  s1.gender,
                                  s1.race,
                                  s1.ethnicity,
                                  a2.arm_accession,
                                  a2.name as arm_name
                                  FROM subject s1,
                                  arm_2_subject a1,
                                  arm_or_cohort a2
                                  WHERE s1.subject_accession = a1.subject_accession
                                  AND a1.arm_accession = a2.arm_accession
                                  AND a2.study_accession in (\'", study_id,"\')
                                  ORDER BY s1.subject_accession",sep="")
  
  demographics <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(demographics) > 0)
    colnames(demographics) <- demographics_column_names 
  cat("done", "\n")
  demographics
}