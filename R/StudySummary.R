
study_summary_column_names <- c("study_id", "title","brief_description", "description", "trial_indication", "primary_secondary_objectives", 
                                "investigational_therapy_or_treatment", "hypothesis", "study_start_date", "study_end_date", "target_enrollment", 
                                "actual_number_of_subjects", "maximum_age", "minimum_age", "age_unit", "sex_of_participants", "study_sponsor",  
                                "study_type")

getStudySummary <- function(conn,study_id) {
  cat("loading Study Summary data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    s1.study_accession,
                    s1.brief_title,
                    s1.brief_description,
                    s1.description,
                    s1.condition_studied,
                    s1.objectives,
                    s1.intervention_agent,
                    s1.hypothesis,
                    s1.actual_start_date,
                    s1.actual_completion_date,
                    s1.target_enrollment,
                    s1.actual_enrollment,
                    s1.maximum_age,
                    s1.minimum_age,
                    s1.age_unit,
                    s1.gender_included,
                    s1.sponsoring_organization,
                    s1.type
                    FROM study s1
                    WHERE s1.study_accession in (\'", study_id,"\')", sep="")
  
  study_summary <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(study_summary) > 0)
    colnames(study_summary) <- study_summary_column_names 
  cat("done", "\n")
  study_summary
}