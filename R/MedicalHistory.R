
medical_history_column_names <- c("study_id", "subject_id", "reported_term_for_the_medical_history",
                                 "category_for_medical_history", "body_system_or_organ_class",
                                 "age_at_onset", "age_at_onset_unit", "date_time_of_collection", "study_day_of_collection")

getMedicalHistory <- function(conn,study_id) {
  cat("loading Medical History data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    asm.study_accession,
                    asm.subject_accession,
                    asm.component_name_reported,
                    asm.panel_name_reported,
                    asm.organ_or_body_system_reported,
                    asm.age_at_onset_reported,
                    asm.age_at_onset_reported_unit,
                    asm.time_of_day,
                    asm.study_day                    
                    FROM  assessment asm
                    WHERE asm.study_accession in (\'", study_id,"\') AND asm.assessment_type='Medical History'
                    ORDER BY asm.subject_accession",sep="")
  
  medical_history <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(medical_history) > 0)
    colnames(medical_history) <- medical_history_column_names 
  cat("done", "\n")
  
  medical_history
}