
vital_signs_column_names <- c("study_id", "subject_id", "vital_signs_test_name", "body_system_or_organ_class",
                                 "result_in_original_units", "original_units",
                                 "date_time_of_collection", "study_day_of_collection")

getVitalSigns <- function(conn,study_id) {
  cat("loading Vital Signs data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    asm.study_accession,
                    asm.subject_accession,
                    asm.component_name_reported,
                    asm.organ_or_body_system_reported,
                    asm.result_value_reported,
                    asm.result_unit_reported,
                    asm.time_of_day,
                    asm.study_day                    
                    FROM  assessment asm
                    WHERE (asm.study_accession in (\'", study_id,"\')) AND 
                          (asm.assessment_type='Physical Exam') AND
                          ((asm.component_name_reported='Heart Rate') OR
                           (asm.component_name_reported='Diastolic Blood Pressure') OR
                           (asm.component_name_reported='Systolic Blood Pressure') OR
                           (asm.component_name_reported='Height') OR
                           (asm.component_name_reported='Weight') OR
                           (asm.component_name_reported='Respiration Rate') OR
                           (asm.component_name_reported='Temperature'))
                    ORDER BY asm.subject_accession",sep="")
  
  vital_signs <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(vital_signs) > 0)
    colnames(vital_signs) <- vital_signs_column_names 
  cat("done", "\n")
  
  vital_signs
}