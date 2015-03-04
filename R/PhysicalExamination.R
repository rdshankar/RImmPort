
physical_examination_column_names <- c("study_id", "subject_id", "body_system_examined", "body_system_or_organ_class",
                              "verbatim_examination_finding", "original_units",
                              "date_time_of_collection", "study_day_of_collection")

getPhysicalExamination <- function(conn,study_id) {
  cat("loading Physical Examination data....")
  
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
                    ((asm.component_name_reported!='Heart Rate') AND
                    (asm.component_name_reported!='Diastolic Blood Pressure') AND
                    (asm.component_name_reported!='Systolic Blood Pressure') AND
                    (asm.component_name_reported!='Height') AND
                    (asm.component_name_reported!='Weight') AND
                    (asm.component_name_reported!='Respiration Rate') AND
                    (asm.component_name_reported!='Temperature'))
                    ORDER BY asm.subject_accession",sep="")
  
  physical_examination <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(physical_examination) > 0)
    colnames(physical_examination) <- physical_examination_column_names 
  cat("done", "\n")
  
  physical_examination
}