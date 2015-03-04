
findings_about_column_names <- c("study_id", "subject_id", "findings_about_test_name", "object_of_the_observation",
                                 "category_for_findings_about", "results_in_original_units", "original_units", "location_of_finding_about", 
                                 "date_time_of_collection", "study_day_of_collection")

getFindingsAbout <- function(conn,study_id) {
  cat("loading Findings About data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                        asm.study_accession,
                        asm.subject_accession,
                        asm.component_name_reported,
                        asm.organ_or_body_system_reported,
                        asm.panel_name_reported,
                        asm.result_value_reported,
                        asm.result_unit_reported,
                        asm.location_of_finding_reported,
                        asm.time_of_day,
                        asm.study_day                    
                    FROM  assessment asm
                    WHERE asm.study_accession in (\'", study_id,"\') AND asm.assessment_type='Other'
                    ORDER BY asm.subject_accession",sep="")
  
  findings_about <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(findings_about) > 0)
    colnames(findings_about) <- findings_about_column_names 
  cat("done", "\n")
  
  findings_about
}