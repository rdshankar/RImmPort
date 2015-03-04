
labs_column_names <- c("study_id", "subject_id", "lab_test_name","category_for_lab_test",
                           "result_in_original_units", "original_units",
                           "reference_range_lower_limit", "reference_range_upper_limit",
                           "reference_range_units", "reference_range_source",
                           "speciment_name", "specimen_type", "specimen_subtype",
                           "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                           "study_time_t0_event", "study_time_t0_event_specify")

getLaboratoryTestResults <- function(conn,study_id) {
  cat("loading Laboratory Test Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    lt.study_accession,
                    bs.subject_accession,
                    lt.name_reported,
                    lt.panel_name_reported,
                    lt.result_value_reported,
                    lt.result_unit_reported,
                    rr.upper_limit,
                    rr.lower_limit,
                    rr.unit_of_measure,
                    rr.lab_or_study_source,
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify                   
                    FROM  lab_test lt,
                          reference_range rr,
                          biosample bs
                    WHERE lt.study_accession in (\'", study_id,"\') AND 
                          lt.reference_range_accession=rr.reference_range_accession AND
                          lt.biosample_accession=bs.biosample_accession
                    ORDER BY bs.subject_accession",sep="")
  
  labs <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(labs) > 0)
    colnames(labs) <- labs_column_names 
  
  cat("done", "\n")
  labs
}