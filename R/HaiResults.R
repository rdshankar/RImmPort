# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getHaiResults <- function(conn,study_id, measurement_type) {
  cat("loading HAI Assay Results data....")
  
  hai_cols <- c("study_id", "subject_id", "result_id",
                "result_in_original_units", "original_units", 
                "experiment_title", "assay_purpose", "measurement_technique",
                "experiment_sample_accession", "biosample_accession", "specimen_type", "specimen_subtype",
                "specimen_treatment", 
                "treatment_amount_value", "treatment_amount_unit",
                "treatment_duration_value", "treatment_duration_unit",
                "treatment_temperature_value", "treatment_temperature_unit",
                "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                "study_time_t0_event", "study_time_t0_event_specify",
                "virus_strain")
  
  sql_stmt <- paste("
                    SELECT distinct
                    hai.study_accession,
                    hai.subject_accession,
                    hai.result_id,
                    hai.value_reported,
                    'HA' as unit_reported,
                    ex.title,
                    ex.purpose,
                    ex.measurement_technique,
                    hai.expsample_accession,
                    bs.biosample_accession, 
                    bs.type,
                    bs.subtype,
                    tr.name,
                    tr.amount_value,
                    tr.amount_unit,
                    tr.duration_value,
                    tr.duration_unit,
                    tr.temperature_value,
                    tr.temperature_unit,
                    pv.visit_name,
                    pv.min_start_day,
                    pv.max_start_day,
                    pv.order_number,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    hai.virus_strain
                    FROM  
                      hai_result hai
                    INNER JOIN
                      experiment ex ON hai.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON hai.biosample_accession=bs.biosample_accession
                    LEFT OUTER JOIN
                      expsample_2_treatment es2tr ON hai.expsample_accession=es2tr.expsample_accession
                    LEFT OUTER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE hai.study_accession in (\'", study_id,"\') 
                    ORDER BY hai.subject_accession",sep="")
  
  hai_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(hai_df) > 0) {
    colnames(hai_df) <- hai_cols 
    
    hai_df <- ddply(hai_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
                      covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
                                                        unit_of_study_time_of_specimen_collection))
    
    hai_df <- ddply(hai_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
                      getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
    
  }
  
  cat("done", "\n")
  hai_df
}

getCountOfHaiResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      hai_result hai
                    INNER JOIN
                      experiment ex ON hai.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON hai.biosample_accession=bs.biosample_accession
                    WHERE hai.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}