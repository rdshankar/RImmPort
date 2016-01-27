# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getNeutAbTiterResults <- function(conn,study_id, measurement_type) {
  cat("loading Neut. Ab Titer Results data....")

  nat_cols <- c("study_id", "subject_id", "result_id",
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
                    nat.study_accession,
                    nat.subject_accession,
                    nat.result_id,
                    nat.value_reported,
                    'Neut Ab' as unit_reported,
                    ex.title,
                    ex.purpose,
                    ex.measurement_technique,
                    nat.expsample_accession,
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
                    nat.virus_strain
                    FROM  
                      neut_ab_titer_result nat
                    INNER JOIN
                      experiment ex ON nat.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON nat.biosample_accession=bs.biosample_accession
                    LEFT OUTER JOIN
                      expsample_2_treatment es2tr ON nat.expsample_accession=es2tr.expsample_accession
                    LEFT OUTER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE nat.study_accession in (\'", study_id,"\')               
                    ORDER BY nat.subject_accession",sep="")
  
  nat_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(nat_df) > 0) {
    colnames(nat_df) <- nat_cols 
    
    nat_df <- ddply(nat_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
                      covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
                                                        unit_of_study_time_of_specimen_collection))
    
    nat_df <- ddply(nat_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
                      getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
  }
  
  cat("done", "\n")
  nat_df
}

getCountOfNeutAbTiterResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      neut_ab_titer_result nat
                    INNER JOIN
                      experiment ex ON nat.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON nat.biosample_accession=bs.biosample_accession
                    WHERE nat.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}