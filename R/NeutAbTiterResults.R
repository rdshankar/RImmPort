
nat_cols <- c("study_id", "subject_id", "result_id",
                      "result_in_original_units", "original_units", 
                      "experiment_title", "assay_purpose", "measurement_technique",
                      "biosample_accession", "specimen_type", "specimen_subtype",
                      "visit_name", "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                      "study_time_t0_event", "study_time_t0_event_specify",
                      "virus_strain")

getNeutAbTiterResults <- function(conn,study_id, measurement_type) {
  cat("loading Neut. Ab Titer Results data....")
  
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
                    bs.biosample_accession, 
                    bs.type,
                    bs.subtype,
                    pv.visit_name,
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