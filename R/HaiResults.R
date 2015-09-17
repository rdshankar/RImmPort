
hai_cols <- c("study_id", "subject_id", "result_id",
                       "result_in_original_units", "original_units", 
                       "experiment_title", "assay_purpose", "measurement_technique",
                       "biosample_accession", "specimen_type", "specimen_subtype",
                       "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                       "study_time_t0_event", "study_time_t0_event_specify",
                       "virus_strain")

getHaiResults <- function(conn,study_id, measurement_type) {
  cat("loading HAI Assay Results data....")
  
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
                    bs.biosample_accession, 
                    bs.type,
                    bs.subtype,
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
                    WHERE hai.study_accession in (\'", study_id,"\') 
                    ORDER BY hai.subject_accession",sep="")
  
  hai_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(hai_df) > 0)
    colnames(hai_df) <- hai_cols 
  
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