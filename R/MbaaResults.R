# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getMbaaResults <- function(conn,study_id, measurement_types) {
  cat("loading MBAA Results data....")
  
#   old <- options(stringsAsFactors = FALSE)
#   on.exit(options(old), add = TRUE)
#   options(useFancyQuotes = FALSE)
#   measurement_types <- paste(mapply(sQuote, measurement_types), collapse=", ")
  
  mbaa_cols <- c("study_id", "subject_id", "result_id",
                 "analyte", 
                 "concentration_unit", "concentration_value", 
                 "mfi", "mfi_coordinate", 
                 "experiment_title", "assay_purpose", "measurement_technique",
                 "experiment_sample_accession", "biosample_accession", "specimen_type", "specimen_subtype",
                 "specimen_treatment", 
                 "treatment_amount_value", "treatment_amount_unit",
                 "treatment_duration_value", "treatment_duration_unit",
                 "treatment_temperature_value", "treatment_temperature_unit",
                 "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                 "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                 "study_time_t0_event", "study_time_t0_event_specify",
                 "file_name")
  
  sql_stmt <- paste("
                    SELECT distinct
                    mbaa.study_accession,
                    mbaa.subject_accession,
                    mbaa.result_id,
                    mbaa.analyte, 
                    mbaa.concentration_unit, 
                    mbaa.concentration_value, 
                    mbaa.mfi, 
                    mbaa.mfi_coordinate, 
                    ex.title,
                    ex.purpose,
                    ex.measurement_technique,
                    mbaa.source_accession,
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
                    \"\"
                    FROM  
                      mbaa_result mbaa
                    INNER JOIN
                      experiment ex ON mbaa.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON mbaa.biosample_accession=bs.biosample_accession
                    LEFT OUTER JOIN
                      expsample_2_treatment es2tr ON mbaa.source_type = 'EXPERIMENTAL_SAMPLE' AND mbaa.source_accession=es2tr.expsample_accession
                    LEFT OUTER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE mbaa.study_accession in (\'", study_id,"\') 
                    ORDER BY mbaa.subject_accession",sep="")
  
  mbaa_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(mbaa_df) > 0) {
    colnames(mbaa_df) <- mbaa_cols 
    
    mbaa_df <- ddply(mbaa_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
                      covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
                                                        unit_of_study_time_of_specimen_collection))
    
    mbaa_df <- ddply(mbaa_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
                      getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
  }
  
  cat("done", "\n")
  mbaa_df
}

getCountOfMbaaResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      mbaa_result mbaa
                    INNER JOIN
                    experiment ex ON mbaa.experiment_accession=ex.experiment_accession
                    INNER JOIN
                    biosample bs ON mbaa.biosample_accession=bs.biosample_accession
                    WHERE mbaa.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}