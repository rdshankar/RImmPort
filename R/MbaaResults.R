
mbaa_cols <- c("study_id", "subject_id", "result_id",
                                "analyte", 
                                "concentration_unit", "concentration_value", 
                                "mfi", "mfi_coordinate", 
                                "experiment_title", "assay_purpose", "measurement_technique",
                                "biosample_accession", "specimen_type", "specimen_subtype",
                                "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                                "study_time_t0_event", "study_time_t0_event_specify",
                                "file_name")

getMbaaResults <- function(conn,study_id, measurement_types) {
  cat("loading MBAA Results data....")
  
#   old <- options(stringsAsFactors = FALSE)
#   on.exit(options(old), add = TRUE)
#   options(useFancyQuotes = FALSE)
#   measurement_types <- paste(mapply(sQuote, measurement_types), collapse=", ")
  
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
                    bs.biosample_accession,
                    bs.type,
                    bs.subtype,
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
                    WHERE mbaa.study_accession in (\'", study_id,"\') 
                    ORDER BY mbaa.subject_accession",sep="")
  
  mbaa_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(mbaa_df) > 0)
    colnames(mbaa_df) <- mbaa_cols 
  
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