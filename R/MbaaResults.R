
mbaa_column_names <- c("study_id", "subject_id", 
                                "analyte", 
                                "concentration_unit", "concentration_value", 
                                "mfi", "mfi_coordinate", 
                                "assay_purpose",
                                "speciment_name", "specimen_type", "specimen_subtype",
                                "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                                "study_time_t0_event", "study_time_t0_event_specify",
                                "file_name")

getMbaaResults <- function(conn,study_id) {
  cat("loading MBAA Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    mbaa.study_accession,
                    mbaa.subject_accession,
                    mbaa.analyte, 
                    mbaa.concentration_unit, 
                    mbaa.concentration_value, 
                    mbaa.mfi, 
                    mbaa.mfi_coordinate, 
                    ex.purpose,
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    fi.name
                    FROM  mbaa_result mbaa,
                    experiment ex,
                    biosample bs,
                    file_info fi
                    WHERE mbaa.study_accession in (\'", study_id,"\') AND 
                    mbaa.experiment_accession=ex.experiment_accession AND
                    mbaa.biosample_accession=bs.biosample_accession AND
                    mbaa.file_info_id=fi.file_info_id
                    ORDER BY mbaa.subject_accession",sep="")
  
  mbaa_results <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(mbaa_results) > 0)
    colnames(mbaa_results) <- mbaa_column_names 
  
  cat("done", "\n")
  mbaa_results
}