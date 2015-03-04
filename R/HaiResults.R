
hai_assay_column_names <- c("study_id", "subject_id", 
                       "result_in_original_units", "original_units", "assay_purpose",
                       "speciment_name", "specimen_type", "specimen_subtype",
                       "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                       "study_time_t0_event", "study_time_t0_event_specify",
                       "virus_strain", "file_name")

getHaiResults <- function(conn,study_id) {
  cat("loading HAI Assay Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    hai.study_accession,
                    hai.subject_accession,
                    hai.value_reported,
                    'HA' as unit_reported,
                    ex.purpose,
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    hai.virus_strain,
                    fi.name
                    FROM  hai_result hai,
                    experiment ex,
                    biosample bs,
                    file_info fi
                    WHERE hai.study_accession in (\'", study_id,"\') AND 
                    hai.experiment_accession=ex.experiment_accession AND
                    hai.biosample_accession=bs.biosample_accession AND
                    hai.file_info_id=fi.file_info_id
                    ORDER BY hai.subject_accession",sep="")
  
  hai_assay <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(hai_assay) > 0)
    colnames(hai_assay) <- hai_assay_column_names 
  
  cat("done", "\n")
  hai_assay
}