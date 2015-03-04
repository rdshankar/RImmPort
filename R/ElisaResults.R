
elisa_column_names <- c("study_id", "subject_id", 
                                "analyte", "comments", 
                                "value", "unit",
                                "assay_purpose",
                                "speciment_name", "specimen_type", "specimen_subtype",
                                "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                                "study_time_t0_event", "study_time_t0_event_specify",
                                "file_name")

getElisaResults <- function(conn,study_id) {
  cat("loading ELISA Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    els.study_accession,
                    els.subject_accession,
                    els.analyte, 
                    els.comments, 
                    els.value_reported, 
                    els.unit_reported, 
                    ex.purpose,
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    fi.name
                    FROM  elisa_result els,
                    experiment ex,
                    biosample bs,
                    file_info fi
                    WHERE els.study_accession in (\'", study_id,"\') AND 
                    els.experiment_accession=ex.experiment_accession AND
                    els.biosample_accession=bs.biosample_accession AND
                    els.file_info_id=fi.file_info_id
                    ORDER BY els.subject_accession",sep="")
  
  elisa_results <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(elisa_results) > 0)
    colnames(elisa_results) <- elisa_column_names 
  
  cat("done", "\n")
  elisa_results
}