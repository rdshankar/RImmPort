
fcs_column_names <- c("study_id", "subject_id", 
                            "speciment_name", "specimen_type", "specimen_subtype",
                            "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                            "study_time_t0_event", "study_time_t0_event_specify",
                            "assay_purpose",
                            "assay_measurement_technique",
                            "file_name")

getFcsResults <- function(conn,study_id) {
  cat("loading FCS Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    bs.study_accession,
                    bs.subject_accession,                   
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    ex.purpose,
                    ex.measurement_technique,
                    fi.name
                    FROM biosample bs,
                    experiment ex,
                    biosample_2_expsample bs2es,
                    expsample_2_file_info es2fi,
                    file_info fi                    
                    WHERE bs.study_accession in (\'", study_id,"\') AND 
                    bs.biosample_accession=bs2es.biosample_accession AND
                    bs2es.experiment_accession=ex.experiment_accession AND
                    bs2es.expsample_accession=es2fi.expsample_accession AND 
                    es2fi.file_info_id=fi.file_info_id AND 
                    fi.detail IN (\"Flow cytometry result in fcs format\") AND 
                    fi.purpose IN (\"Flow cytometry result\")
                    ORDER BY bs.subject_accession",sep="")
  
  fcs_results <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(fcs_results) > 0)
    colnames(fcs_results) <- fcs_column_names 
  
  cat("done", "\n")
  fcs_results
}