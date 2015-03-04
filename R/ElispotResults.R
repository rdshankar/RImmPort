
elispot_column_names <- c("study_id", "subject_id", 
                                "analyte", "comments", 
                                "cell_number", "cell_type", "spot_number", 
                                "assay_purpose",
                                "speciment_name", "specimen_type", "specimen_subtype",
                                "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                                "study_time_t0_event", "study_time_t0_event_specify",
                                "file_name")

getElispotResults <- function(conn,study_id) {
  cat("loading ELISPOT Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    elp.study_accession,
                    elp.subject_accession,
                    elp.analyte, 
                    elp.comments, 
                    elp.cell_number_reported, 
                    elp.cell_type, 
                    elp.spot_number_reported, 
                    ex.purpose,
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    fi.name
                    FROM  elispot_result elp,
                    experiment ex,
                    biosample bs,
                    file_info fi
                    WHERE elp.study_accession in (\'", study_id,"\') AND 
                    elp.experiment_accession=ex.experiment_accession AND
                    elp.biosample_accession=bs.biosample_accession AND
                    elp.file_info_id=fi.file_info_id
                    ORDER BY elp.subject_accession",sep="")
  
  elispot_results <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(elispot_results) > 0)
    colnames(elispot_results) <- elispot_column_names 
  
  cat("done", "\n")
  elispot_results
}