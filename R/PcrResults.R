
pcr_cols <- c("study_id", "subject_id", "result_id",
                      "entrez_gene_id", "gene_name", "gene_symbol", 
                      "threshold_cycles", "value_reported", "unit_reported",
                      "experiment_title", "assay_purpose", "measurement_technique",
                      "biosample_accession", "specimen_type", "specimen_subtype",
                      "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                      "study_time_t0_event", "study_time_t0_event_specify")

getPcrResults <- function(conn,study_id, measurement_type) {
  cat("loading PCR Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    pcr.study_accession,
                    pcr.subject_accession,
                    pcr.result_id,
                    pcr.entrez_gene_id, 
                    pcr.gene_name, 
                    pcr.gene_symbol, 
                    pcr.threshold_cycles, 
                    pcr.value_reported, 
                    pcr.unit_reported, 
                    ex.title,
                    ex.purpose,
                    ex.measurement_technique,
                    bs.biosample_accession, 
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify
                    FROM  
                      pcr_result pcr
                    INNER JOIN
                      experiment ex ON pcr.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON pcr.biosample_accession=bs.biosample_accession
                    WHERE pcr.study_accession in (\'", study_id,"\') 
                    ORDER BY pcr.subject_accession",sep="")
  
  pcr_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(pcr_df) > 0)
    colnames(pcr_df) <- pcr_cols 
  
  cat("done", "\n")
  pcr_df
}

getCountOfPcrResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      pcr_result pcr
                    INNER JOIN
                      experiment ex ON pcr.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON pcr.biosample_accession=bs.biosample_accession
                    WHERE pcr.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}