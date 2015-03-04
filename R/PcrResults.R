
pcr_column_names <- c("study_id", "subject_id",
                      "entrez_gene_id", "gene_symbol", 
                      "threshold_cycles", 
                      "assay_purpose",
                      "speciment_name", "specimen_type", "specimen_subtype",
                      "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                      "study_time_t0_event", "study_time_t0_event_specify",
                      "file_name")

getPcrResults <- function(conn,study_id) {
  cat("loading PCR Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    pcr.study_accession,
                    pcr.subject_accession,
                    pcr.entrez_gene_id, 
                    pcr.gene_symbol, 
                    pcr.threshold_cycles, 
                    ex.purpose,
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    fi.name
                    FROM  pcr_result pcr,
                    experiment ex,
                    biosample bs,
                    file_info fi
                    WHERE pcr.study_accession in (\'", study_id,"\') AND 
                    pcr.experiment_accession=ex.experiment_accession AND
                    pcr.biosample_accession=bs.biosample_accession AND
                    pcr.file_info_id=fi.file_info_id
                    ORDER BY pcr.subject_accession",sep="")
  
  pcr_results <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(pcr_results) > 0)
    colnames(pcr_results) <- pcr_column_names 
  
  cat("done", "\n")
  pcr_results
}