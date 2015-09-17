

hla_cols <- c("study_id", "subject_id", "result_id", "result_set_id",
                            "allele_1", "allele_2", 
                            "locus_name", "pop_area_name", 
                      "experiment_title", "assay_purpose", "measurement_technique",
                      "biosample_accession", "specimen_type", "specimen_subtype",
                            "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                            "study_time_t0_event", "study_time_t0_event_specify")

getHlaTypingResults <- function(conn,study_id, measurement_type) {
  cat("loading HLA Typing Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    hla.study_accession,
                    hla.subject_accession,
                    hla.result_id,
                    hla.result_set_id,
                    hla.allele_1, 
                    hla.allele_2, 
                    hla.locus_name, 
                    hla.pop_area_name, 
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
                      hla_typing_result hla
                    INNER JOIN
                      experiment ex ON hla.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON hla.biosample_accession=bs.biosample_accession
                    WHERE hla.study_accession in (\'", study_id,"\') 
                    ORDER BY hla.subject_accession",sep="")
  
  hla_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(hla_df) > 0)
    colnames(hla_df) <- hla_cols 
  
  cat("done", "\n")
  hla_df
}

getCountOfHlaTypingResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      hla_typing_result hla
                    INNER JOIN
                      experiment ex ON hla.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON hla.biosample_accession=bs.biosample_accession
                    WHERE hla.study_accession in (\'", study_id,"\' )", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}