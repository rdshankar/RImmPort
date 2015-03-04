

hla_column_names <- c("study_id", "subject_id", "result_set_id",
                            "allele_1", "allele_2", 
                            "locus_name", "pop_area_name", 
                            "assay_purpose",
                            "speciment_name", "specimen_type", "specimen_subtype",
                            "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                            "study_time_t0_event", "study_time_t0_event_specify",
                            "file_name")

getHlaTypingResults <- function(conn,study_id) {
  cat("loading HLA Typing Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    hla.study_accession,
                    hla.subject_accession,
                    hla.result_set_id,
                    hla.allele_1, 
                    hla.allele_2, 
                    hla.locus_name, 
                    hla.pop_area_name, 
                    ex.purpose,
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    fi.name
                    FROM  hla_typing_result hla,
                    experiment ex,
                    biosample bs,
                    file_info fi
                    WHERE hla.study_accession in (\'", study_id,"\') AND 
                    hla.experiment_accession=ex.experiment_accession AND
                    hla.biosample_accession=bs.biosample_accession AND
                    hla.file_info_id=fi.file_info_id
                    ORDER BY hla.subject_accession",sep="")
  
  hla_results <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(hla_results) > 0)
    colnames(hla_results) <- hla_column_names 
  
  cat("done", "\n")
  hla_results
}