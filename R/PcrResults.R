# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))

getPcrResults <- function(conn,study_id, measurement_type) {
  cat("loading PCR Results data....")

  pcr_cols <- c("study_id", "subject_id", "sequence",
                "entrez_gene_id", "gene_name", "gene_symbol", 
                "threshold_cycles", "value_reported", "unit_reported",
                "experiment_title", "assay_purpose", "measurement_technique",
                "experiment_sample_accession", "biosample_accession", "specimen_type", "specimen_subtype",
                "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                "study_time_t0_event", "study_time_t0_event_specify")

  tr_cols <- c("experiment_sample_accession",
               "specimen_treatment", 
               "treatment_amount_value", "treatment_amount_unit",
               "treatment_duration_value", "treatment_duration_unit",
               "treatment_temperature_value", "treatment_temperature_unit")
  
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
                    pcr.expsample_accession,
                    bs.biosample_accession, 
                    bs.type,
                    bs.subtype,
                    pv.visit_name,
                    pv.min_start_day,
                    pv.max_start_day,
                    pv.order_number,
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
                    LEFT OUTER JOIN
                      expsample_2_treatment es2tr ON pcr.expsample_accession=es2tr.expsample_accession
                    LEFT OUTER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE pcr.study_accession in (\'", study_id,"\') 
                    ORDER BY pcr.subject_accession",sep="")
  
  pcr_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(pcr_df) > 0) {
    colnames(pcr_df) <- pcr_cols 

    pcr_df$elapsed_time_of_specimen_collection = mapply(covertElaspsedTimeToISO8601Format, pcr_df$study_time_of_specimen_collection, 
                                                        pcr_df$unit_of_study_time_of_specimen_collection)
    
    pcr_df$time_point_reference = mapply(getTimePointReference, pcr_df$study_time_t0_event, 
                                         pcr_df$study_time_t0_event_specify)
    
    sql_stmt <- paste("
                      SELECT distinct
                      pcr.expsample_accession,
                      tr.name,
                      tr.amount_value,
                      tr.amount_unit,
                      tr.duration_value,
                      tr.duration_unit,
                      tr.temperature_value,
                      tr.temperature_unit
                      FROM  
                      pcr_result pcr
                      INNER JOIN
                      expsample_2_treatment es2tr ON pcr.expsample_accession=es2tr.expsample_accession
                      INNER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                      WHERE 
                      pcr.study_accession in (\'", study_id,"\')",sep="")
    
    tr_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(tr_df) <- tr_cols 
    
    if (nrow(tr_df) >0) {
#      tr_df <- aggregate(. ~ experiment_sample_accession,paste,collapse="||",data=tr_df)
      tr_df <- setDF(setDT(tr_df)[, lapply(.SD, paste, collapse="||"), by="experiment_sample_accession"])
      pcr_df <- merge(pcr_df ,tr_df, by="experiment_sample_accession", all.x = TRUE)
    } else {
      pcr_df["specimen_treatment"] = ""
      pcr_df["treatment_amount_value"] = ""
      pcr_df["treatment_amount_unit"] = ""
      pcr_df["treatment_duration_value"] = ""
      pcr_df["treatment_duration_unit"] = ""
      pcr_df["treatment_temperature_value"] = ""
      pcr_df["treatment_temperature_unit"] = ""
    }
    
#     pcr_df <- transform(pcr_df, sequence = as.integer(sequence))
#     setDT(pcr_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
    setDT(pcr_df)
    setorder(pcr_df, "subject_id")
    pcr_df <- as.data.frame(pcr_df)
    
#     pcr_df <- ddply(pcr_df, .(study_id, subject_id, sequence), mutate, elapsed_time_of_specimen_collection = 
#                       covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
#                                                         unit_of_study_time_of_specimen_collection))
#     
#     pcr_df <- ddply(pcr_df, .(study_id, subject_id, sequence), mutate, time_point_reference = 
#                       getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
  }
  
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