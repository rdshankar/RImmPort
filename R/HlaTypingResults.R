# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getHlaTypingResults <- function(conn,study_id, measurement_type) {
  cat("loading HLA Typing Results data....")
  
  hla_cols <- c("study_id", "subject_id", "result_id", "result_set_id",
                "allele_1", "allele_2", 
                "locus_name", "pop_area_name", 
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
                    hla.expsample_accession,
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
                      hla_typing_result hla
                    INNER JOIN
                      experiment ex ON hla.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON hla.biosample_accession=bs.biosample_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE hla.study_accession in (\'", study_id,"\') 
                    ORDER BY hla.subject_accession",sep="")
  
  hla_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(hla_df) > 0) {
    colnames(hla_df) <- hla_cols 

    hla_df$elapsed_time_of_specimen_collection = mapply(covertElaspsedTimeToISO8601Format, hla_df$study_time_of_specimen_collection, 
                                                        hla_df$unit_of_study_time_of_specimen_collection)
    
    hla_df$time_point_reference = mapply(getTimePointReference, hla_df$study_time_t0_event, 
                                         hla_df$study_time_t0_event_specify)
    
    sql_stmt <- paste("
                      SELECT distinct
                      hla.expsample_accession,
                      tr.name,
                      tr.amount_value,
                      tr.amount_unit,
                      tr.duration_value,
                      tr.duration_unit,
                      tr.temperature_value,
                      tr.temperature_unit
                      FROM  
                      hla_typing_result hla
                      INNER JOIN
                      expsample_2_treatment es2tr ON hla.expsample_accession=es2tr.expsample_accession
                      INNER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                      WHERE 
                      hla.study_accession in (\'", study_id,"\')",sep="")
    
    tr_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(tr_df) <- tr_cols 
    
    if (nrow(tr_df) >0) {
#      tr_df <- aggregate(. ~ experiment_sample_accession,paste,collapse="||",data=tr_df)
      tr_df <- setDF(setDT(tr_df)[, lapply(.SD, paste, collapse="||"), by="experiment_sample_accession"])
      hla_df <- merge(hla_df ,tr_df, by="experiment_sample_accession", all.x = TRUE)
    } else {
      hla_df["specimen_treatment"] = ""
      hla_df["treatment_amount_value"] = ""
      hla_df["treatment_amount_unit"] = ""
      hla_df["treatment_duration_value"] = ""
      hla_df["treatment_duration_unit"] = ""
      hla_df["treatment_temperature_value"] = ""
      hla_df["treatment_temperature_unit"] = ""
    }
    
    # hla_df <- transform(hla_df, sequence = as.integer(sequence))
    #setDT(hla_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
    #setorder(hla_df, "subject_id")
    #hla_df <- as.data.frame(hla_df)
    
    setDT(hla_df)
    setorder(hla_df, "subject_id")
    hla_df <- as.data.frame(hla_df)
    
    
#     hla_df <- ddply(hla_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
#                       covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
#                                                         unit_of_study_time_of_specimen_collection))
#     
#     hla_df <- ddply(hla_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
#                       getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
    
  }
  
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