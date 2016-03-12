# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getNeutAbTiterResults <- function(conn,study_id, measurement_type) {
  cat("loading Neut. Ab Titer Results data....")

  nat_cols <- c("study_id", "subject_id", "sequence",
                "result_in_original_units", "original_units", 
                "experiment_title", "assay_purpose", "measurement_technique",
                "experiment_sample_accession", "biosample_accession", "specimen_type", "specimen_subtype",
                "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                "study_time_t0_event", "study_time_t0_event_specify",
                "virus_strain")

  tr_cols <- c("experiment_sample_accession",
               "specimen_treatment", 
               "treatment_amount_value", "treatment_amount_unit",
               "treatment_duration_value", "treatment_duration_unit",
               "treatment_temperature_value", "treatment_temperature_unit")
  
  sql_stmt <- paste("
                    SELECT distinct
                    nat.study_accession,
                    nat.subject_accession,
                    nat.result_id,
                    nat.value_reported,
                    'Neut Ab' as unit_reported,
                    ex.title,
                    ex.purpose,
                    ex.measurement_technique,
                    nat.expsample_accession,
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
                    bs.study_time_t0_event_specify,
                    nat.virus_strain
                    FROM  
                      neut_ab_titer_result nat
                    INNER JOIN
                      experiment ex ON nat.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON nat.biosample_accession=bs.biosample_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE nat.study_accession in (\'", study_id,"\')               
                    ORDER BY nat.subject_accession",sep="")
  
  nat_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(nat_df) > 0) {
    colnames(nat_df) <- nat_cols 
    
    nat_df$elapsed_time_of_specimen_collection = mapply(covertElaspsedTimeToISO8601Format, nat_df$study_time_of_specimen_collection, 
                                                        nat_df$unit_of_study_time_of_specimen_collection)
    
    nat_df$time_point_reference = mapply(getTimePointReference, nat_df$study_time_t0_event, 
                                         nat_df$study_time_t0_event_specify)
    
    sql_stmt <- paste("
                      SELECT distinct
                      nat.expsample_accession,
                      tr.name,
                      tr.amount_value,
                      tr.amount_unit,
                      tr.duration_value,
                      tr.duration_unit,
                      tr.temperature_value,
                      tr.temperature_unit
                      FROM  
                      neut_ab_titer_result nat
                      INNER JOIN
                      expsample_2_treatment es2tr ON nat.expsample_accession=es2tr.expsample_accession
                      INNER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                      WHERE 
                      nat.study_accession in (\'", study_id,"\')",sep="")
    
    tr_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(tr_df) <- tr_cols 
    
    if (nrow(tr_df) >0) {
#      tr_df <- aggregate(. ~ experiment_sample_accession,paste,collapse="||",data=tr_df)
      tr_df <- setDF(setDT(tr_df)[, lapply(.SD, paste, collapse="||"), by="experiment_sample_accession"])
      nat_df <- merge(nat_df ,tr_df, by="experiment_sample_accession", all.x = TRUE)
    } else {
      nat_df["specimen_treatment"] = ""
      nat_df["treatment_amount_value"] = ""
      nat_df["treatment_amount_unit"] = ""
      nat_df["treatment_duration_value"] = ""
      nat_df["treatment_duration_unit"] = ""
      nat_df["treatment_temperature_value"] = ""
      nat_df["treatment_temperature_unit"] = ""
    }
    
#     nat_df <- transform(nat_df, sequence = as.integer(sequence))
#     setDT(nat_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
    setDT(nat_df)
    setorder(nat_df, "subject_id")
    nat_df <- as.data.frame(nat_df)
    
#     nat_df <- ddply(nat_df, .(study_id, subject_id, sequence), mutate, elapsed_time_of_specimen_collection = 
#                       covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
#                                                         unit_of_study_time_of_specimen_collection))
#     
#     nat_df <- ddply(nat_df, .(study_id, subject_id, sequence), mutate, time_point_reference = 
#                       getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
  }
  
  cat("done", "\n")
  nat_df
}

getCountOfNeutAbTiterResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      neut_ab_titer_result nat
                    INNER JOIN
                      experiment ex ON nat.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON nat.biosample_accession=bs.biosample_accession
                    WHERE nat.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}