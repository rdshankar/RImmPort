# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getElisaResults <- function(conn,study_id, measurement_types) {
  cat("loading ELISA Results data....")

#   old <- options(stringsAsFactors = FALSE)
#   on.exit(options(old), add = TRUE)
#   options(useFancyQuotes = FALSE)
#   measurement_types <- paste(mapply(sQuote, measurement_types), collapse=", ")
  elisa_cols <- c("study_id", "subject_id", "sequence",
                  "analyte", "comments", 
                  "value", "unit",
                  "experiment_title", "assay_purpose", "measurement_technique",
                  "experiment_sample_accession",
                  "biosample_accession", "specimen_type", "specimen_subtype",
                  "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                  "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify",
                  "file_name")

  tr_cols <- c("experiment_sample_accession",
               "specimen_treatment", 
               "treatment_amount_value", "treatment_amount_unit",
               "treatment_duration_value", "treatment_duration_unit",
               "treatment_temperature_value", "treatment_temperature_unit")
  
  sql_stmt <- paste("
                    SELECT distinct
                      els.study_accession,
                      els.subject_accession,
                      els.result_id,
                      els.analyte, 
                      els.comments, 
                      els.value_reported, 
                      els.unit_reported, 
                      ex.title,
                      ex.purpose,
                      ex.measurement_technique,
                      els.expsample_accession,
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
                      \"\"
                    FROM  
                      elisa_result els
					          INNER JOIN
						          experiment ex ON els.experiment_accession=ex.experiment_accession
					          INNER JOIN
						          biosample bs ON els.biosample_accession=bs.biosample_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE els.study_accession in (\'", study_id,"\') AND
                          ex.purpose != 'Neutralizing_Antibody_Titer'
                    ORDER BY els.subject_accession",sep="")

  elisa_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(elisa_df) > 0) {
    colnames(elisa_df) <- elisa_cols 
    els_df$elapsed_time_of_specimen_collection = mapply(covertElaspsedTimeToISO8601Format, els_df$study_time_of_specimen_collection, 
                                                        els_df$unit_of_study_time_of_specimen_collection)
    
    els_df$time_point_reference = mapply(getTimePointReference, els_df$study_time_t0_event, 
                                         els_df$study_time_t0_event_specify)
    
    sql_stmt <- paste("
                      SELECT distinct
                      els.expsample_accession,
                      tr.name,
                      tr.amount_value,
                      tr.amount_unit,
                      tr.duration_value,
                      tr.duration_unit,
                      tr.temperature_value,
                      tr.temperature_unit
                      FROM  
                      els_result els
                      INNER JOIN
                      expsample_2_treatment es2tr ON els.expsample_accession=es2tr.expsample_accession
                      INNER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                      WHERE 
                      els.study_accession in (\'", study_id,"\')",sep="")
    
    tr_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(tr_df) <- tr_cols 
    
    if (nrow(tr_df) >0) {
      #tr_df <- aggregate(. ~ experiment_sample_accession,paste,collapse="||",data=tr_df)
      tr_df <- setDF(setDT(tr_df)[, lapply(.SD, paste, collapse="||"), by="experiment_sample_accession"])
      elisa_df <- merge(elisa_df ,tr_df, by="experiment_sample_accession", all.x = TRUE)
    } else {
      elisa_df["specimen_treatment"] = ""
      elisa_df["treatment_amount_value"] = ""
      elisa_df["treatment_amount_unit"] = ""
      elisa_df["treatment_duration_value"] = ""
      elisa_df["treatment_duration_unit"] = ""
      elisa_df["treatment_temperature_value"] = ""
      elisa_df["treatment_temperature_unit"] = ""
    }
    
#     elisa_df <- transform(elisa_df, sequence = as.integer(sequence))
#     setDT(elisa_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
    setDT(elisa_df)
    setorder(elisa_df, "subject_id")
    elisa_df <- as.data.frame(elisa_df)
    
    
#     elisa_df <- ddply(elisa_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
#                       covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
#                                                         unit_of_study_time_of_specimen_collection))
#     
#     elisa_df <- ddply(elisa_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
#                       getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
    
  }
  
  cat("done", "\n")
  elisa_df
}

getCountOfElisaResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      elisa_result els
                    INNER JOIN
                      experiment ex ON els.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON els.biosample_accession=bs.biosample_accession
                    WHERE els.study_accession in (\'", study_id,"\') AND
                          ex.purpose != 'Neutralizing_Antibody_Titer'", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}