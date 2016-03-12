# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getMbaaResults <- function(conn,study_id, measurement_types) {
  cat("loading MBAA Results data....")
  
#   old <- options(stringsAsFactors = FALSE)
#   on.exit(options(old), add = TRUE)
#   options(useFancyQuotes = FALSE)
#   measurement_types <- paste(mapply(sQuote, measurement_types), collapse=", ")
  
  mbaa_cols <- c("study_id", "subject_id", "sequence",
                 "analyte", 
                 "concentration_unit", "concentration_value", 
                 "mfi", "mfi_coordinate", 
                 "experiment_title", "assay_purpose", "measurement_technique",
                 "experiment_sample_accession", "biosample_accession", "specimen_type", "specimen_subtype",
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
                      mbaa.study_accession,
                      mbaa.subject_accession,
                      mbaa.result_id,
                      mbaa.analyte, 
                      mbaa.concentration_unit, 
                      mbaa.concentration_value, 
                      mbaa.mfi, 
                      mbaa.mfi_coordinate, 
                      ex.title,
                      ex.purpose,
                      ex.measurement_technique,
                      mbaa.source_accession,
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
                      mbaa_result mbaa
                    INNER JOIN
                      experiment ex ON mbaa.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON mbaa.biosample_accession=bs.biosample_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE mbaa.study_accession in (\'", study_id,"\') 
                    ORDER BY mbaa.subject_accession",sep="")
  
  mbaa_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(mbaa_df) > 0) {
    colnames(mbaa_df) <- mbaa_cols 

    mbaa_df$elapsed_time_of_specimen_collection = mapply(covertElaspsedTimeToISO8601Format, mbaa_df$study_time_of_specimen_collection, 
                                                        mbaa_df$unit_of_study_time_of_specimen_collection)
    
    mbaa_df$time_point_reference = mapply(getTimePointReference, mbaa_df$study_time_t0_event, 
                                         mbaa_df$study_time_t0_event_specify)
    
    sql_stmt <- paste("
                      SELECT distinct
                        mbaa.source_accession,
                        tr.name,
                        tr.amount_value,
                        tr.amount_unit,
                        tr.duration_value,
                        tr.duration_unit,
                        tr.temperature_value,
                        tr.temperature_unit
                      FROM  
                        mbaa_result mbaa
                      INNER JOIN
                        expsample_2_treatment es2tr ON mbaa.source_accession=es2tr.expsample_accession
                      INNER JOIN
                        treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                      WHERE 
                        mbaa.study_accession in (\'", study_id,"\')",sep="")
    
    tr_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(tr_df) <- tr_cols 
    
    if (nrow(tr_df) >0) {
#      tr_df <- aggregate(. ~ experiment_sample_accession,paste,collapse="||",data=tr_df)
      tr_df <- setDF(setDT(tr_df)[, lapply(.SD, paste, collapse="||"), by="experiment_sample_accession"])
      mbaa_df <- merge(mbaa_df ,tr_df, by="experiment_sample_accession", all.x = TRUE)
    } else {
      mbaa_df["specimen_treatment"] = ""
      mbaa_df["treatment_amount_value"] = ""
      mbaa_df["treatment_amount_unit"] = ""
      mbaa_df["treatment_duration_value"] = ""
      mbaa_df["treatment_duration_unit"] = ""
      mbaa_df["treatment_temperature_value"] = ""
      mbaa_df["treatment_temperature_unit"] = ""
    }
    
#    mbaa_df <- transform(mbaa_df, sequence = as.integer(sequence))
#    setDT(mbaa_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
    setDT(mbaa_df)
    setorder(mbaa_df, "subject_id")
    mbaa_df <- as.data.frame(mbaa_df)
    
#     mbaa_df <- ddply(mbaa_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
#                       covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
#                                                         unit_of_study_time_of_specimen_collection))
#     
#     mbaa_df <- ddply(mbaa_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
#                       getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
  }
  
  cat("done", "\n")
  mbaa_df
}

getCountOfMbaaResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      mbaa_result mbaa
                    INNER JOIN
                      experiment ex ON mbaa.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      biosample bs ON mbaa.biosample_accession=bs.biosample_accession
                    WHERE mbaa.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}