# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getElispotResults <- function(conn,study_id, measurement_types) {
  cat("loading ELISPOT Results data....")
  
#   old <- options(stringsAsFactors = FALSE)
#   on.exit(options(old), add = TRUE)
#   options(useFancyQuotes = FALSE)
#   measurement_types <- paste(mapply(sQuote, measurement_types), collapse=", ")
  
  elispot_cols <- c("study_id", "subject_id", "sequence",
                    "analyte", "comments", 
                    "cell_number", "cell_type", "spot_number", 
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
                    elp.study_accession,
                    elp.subject_accession,
                    elp.result_id,
                    elp.analyte, 
                    elp.comments, 
                    elp.cell_number_reported, 
                    elp.cell_type, 
                    elp.spot_number_reported, 
                    ex.title,
                    ex.purpose,
                    ex.measurement_technique,
                    elp.expsample_accession,
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
                      elispot_result elp
					          INNER JOIN
						          experiment ex ON elp.experiment_accession=ex.experiment_accession
					          INNER JOIN
						          biosample bs ON elp.biosample_accession=bs.biosample_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    WHERE elp.study_accession in (\'", study_id,"\')  
                    ORDER BY elp.subject_accession",sep="")
  
  elispot_df <- dbGetQuery(conn,statement=sql_stmt)

  if (nrow(elispot_df) > 0) {
    colnames(elispot_df) <- elispot_cols 

    elispot_df$elapsed_time_of_specimen_collection = mapply(covertElaspsedTimeToISO8601Format, elispot_df$study_time_of_specimen_collection, 
                                                        elispot_df$unit_of_study_time_of_specimen_collection)
    
    elispot_df$time_point_reference = mapply(getTimePointReference, elispot_df$study_time_t0_event, 
                                         elispot_df$study_time_t0_event_specify)
    
    sql_stmt <- paste("
                      SELECT distinct
                      elp.expsample_accession,
                      tr.name,
                      tr.amount_value,
                      tr.amount_unit,
                      tr.duration_value,
                      tr.duration_unit,
                      tr.temperature_value,
                      tr.temperature_unit
                      FROM  
                      elispot_result elp
                      INNER JOIN
                      expsample_2_treatment es2tr ON elp.expsample_accession=es2tr.expsample_accession
                      INNER JOIN
                      treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                      WHERE 
                      elp.study_accession in (\'", study_id,"\')",sep="")
    
    tr_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(tr_df) <- tr_cols 
    
    if (nrow(tr_df) >0) {
#      tr_df <- aggregate(. ~ experiment_sample_accession,paste,collapse="||",data=tr_df)
      tr_df <- setDF(setDT(tr_df)[, lapply(.SD, paste, collapse="||"), by="experiment_sample_accession"])
      elispot_df <- merge(elispot_df ,tr_df, by="experiment_sample_accession", all.x = TRUE)
    } else {
      elispot_df["specimen_treatment"] = ""
      elispot_df["treatment_amount_value"] = ""
      elispot_df["treatment_amount_unit"] = ""
      elispot_df["treatment_duration_value"] = ""
      elispot_df["treatment_duration_unit"] = ""
      elispot_df["treatment_temperature_value"] = ""
      elispot_df["treatment_temperature_unit"] = ""
    }
    
#     elispot_df <- transform(elispot_df, sequence = as.integer(sequence))
#     setDT(elispot_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
    setDT(elispot_df)
    setorder(elispot_df, "subject_id")
    elispot_df <- as.data.frame(elispot_df)
    
    
#     elispot_df <- ddply(elispot_df, .(study_id, subject_id, sequence), mutate, elapsed_time_of_specimen_collection = 
#                       covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
#                                                         unit_of_study_time_of_specimen_collection))
#     
#     elispot_df <- ddply(elispot_df, .(study_id, subject_id, sequence), mutate, time_point_reference = 
#                       getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
    
  }
  
  cat("done", "\n")
  elispot_df
}

getCountOfElispotResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM  
                      elispot_result elp
					          INNER JOIN
						          experiment ex ON elp.experiment_accession=ex.experiment_accession
					          INNER JOIN
						          biosample bs ON elp.biosample_accession=bs.biosample_accession
                    WHERE elp.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}