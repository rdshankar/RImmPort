# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))


getFcsResults <- function(conn,study_id, measurement_types) {
  cat("loading FCS Results data....")
  
#   old <- options(stringsAsFactors = FALSE)
#   on.exit(options(old), add = TRUE)
#   options(useFancyQuotes = FALSE)
#   measurement_types <- paste(mapply(sQuote, measurement_types), collapse=", ")
  
  fcs_cols <- c("study_id", "subject_id", "sequence",
                "experiment_title", "assay_purpose", "measurement_technique",
                "experiment_sample_accession",
                "biosample_accession", "specimen_type", "specimen_subtype", 
                "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                "study_time_t0_event", "study_time_t0_event_specify",
                "file_name",
                "base_parent_population", 
                "population_cell_number", "population_cell_number_unit",
                "population_defnition_reported", "population_name_reported")
  
  fcf_cols <- c("experiment_sample_accession",
                "control_files_names")
  
  tr_cols <- c("experiment_sample_accession",
               "specimen_treatment", 
               "treatment_amount_value", "treatment_amount_unit",
               "treatment_duration_value", "treatment_duration_unit",
               "treatment_temperature_value", "treatment_temperature_unit")

#   far_cols <- c("experiment_sample_accession",
#                 "base_parent_population", 
#                 "population_cell_number", "population_cell_number_unit",
#                 "population_defnition_reported", "population_name_reported")
  
  sql_stmt <- paste("
                    SELECT distinct
                      bs.study_accession,
                      bs.subject_accession,     
                      cast(0 as UNSIGNED INTEGER) as seq,
                      ex.title,
                      ex.purpose,
                      ex.measurement_technique,
                      bs2es.expsample_accession,
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
                      fi.name,
                      \"\", \"\", \"\", \"\", \"\"
                    
                    FROM  
                      biosample bs
                    INNER JOIN
                      biosample_2_expsample bs2es ON bs.biosample_accession=bs2es.biosample_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    INNER JOIN
                      experiment ex ON bs2es.experiment_accession=ex.experiment_accession
                    INNER JOIN
                      expsample_2_file_info es2fi ON bs2es.expsample_accession=es2fi.expsample_accession
                    INNER JOIN
                      file_info fi ON es2fi.file_info_id=fi.file_info_id                    
                    WHERE 
                      bs.study_accession in (\'", study_id,"\') AND
                      fi.detail IN (\'Flow cytometry result in fcs format\', \'CyTOF result in fcs format\') AND 
                      fi.purpose IN (\'Flow cytometry result\', \'CyTOF result\')  
                    ORDER BY bs.subject_accession",sep="")

  fcs_df <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(fcs_df) > 0) {
    colnames(fcs_df) <- fcs_cols 
    
    fcs_df$elapsed_time_of_specimen_collection = mapply(covertElaspsedTimeToISO8601Format, fcs_df$study_time_of_specimen_collection, 
                                                        fcs_df$unit_of_study_time_of_specimen_collection)
    
    fcs_df$time_point_reference = mapply(getTimePointReference, fcs_df$study_time_t0_event, 
                                         fcs_df$study_time_t0_event_specify)
    
#     fcs_df <- transform(fcs_df, sequence = as.integer(sequence))
#     setDT(fcs_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
#     fcs_df <- as.data.frame(fcs_df)
#     fcs_df <- ddply(fcs_df, .(study_id, subject_id, sequence), mutate, elapsed_time_of_specimen_collection = 
#             covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
#                                               unit_of_study_time_of_specimen_collection))
#     
#     fcs_df <- ddply(fcs_df, .(study_id, subject_id, sequence), mutate, time_point_reference = 
#                       getTimePointReference(study_time_t0_event, study_time_t0_event_specify))

    sql_stmt <- paste("
                      SELECT distinct
                        bs2es.expsample_accession,
                        fi.name
                      FROM  
                        biosample bs
                      INNER JOIN
                        biosample_2_expsample bs2es ON bs.biosample_accession=bs2es.biosample_accession
                      INNER JOIN
                        expsample_2_file_info es2fi ON bs2es.expsample_accession=es2fi.expsample_accession
                      INNER JOIN
                        file_info fi ON es2fi.file_info_id=fi.file_info_id                    
                      WHERE 
                        bs.study_accession in (\'", study_id,"\') AND
                        fi.purpose IN (\'Flow cytometry compensation or control\')  
                      ORDER BY bs.subject_accession",sep="")
    
    fcf_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(fcf_df) <- fcf_cols 
    if (nrow(fcf_df) >0) {
      fcf_df <- aggregate(control_files_names~experiment_sample_accession,paste,collapse="||",data=fcf_df)
      fcs_df <- merge(fcs_df ,fcf_df, by="experiment_sample_accession")
    } else {
      fcs_df["control_files_names"] = ""
    }
    
    sql_stmt <- paste("
                      SELECT distinct
                        bs2es.expsample_accession,
                        tr.name,
                        tr.amount_value,
                        tr.amount_unit,
                        tr.duration_value,
                        tr.duration_unit,
                        tr.temperature_value,
                        tr.temperature_unit
                      FROM  
                        biosample bs
                      INNER JOIN
                        biosample_2_expsample bs2es ON bs.biosample_accession=bs2es.biosample_accession
                      INNER JOIN
                        expsample_2_treatment es2tr ON bs2es.expsample_accession=es2tr.expsample_accession
                      INNER JOIN
                        treatment tr ON es2tr.treatment_accession=tr.treatment_accession
                      WHERE 
                        bs.study_accession in (\'", study_id,"\')",sep="")
    
    tr_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(tr_df) <- tr_cols 
    
    if (nrow(tr_df) >0) {
#      tr_df <- aggregate(. ~ experiment_sample_accession,paste,collapse="||",data=tr_df)
      tr_df <- setDF(setDT(tr_df)[, lapply(.SD, paste, collapse="||"), by="experiment_sample_accession"])
      fcs_df <- merge(fcs_df ,tr_df, by="experiment_sample_accession", all.x = TRUE)
    } else {
      fcs_df["specimen_treatment"] = ""
      fcs_df["treatment_amount_value"] = ""
      fcs_df["treatment_amount_unit"] = ""
      fcs_df["treatment_duration_value"] = ""
      fcs_df["treatment_duration_unit"] = ""
      fcs_df["treatment_temperature_value"] = ""
      fcs_df["treatment_temperature_unit"] = ""
    }
    
    fcs_df <- transform(fcs_df, sequence = as.integer(sequence))
    setDT(fcs_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
    setorder(fcs_df, "subject_id")
    fcs_df <- as.data.frame(fcs_df)
    
    
#     sql_stmt <- paste("
#                       SELECT distinct
#                       far.expsample_accession,
#                       far.base_parent_population, 
#                       far.population_cell_number, 
#                       far.population_cell_number_unit,
#                       far.population_defnition_reported, 
#                       far.population_name_reported
#                       FROM  
#                       fcs_analyzed_result far
#                       WHERE 
#                       far.study_accession in (\'", study_id,"\')",sep="")
#     
#     
#     far_df <- dbGetQuery(conn,statement=sql_stmt)
#     colnames(far_df) <- far_cols 
#     fcs_df <- merge(fcs_df ,far_df, all=TRUE)
  }
  
  cat("done", "\n")
  fcs_df
}

getCountOfFcsResults <- function(conn,study_id) {  
  sql_stmt <- paste("
                    SELECT count(*)
                    FROM biosample bs,
                    experiment ex,
                    biosample_2_expsample bs2es,
                    expsample_2_file_info es2fi,
                    file_info fi                    
                    WHERE bs.study_accession in (\'", study_id,"\') AND 
                    bs.biosample_accession=bs2es.biosample_accession AND
                    bs2es.experiment_accession=ex.experiment_accession AND
                    bs2es.expsample_accession=es2fi.expsample_accession AND 
                    es2fi.file_info_id=fi.file_info_id AND 
                    fi.detail IN (\"Flow cytometry result in fcs format\") AND 
                    fi.purpose IN (\"Flow cytometry result\")", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}