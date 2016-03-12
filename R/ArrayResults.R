

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("sequence", "dataset_id", "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "study_time_t0_event", "study_time_t0_event_specify"))

#' @importFrom data.table as.data.table is.data.table .N := setorder
getArrayResults <- function(conn, study_id, measurement_type) {
    cat("loading Array Results data....")

  array_cols <- c("study_id", "subject_id", "sequence", "dataset_id", 
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
  
  sql_stmt <- paste("SELECT distinct
                        bs.study_accession,
                    bs.subject_accession,
                    cast(0 as UNSIGNED INTEGER) as sequence,
                    fi.name as dataset_id,
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
                    bs.study_time_t0_event_specify
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
                    es2fi.data_format like \"%Gene_Expression%\" AND
                    es2fi.file_info_id = fi.file_info_id AND
                    LOWER(fi.name) like \"%.cel%\"
                    ORDER BY bs.subject_accession",sep="")
  
  
  array_df <- DBI::dbGetQuery(conn, statement = sql_stmt)
  if (nrow(array_df) == 0) {
    
    sql_stmt <- paste("SELECT distinct
                         bs.study_accession,
                         bs.subject_accession,
                         cast(0 as UNSIGNED INTEGER) as sequence,
                         e2r.repository_accession as dataset_id,
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
                         bs.study_time_t0_event_specify
                      FROM  
                        biosample bs
                      INNER JOIN
                      biosample_2_expsample bs2es ON bs.biosample_accession=bs2es.biosample_accession
                      INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                      INNER JOIN
                      experiment ex ON bs2es.experiment_accession=ex.experiment_accession
                      INNER JOIN
                      expsample_public_repository e2r ON bs2es.expsample_accession=e2r.expsample_accession
                      WHERE 
                      bs.study_accession in (\'", study_id,"\') AND
                      e2r.repository_name='GEO' 
                      ORDER BY bs.subject_accession", sep = "")
    
    array_df <- DBI::dbGetQuery(conn, statement = sql_stmt)
    if (nrow(array_df) > 0) {
      array_df <- mutate(array_df, dataset_id = paste("urn:lsid:", "www.ncbi.nlm.nih.gov/geo", ":GEO:", dataset_id))
    } else {
      sql_stmt <- paste("SELECT distinct
                        bs.study_accession,
                        bs.subject_accession,
                        cast(0 as UNSIGNED INTEGER) as sequence,
                        fi.name as dataset_id,
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
                        bs.study_time_t0_event_specify
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
                        es2fi.data_format like \"%Gene_Expression%\" AND
                        es2fi.file_info_id = fi.file_info_id 
                        ORDER BY bs.subject_accession",sep="")
      
      
      array_df <- DBI::dbGetQuery(conn, statement = sql_stmt)
    }
  }

    colnames(array_df) <- array_cols
    if (nrow(array_df) > 0) {
      
      array_df$elapsed_time_of_specimen_collection = mapply(covertElaspsedTimeToISO8601Format, array_df$study_time_of_specimen_collection, 
                                                          array_df$unit_of_study_time_of_specimen_collection)
      
      array_df$time_point_reference = mapply(getTimePointReference, array_df$study_time_t0_event, 
                                           array_df$study_time_t0_event_specify)
      
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
        #tr_df <- aggregate(. ~ experiment_sample_accession,paste,collapse="||",data=tr_df)
        tr_df <- setDF(setDT(tr_df)[, lapply(.SD, paste, collapse="||"), by="experiment_sample_accession"])
        array_df <- merge(array_df ,tr_df, by="experiment_sample_accession", all.x = TRUE)
      } else {
        array_df["specimen_treatment"] = ""
        array_df["treatment_amount_value"] = ""
        array_df["treatment_amount_unit"] = ""
        array_df["treatment_duration_value"] = ""
        array_df["treatment_duration_unit"] = ""
        array_df["treatment_temperature_value"] = ""
        array_df["treatment_temperature_unit"] = ""
      }
      
      array_df <- transform(array_df, sequence = as.integer(sequence))
      setDT(array_df)[, `:=`(sequence, seq_len(.N)), by = "subject_id"]
      setorder(array_df, "subject_id")
      array_df <- as.data.frame(array_df)
      
      
#         array_df <- transform(array_df, result_id = as.integer(result_id))
#         arr_dt <- as.data.table(array_df)
#         if (is.data.table(arr_dt) == TRUE) {
#           arr_dt[, `:=`(result_id, seq_len(.N)), by = "subject_id"]
#         }
#         array_df <- as.data.frame(arr_dt)
# 
#         array_df <- ddply(array_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
#                           covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
#                                                             unit_of_study_time_of_specimen_collection))
#         
#         array_df <- ddply(array_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
#                           getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
        
    }
    
    
    cat("done", "\n")
    
    array_df
}

getCountOfArrayResults <- function(conn, study_id) {
  sql_stmt <- paste("SELECT count(*)
                      FROM experiment ex,
                    biosample bs,
                    biosample_2_expsample be,
                    expsample_public_repository er
                    WHERE ex.study_accession in ('", study_id, "') AND
                    be.experiment_accession=ex.experiment_accession AND
                    bs.biosample_accession=be.biosample_accession AND
                    be.expsample_accession=er.expsample_accession AND
                    er.repository_name='GEO'", sep = "")
  
  count <- dbGetQuery(conn, statement = sql_stmt)
  
  geo_count <- count[1, 1]
  
  sql_stmt <- paste("SELECT count(*)
                      FROM 
                        experiment ex,
                        biosample bs,
                        biosample_2_expsample be,
                        expsample_2_file_info es2fi, 
                        file_info fi                      
                      WHERE 
                        ex.study_accession in ('", study_id, "') AND 
                        be.experiment_accession=ex.experiment_accession AND
                        bs.biosample_accession=be.biosample_accession AND
                        be.expsample_accession=es2fi.expsample_accession AND
                        es2fi.data_format like \"%Gene_Expression%\" AND
                        es2fi.file_info_id = fi.file_info_id", 
                    sep = "")

  count <- dbGetQuery(conn, statement = sql_stmt)
  
  file_count <- count[1, 1]
  
  arr_count <- geo_count + file_count
  
  arr_count
} 
