
array_cols <- c("study_id", "subject_id", "result_id", "dataset_id", 
                        "experiment_title", "assay_purpose", "measurement_technique",
                        "biosample_accession", "specimen_type", "specimen_subtype", 
                        "visit_name", "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                        "study_time_t0_event", "study_time_t0_event_specify")

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("result_id"))

#' @importFrom data.table as.data.table is.data.table .N :=
getArrayResults <- function(conn, study_id, measurement_type) {
    cat("loading Array Results data....")
    
    sql_stmt <- paste("SELECT distinct
                         bs.study_accession,
                         bs.subject_accession,
                         cast(0 as UNSIGNED INTEGER) as result_id,
                         er.repository_accession as dataset_id,
                         ex.title,
                         ex.purpose,
                         ex.measurement_technique,
                         bs.biosample_accession, 
                         bs.type,
                         bs.subtype,
                         pv.visit_name,
                         bs.study_time_collected,
                         bs.study_time_collected_unit,
                         bs.study_time_t0_event,
                         bs.study_time_t0_event_specify
                       FROM experiment ex,
                         biosample bs,
                         planned_visit pv,
                         biosample_2_expsample be,
                         expsample_public_repository er
                       WHERE ex.study_accession in ('", study_id, "') AND 
                         be.experiment_accession=ex.experiment_accession AND
                         bs.biosample_accession=be.biosample_accession AND
                         bs.planned_visit_accession=pv.planned_visit_accession AND
                         be.expsample_accession=er.expsample_accession AND
                         er.repository_name='GEO'
                       ORDER BY bs.subject_accession", sep = "")
    
    geo_df <- DBI::dbGetQuery(conn, statement = sql_stmt)
    if (nrow(geo_df) > 0) {
      geo_df <- mutate(geo_df, dataset_id = paste("urn:lsid:", "www.ncbi.nlm.nih.gov/geo", ":GEO:", dataset_id))
    }

    sql_stmt <- paste("SELECT distinct
                        bs.study_accession,
                        bs.subject_accession,
                        cast(0 as UNSIGNED INTEGER) as result_id,
                        fi.name as dataset_id,
                        ex.title,
                        ex.purpose,
                        ex.measurement_technique,
                        bs.biosample_accession, 
                        bs.type,
                        bs.subtype,
                         pv.visit_name,
                        bs.study_time_collected,
                        bs.study_time_collected_unit,
                        bs.study_time_t0_event,
                        bs.study_time_t0_event_specify
                      FROM 
                        experiment ex,
                        biosample bs,
                         planned_visit pv,
                        biosample_2_expsample be,
                        expsample_2_file_info es2fi, 
                        file_info fi                      
                      WHERE 
                        ex.study_accession in ('", study_id, "') AND 
                        be.experiment_accession=ex.experiment_accession AND
                        bs.biosample_accession=be.biosample_accession AND
                        bs.planned_visit_accession=pv.planned_visit_accession AND
                        be.expsample_accession=es2fi.expsample_accession AND
                        es2fi.data_format like \"%Gene_Expression%\" AND
                        es2fi.file_info_id = fi.file_info_id                      
                      ORDER BY bs.subject_accession", sep = "")
    
    file_df <- DBI::dbGetQuery(conn, statement = sql_stmt)
    arr_df <- rbind(geo_df, file_df)
    
    colnames(arr_df) <- array_cols
    if (nrow(arr_df) > 0) {
        arr_df <- transform(arr_df, result_id = as.integer(result_id))
        arr_dt <- as.data.table(arr_df)
        if (is.data.table(arr_dt) == TRUE) {
          arr_dt[, `:=`(result_id, seq_len(.N)), by = "subject_id"]
        }
        arr_df <- as.data.frame(arr_dt)

        arr_df <- ddply(arr_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
                          covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
                                                            unit_of_study_time_of_specimen_collection))
        
        arr_df <- ddply(arr_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
                          getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
        
    }
    
    
    cat("done", "\n")
    
    arr_df
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
