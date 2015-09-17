
array_cols <- c("study_id", "subject_id", "result_id", "repository_name", "repository_id", 
                        "experiment_title", "assay_purpose", "measurement_technique",
                        "biosample_accession", "specimen_type", "specimen_subtype", 
                        "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection")

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
                         er.repository_name,
                         er.repository_accession,
                         ex.title,
                         ex.purpose,
                         ex.measurement_technique,
                         bs.biosample_accession, 
                         bs.type,
                         bs.subtype,
                         bs.study_time_collected,
                         bs.study_time_collected_unit
                       FROM experiment ex,
                         biosample bs,\
                         biosample_2_expsample be,
                         expsample_public_repository er
                       WHERE ex.study_accession in ('", study_id, "') AND 
                         be.experiment_accession=ex.experiment_accession AND
                         bs.biosample_accession=be.biosample_accession AND
                         be.expsample_accession=er.expsample_accession AND
                         er.repository_name='GEO'
                       ORDER BY bs.subject_accession", sep = "")
    
    arr_df <- DBI::dbGetQuery(conn, statement = sql_stmt)
    colnames(arr_df) <- array_cols
    if (nrow(arr_df) > 0) {
        arr_df <- transform(arr_df, result_id = as.integer(result_id))
        arr_dt <- as.data.table(arr_df)
        if (is.data.table(arr_dt) == TRUE) {
          arr_dt[, `:=`(result_id, seq_len(.N)), by = "subject_id"]
        }
        arr_df <- as.data.frame(arr_dt)
            
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
    
    count[1, 1]
    
} 
