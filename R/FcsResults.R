
fcs_cols <- c("study_id", "subject_id", "sequence",
                      "experiment_title", "assay_purpose", "measurement_technique",
                      "experiment_sample_accession",
                      "biosample_accession", "specimen_type", "specimen_subtype",
                      "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                      "study_time_t0_event", "study_time_t0_event_specify",
                      "file_name")

far_cols <- c("experiment_sample_accession",
                      "base_parent_population", 
                      "population_cell_number", "population_cell_number_unit",
                      "population_defnition_reported", "population_name_reported")

getFcsResults <- function(conn,study_id, measurement_types) {
  cat("loading FCS Results data....")
  
#   old <- options(stringsAsFactors = FALSE)
#   on.exit(options(old), add = TRUE)
#   options(useFancyQuotes = FALSE)
#   measurement_types <- paste(mapply(sQuote, measurement_types), collapse=", ")
  
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
                      bs.study_time_collected,
                      bs.study_time_collected_unit,
                      bs.study_time_t0_event,
                      bs.study_time_t0_event_specify,
                      fi.name
                    
                    FROM  
                      biosample bs
                    INNER JOIN
                      biosample_2_expsample bs2es ON bs.biosample_accession=bs2es.biosample_accession
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
    sql_stmt <- paste("
                      SELECT distinct
                      far.expsample_accession,
                      far.base_parent_population, 
                      far.population_cell_number, 
                      far.population_cell_number_unit,
                      far.population_defnition_reported, 
                      far.population_name_reported
                      
                      FROM  
                      fcs_analyzed_result far
                      WHERE 
                      far.study_accession in (\'", study_id,"\')",sep="")
    
    
    far_df <- dbGetQuery(conn,statement=sql_stmt)
    colnames(far_df) <- far_cols 
    fcs_df <- merge(fcs_df ,far_df, by=c("experiment_sample_accession"), all=TRUE)
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