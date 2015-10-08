
elispot_cols <- c("study_id", "subject_id", "result_id",
                                "analyte", "comments", 
                                "cell_number", "cell_type", "spot_number", 
                                "experiment_title", "assay_purpose", "measurement_technique",
                                "biosample_accession", "specimen_type", "specimen_subtype",
                                "visit_name", "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                                "study_time_t0_event", "study_time_t0_event_specify",
                                "file_name")

getElispotResults <- function(conn,study_id, measurement_types) {
  cat("loading ELISPOT Results data....")
  
#   old <- options(stringsAsFactors = FALSE)
#   on.exit(options(old), add = TRUE)
#   options(useFancyQuotes = FALSE)
#   measurement_types <- paste(mapply(sQuote, measurement_types), collapse=", ")
  
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
                    bs.biosample_accession,
                    bs.type,
                    bs.subtype,
                    pv.visit_name,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify,
                    fi.name
                    FROM  
                      elispot_result elp
					          INNER JOIN
						          experiment ex ON elp.experiment_accession=ex.experiment_accession
					          INNER JOIN
						          biosample bs ON elp.biosample_accession=bs.biosample_accession
                    INNER JOIN
                      planned_visit pv ON bs.planned_visit_accession=pv.planned_visit_accession
                    LEFT OUTER JOIN
                      expsample_2_file_info es2fi ON elp.expsample_accession=es2fi.expsample_accession
                    LEFT OUTER JOIN
                      file_info fi ON es2fi.file_info_id=fi.file_info_id
                    WHERE elp.study_accession in (\'", study_id,"\')  
                    ORDER BY elp.subject_accession",sep="")
  
  elispot_df <- dbGetQuery(conn,statement=sql_stmt)

  if (nrow(elispot_df) > 0) {
    colnames(elispot_df) <- elispot_cols 

    elispot_df <- ddply(elispot_df, .(study_id, subject_id, result_id), mutate, elapsed_time_of_specimen_collection = 
                      covertElaspsedTimeToISO8601Format(study_time_of_specimen_collection, 
                                                        unit_of_study_time_of_specimen_collection))
    
    elispot_df <- ddply(elispot_df, .(study_id, subject_id, result_id), mutate, time_point_reference = 
                      getTimePointReference(study_time_t0_event, study_time_t0_event_specify))
    
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
                    LEFT OUTER JOIN
                      expsample_2_file_info es2fi ON elp.expsample_accession=es2fi.expsample_accession
                    LEFT OUTER JOIN
                      file_info fi ON es2fi.file_info_id=fi.file_info_id
                    WHERE elp.study_accession in (\'", study_id,"\')", sep="")
  
  count <- dbGetQuery(conn,statement=sql_stmt)
  
  count[1,1]
}