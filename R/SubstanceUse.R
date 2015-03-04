
substance_use_column_names <- c("study_id", "subject_id", "reported_name_of_substance","category_of_substance_use",
                           "dose", "dose_description", "dose_units",
                           "dosing_frequency_per_interval", "route_of_administration",
                           "start_date_time_of_substance_use", "end_date_time_of_substance_use", 
                           "study_day_of_start_of_substance_use", "study_day_of_end_of_substance_use")

getSubstanceUse <- function(conn,study_id) {
  cat("loading Substance Use data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    sub.study_accession,
                    sub.subject_accession,
                    sub.compound_name_reported,
                    sub.merge_name_reported,
                    sub.dose,
                    sub.dose_reported,
                    sub.dose_units,
                    sub.dose_freq_per_interval,
                    sub.route_of_admin_reported,
                    sub.start_time,
                    sub.end_time,
                    sub.start_day,                   
                    sub.end_day
                    FROM  substance_merge sub
                    WHERE sub.study_accession in (\'", study_id,"\') AND sub.compound_role='Substance Use'
                    ORDER BY sub.subject_accession",sep="")
  
  substance_use <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(substance_use) > 0)
    colnames(substance_use) <- substance_use_column_names 

  cat("done", "\n")
  
  substance_use
}