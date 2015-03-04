
exposure_column_names <- c("study_id", "subject_id", "name_of_treatment","category_of_treatment",
                                  "dose", "dose_description", "dose_units",
                                  "dosing_frequency_per_interval", "route of administration",
                                  "start_date_time_of_treatment", "end_date_time_of_treatment", 
                                  "study_day_of_start_of_treatment", "study_day_of_end_of_treatment")

getExposure <- function(conn,study_id) {
  cat("loading Exposure data....")
  
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
                    WHERE sub.study_accession in (\'", study_id,"\') AND sub.compound_role='Intervention'
                    ORDER BY sub.subject_accession",sep="")
  
  exposure <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(exposure) > 0)
    colnames(exposure) <- exposure_column_names 

  cat("done", "\n")
  
  exposure
}