
concomitant_medications_column_names <- c("study_id", "subject_id", "reported_name_of_medication","category_for_medication",
                           "dose", "dose_description", "dose_units",
                           "dosing_frequency_per_interval", "route of administration",
                           "start_date_time_of_medication", "end_date_time_of_medication", 
                           "study_day_of_start_of_medication", "study_day_of_end_of_medication")

getConcomitantMedications <- function(conn,study_id) {
  cat("loading Concomitant Medications data....")
  
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
                    WHERE sub.study_accession in (\'", study_id,"\') AND sub.compound_role='Concomitant Medication'
                    ORDER BY sub.subject_accession",sep="")
  
  concomitant_medications <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(concomitant_medications) > 0)
    colnames(concomitant_medications) <- concomitant_medications_column_names 
  
  cat("done", "\n")
  concomitant_medications
}