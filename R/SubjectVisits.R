
subject_visits_column_names <- c("study_id", "subject_id","visit_number", "visit_name", "planned_study_day_of_visit", 
                                 "start_datetime_of_visit", "end_datetime_of_visit", "study_day_of_start_of_visit", 
                                 "study_day_of_end_of_visit", "description_of_unplanned_visit")

getSubjectVisits <- function(conn,study_id) {
  cat("loading Subject Visits data....")
  
  subjectVisitsSQL <- paste("
                           SELECT distinct
                            av.study_accession,
                            av.subject_accession,
                            av.study_day_visit_starts,
                            pv.visit_name,
                            \"\", \"\", \"\", \"\", \"\", \"\"
                           FROM actual_visit av,
                           planned_visit pv
                           WHERE av.planned_visit_accession = pv.planned_visit_accession
                           AND av.study_accession in (\'", study_id,"\')
                           ORDER BY av.subject_accession",sep="")

    subject_visits <- dbGetQuery(conn,statement=subjectVisitsSQL)
    if (nrow(subject_visits) > 0)
      colnames(subject_visits) <- subject_visits_column_names 

    cat("done", "\n")  
  
    subject_visits
}