
protocol_deviations_column_names <- c("study_id", "subject_id", "protocol_deviation_term","related_to_adverse_event",
                           "reason_for_deviation", "resolution_for_deviation", "subject_continued_study",
                           "study_day_of_start_of_deviation", "study_day_of_end_of_deviation")

getProtocolDeviations <- function(conn,study_id) {
  cat("loading Protocol Deviations data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                        pd.study_accession,
                        pd.subject_accession,
                        pd.deviation_description,
                        pd.is_adverse_event_related,
                        pd.reason_for_deviation,
                        pd.resolution_of_deviation,
                        pd.subject_continued_study,
                        pd.deviation_study_start_day,
                        pd.deviation_study_end_day
                    FROM  protocol_deviation pd
                    WHERE pd.study_accession in (\'", study_id,"\')
                    ORDER BY pd.subject_accession",sep="")
  
  protocol_deviations <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(protocol_deviations) > 0)
    colnames(protocol_deviations) <- protocol_deviations_column_names 
  cat("done", "\n")
  
  protocol_deviations
}