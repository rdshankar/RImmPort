
# adverse_events_column_names <- c("STUDYID", "DOMAIN", "USUBJID", "AESPID", "AETERM", "AEMODIFY", 
#                                  "AEBODYSYS", "AELOC", "AESEV", "AESER",
#                                  "AEACN", "AEACNOTH", "AEREL", 
#                                  "AERELNST", "AEOUT",
#                                  "AESTDTC", "AEENDTC", "AESTDY", "AEENDY")

adverse_events_column_names <- c("study_id", "domain", "subject_id", "sponsor_defined_id", "reported_term", "modified_reported_term", 
                                 "body_system_or_organ_class", "location_of_event", "severity_intensity", "serious_event",
                                 "action_taken_with_study_treatment", "other_action_taken", "causality", 
                                 "relationship_to_nonstudy_treatment", "outcome_of_adverse_event",
                                 "start_date_time", "end_date_time", "start_study_day", "end_study_day")

getAdverseEvents <- function(conn,study_id) {
  cat("loading Adverse Events data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    ae.study_accession,
                    \"AE\" as domain,
                    ae.subject_accession,
                    ae.adverse_event_accession,
                    ae.name_reported,
                    ae.name_preferred,
                    ae.organ_or_body_system_reported,
                    ae.location_of_reaction_reported,
                    ae.severity_reported,
                    ae.is_serious_event,
                    ae.study_treatment_action_taken,
                    ae.other_action_taken,
                    ae.relation_to_study_treatment,
                    ae.relation_to_nonstudy_treatment,
                    ae.outcome_reported,
                    ae.start_time,
                    ae.end_time,
                    ae.start_study_day,
                    ae.end_study_day
                    FROM adverse_event ae
                    WHERE ae.study_accession in (\'", study_id,"\')
                    ORDER BY ae.subject_accession",sep="")
  
  adverse_events <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(adverse_events) > 0)
    colnames(adverse_events) <- adverse_events_column_names 
  cat("done", "\n")
  
  adverse_events
}