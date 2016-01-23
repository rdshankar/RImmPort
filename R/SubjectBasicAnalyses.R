

getSubjectBasicAnalyses <- function(conn, study_id) {
    cat("loading Subject Basic Analyses data....")

  subject_basic_analyses_column_names <- c("study_id", "subject_id", "visit_num", "parameter", "parameter_code", "parameter_id", 
                                           "analysis_value", "analysis_timepoint", "outcome_type")
  
    sql_stmt <- paste("SELECT distinct
                        smr.study_accession,
                        smr.subject_accession,
                        smr.study_day,
                        smd.description,
                        smd.title,
                        smd.subject_measure_accession,
                        smr.datavalue,
                        smd.timeframe,
                        smd.outcometype
                      FROM subject_measure_result smr,
                        subject_measure_definition smd
                      WHERE smr.subject_measure_accession = smd.subject_measure_accession AND 
                        smr.study_accession in ('", study_id, "')
                      ORDER BY smr.subject_accession", sep = "")
    
    subject_basic_analyses <- dbGetQuery(conn, statement = sql_stmt)
    if (nrow(subject_basic_analyses) > 0) 
        colnames(subject_basic_analyses) <- subject_basic_analyses_column_names
    
    cat("done", "\n")
    subject_basic_analyses
}

getCountOfSubjectBasicAnalyses <- function(conn, study_id) {
    sql_stmt <- paste("SELECT count(*)
                      FROM subject_measure_result smr,
                        subject_measure_definition smd
                      WHERE smr.subject_measure_accession = smd.subject_measure_accession AND 
                        smr.study_accession in ('", study_id, "')", sep = "")
    
    count <- dbGetQuery(conn, statement = sql_stmt)
    
    count[1, 1]
} 
