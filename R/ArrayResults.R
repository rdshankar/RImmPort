#library(GEOquery)

array_column_names <- c("study_id", "subject_id", 
                        "geo_id", "assay_purpose",
                        "speciment_name", "specimen_type", "specimen_subtype",
                        "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                        "study_time_t0_event", "study_time_t0_event_specify")

# array_column_names <- c("study_id", "subject_id", 
#                         "geo_id", "assay_purpose",
#                         "speciment_name", "specimen_type", "specimen_subtype",
#                         "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
#                         "study_time_t0_event", "study_time_t0_event_specify", 
#                         "channel_count", "data_row_count", 
#                         "label_ch1", "label_ch2", 
#                         "molecule_ch1", "moloecule_ch2", 
#                         "organism_ch1", "organism_ch2")

getArrayResults <- function(conn,study_id) {
  cat("loading Array Results data....")
  
  sql_stmt <- paste("
                    SELECT distinct
                    bs.study_accession,
                    bs.subject_accession,
                    er.repository_accession,
                    ex.purpose,
                    bs.name,
                    bs.type,
                    bs.subtype,
                    bs.study_time_collected,
                    bs.study_time_collected_unit,
                    bs.study_time_t0_event,
                    bs.study_time_t0_event_specify
                    FROM experiment ex,
                    biosample bs,
                    biosample_2_expsample be,
                    expsample_public_repository er
                    WHERE ex.study_accession in (\'", study_id,"\') AND 
                    be.experiment_accession=ex.experiment_accession AND
                    bs.biosample_accession=be.biosample_accession AND
                    be.expsample_accession=er.expsample_accession AND
                    er.repository_name='GEO'
                    ORDER BY bs.subject_accession",sep="")
  
  array <- dbGetQuery(conn,statement=sql_stmt)
  if (nrow(array) > 0) {
    colnames(array) <- array_column_names 
  
# get GEO metadata

#     for(i in 1:nrow(array)) {
#       row <- array[i,]
#       if ((regexpr("GSM", row$geo_id)[1]) == 1) {
#         gsm <- getGEO(row$geo_id)
#         meta <- Meta(gsm)
#         array[i,]$channel_count <- meta$channel_count
#         array[i,]$data_row_count <- meta$data_row_count
#         if (meta$channel_count == 1) {
#           array[i,]$label_ch1 <- meta$label_ch1
#           array[i,]$molecule_ch1 <- meta$molecule_ch1
#           array[i,]$organism_ch1 <- meta$organism_ch1
#         }
#         if (meta$channel_count == 2) {
#           array[i,]$label_ch1 <- meta$label_ch1
#           array[i,]$molecule_ch1 <- meta$molecule_ch1
#           array[i,]$organism_ch1 <- meta$organism_ch1
#           array[i,]$label_ch2 <- meta$label_ch2
#           array[i,]$molecule_ch2 <- meta$molecule_ch2
#           array[i,]$organism_ch2 <- meta$organism_ch2
#         }
#       }
#     }
   }

  
  cat("done", "\n")
  
  array
}