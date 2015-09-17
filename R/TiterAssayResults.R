#' Titer Assay Results Domain
#' 
#' @name Titer Assay Results Domain
#' @description The ImmPort study data generated from assays of types: HAI and Neut Ab Titer are grouped into the 
#' Titer Assay Results Domain. The data is reformated to a custom Titer Assay Results domain model in 
#' CDISC SDTM standards, and is a list of 2 data frames containing 1) Cellular Quantification data \code{\link{ZD}} 
#' and 2) any supplemental Cellular Quantification data \code{\link{SUPP}}
NULL
#> NULL 

ta_cols <- c("STUDYID", "DOMAIN", "USUBJID", "ZDSEQ", "ZDTEST", "ZDCAT", "ZDMETHOD", "ZDSTRAIN", "ZDORRES", 
                     "ZDORRESU",  "ZDSPEC", "ZDSPECSB", "ZDREFID", "ZDSDY", "ZDDYU")

suppta_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("subject_id", "result_id", "experiment_title", "assay_purpose", "measurement_technique",
                  "virus_strain", "result_in_original_units", "original_units",
                  "specimen_type", "specimen_subtype", 
                  "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
                  "biosample_accession"))

# Get Titer Assay Results data of a specific study
# 
# The function \code{getTiterAssayResults} queries the ImmPort database for Titer Assay Results data and 
# reformats it to a custom Titer Assay Results domain model in CDISC SDTM standards. 
# 
# @param data_src A data_srcection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param assay_type Assay Type **optional**
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Titer Assay Results data \code{\link{ZD}} and 2) any supplemental 
#   Titer Assay Results data \code{\link{SUPP}}
# @examples
# \dontrun{
#   # get all of study SDY1's Titer Assay Results data
#   ta_l <- getTiterAssayResults(data_src, "SDY1")
#   
#   # get study SDY1's Titer Assay data that was generated using HAI assay
#   ta_l <- getTiterAssayResults(data_src, "SDY1", "HAI")
# }
getTiterAssayResults <- function(data_src, study_id, assay_type="ALL") {
  cat("loading Titer Assay Results data....")
  
  ta_df = data.frame()
  suppta_df = data.frame()

  
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    if ((assay_type == "ALL") || (assay_type =="HAI")) {
      # get HAI results
      
      #   hai_assay_column_names <- c("study_id", "subject_id", "result_id",
      #                               "result_in_original_units", "original_units", 
      #                               "experiment_title", "assay_purpose", "measurement_technique",
      #                               "biosample_accession", "specimen_type", "specimen_subtype",
      #                               "study_time_of_specimen_collection", "unit_of_study_time_of_specimen_collection",
      #                               "study_time_t0_event", "study_time_t0_event_specify",
      #                               "virus_strain")  
  
      hai.df <- getHaiResults(data_src, study_id, "")
      if (nrow(hai.df) > 0) {
        hai.df <- select(hai.df, STUDYID = study_id, USUBJID = subject_id, ZDSEQ = result_id, ZDTEST = experiment_title, 
                          ZDCAT = assay_purpose, ZDMETHOD = measurement_technique, ZDSTRAIN = virus_strain, 
                          ZDORRES = result_in_original_units, ZDORRESU = original_units,  
                          ZDSPEC = specimen_type, ZDSPECSB = specimen_subtype, 
                          ZDSDY = study_time_of_specimen_collection, ZDDYU = unit_of_study_time_of_specimen_collection, 
                          ZDREFID = biosample_accession)
        
        hai.df$DOMAIN <- "ZD"
        
        hai.df <- hai.df[, c("STUDYID", "DOMAIN", "USUBJID", "ZDSEQ", "ZDTEST", "ZDCAT", "ZDMETHOD", "ZDSTRAIN", "ZDORRES", 
                               "ZDORRESU",  "ZDSPEC", "ZDSPECSB", "ZDREFID", "ZDSDY", "ZDDYU" )]
    
        ta_df <- rbind(ta_df, hai.df)
        
      }
    }
    
    if ((assay_type == "ALL") || (assay_type =="Neut Ab Titer")) {
      # get Neut Ab Titer results
      
      nat.df <- getNeutAbTiterResults(data_src, study_id, "")
      if (nrow(nat.df) > 0) {
        nat.df <- select(nat.df, STUDYID = study_id, USUBJID = subject_id, ZDSEQ = result_id, ZDTEST = experiment_title, 
                         ZDCAT = assay_purpose, ZDMETHOD = measurement_technique, ZDSTRAIN = virus_strain, 
                         ZDORRES = result_in_original_units, ZDORRESU = original_units,  
                         ZDSPEC = specimen_type, ZDSPECSB = specimen_subtype, 
                         ZDSDY = study_time_of_specimen_collection, ZDDYU = unit_of_study_time_of_specimen_collection, 
                        ZDREFID = biosample_accession)
        
        nat.df$DOMAIN <- "ZD"
        
        nat.df <- nat.df[, c("STUDYID", "DOMAIN", "USUBJID", "ZDSEQ", "ZDTEST", "ZDCAT", "ZDMETHOD", "ZDSTRAIN", "ZDORRES", 
                             "ZDORRESU",  "ZDSPEC", "ZDSPECSB", "ZDREFID", "ZDSDY", "ZDDYU")]
        
        ta_df <- rbind(ta_df, nat.df)
        
        
      }
    }
  } else {
    l <- loadSerializedStudyData(data_src, study_id, "Titer Assay Results")
    if (assay_type == "ALL") {
      ta_df <- l[[1]]
      suppta_df <- l[[2]]
    } else {
      
    }
  }   
  
  cat("done", "\n")
  
  ta_l <- list()
  if (nrow(ta_df) > 0)
    ta_l <- list(zd_df=ta_df, suppzd_df=suppta_df)
  
  ta_l
  
} 

# Get count of Titer Assay Results data of a specific study
# 
# The function \code{getCountOfTiterAssayResults} queries the ImmPort database for count 
# of Titer Assay Results data 
# 
# @param data_src A data_srcection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param assay_type Assay Type **optional**
# @param study_id Identifier of a specific study
# @return a count of Titer Assay Results data 
# @examples
# \dontrun{
#   # get count of all of study SDY1's Titer Assay Results data
#   count <- getCountOfTiterAssayResults(data_src, "SDY1")
#   
#   # get count of study SDY1's Titer Assay Results data that was generated using HAI assay
#   count <- getCountOfTiterAssayResults(data_src, "SDY1", "HAI")
# }
getCountOfTiterAssayResults <- function(data_src, study_id, assay_type="ALL") {
  count <- 0
  if ((assay_type == "ALL") || (assay_type =="HAI")) 
    count <- count + getCountOfHaiResults(data_src, study_id)
  if ((assay_type == "ALL") || (assay_type =="Neut Ab Titer")) 
    count <- count + getCountOfNeutAbTiterResults(data_src, study_id)
  
  count
}

##' Titer Assay Results Domain Variables
##' @name ZD
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     ZDSEQ \tab Sequence Number \cr
##'     ZDTEST \tab Titer Assay Results Test Name \cr
##'     ZDCAT \tab Category for Titer Assay Results \cr
##'     ZDMETHOD \tab Measurement Technique \cr
##'     ZDPOPDEF \tab Cell Population Definition \cr
##'     ZDPOPNAM \tab Cell Population Name \cr
##'     ZDORRES \tab Result or Finding in Original Units \cr
##'     ZDORRESU \tab Original Units \cr
##'     ZDBASPOP \tab Base Parent Population \cr
##'     ZDSPEC \tab Specimen Type \cr
##'     ZDSPECSB \tab Specimen Subtype \cr
##'     ZDREFID \tab Specimen Identifier \cr
##'     ZDDY \tab Study Day of Specimen Collection \cr
##'     ZDDYU \tab Units of Study Day of Specimen Collection \cr
##'     ZDXFN \tab Raw Data File or Life Science Identifier
##'   }
##' }
NULL
#> NULL 

