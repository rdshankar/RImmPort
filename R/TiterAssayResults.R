#' Titer Assay Results Domain
#' 
#' @name Titer Assay Results Domain
#' @description The ImmPort study data generated from assays of types: HAI and Neut Ab Titer are grouped into the 
#' Titer Assay Results Domain. The data is reformated to a custom Titer Assay Results domain model in 
#' CDISC SDTM standards, and is a list of 2 data frames containing 1) Titer Assay Results data \code{\link{ZD}} 
#' and 2) any supplemental Titer Assay Results data \code{\link{SUPP}}
NULL
#> NULL 


# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("subject_id", "sequence", "experiment_title", "assay_purpose", "measurement_technique",
                  "virus_strain", "result_in_original_units", "original_units",
                  "specimen_type", "specimen_subtype", "specimen_treatment", 
                  "treatment_amount_value", "treatment_amount_unit",
                  "treatment_duration_value", "treatment_duration_unit",
                  "treatment_temperature_value", "treatment_temperature_unit",
                  "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                  "elapsed_time_of_specimen_collection", "time_point_reference",
                  "biosample_accession", "ZDSPECSB", "ZDREFIDP",
                  "VISITMIN", "VISITMAX",
                  "ZDSPTRT", 
                  "ZDTRTAMV", "ZDTRTAMU",
                  "ZDTRTDUV", "ZDTRTDUU",
                  "ZDTRTTMV", "ZDTRTTMU"))

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
#' @importFrom plyr rename
getTiterAssayResults <- function(data_src, study_id, assay_type="ALL") {
  cat("loading Titer Assay Results data....")
  
  ta_df = data.frame()
  suppta_df = data.frame()
  
  ta_cols <- c("STUDYID", "DOMAIN", "USUBJID", "ZDSEQ", "ZDTEST", "ZDCAT", "ZDMETHOD", "ZDSTRAIN", "ZDORRES", 
               "ZDORRESU",  "ZDSPEC", "ZDREFID", "VISITNUM", "VISIT", "ZDELTM", "ZDTPTREF")
  
  suppta_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
  
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    if ((assay_type == "ALL") || (assay_type =="HAI")) {
      # get HAI results
      
      #   hai_assay_column_names <- c("study_id", "subject_id", "sequence",
      #                               "result_in_original_units", "original_units", 
      #                               "experiment_title", "assay_purpose", "measurement_technique",
      #                               "biosample_accession", "specimen_type", "specimen_subtype",
      #                               "elapsed_time_of_specimen_collection", "time_point_reference",
      #                               "study_time_t0_event", "study_time_t0_event_specify",
      #                               "virus_strain")  
  
      hai_df <- getHaiResults(data_src, study_id, "")
      if (nrow(hai_df) > 0) {
        hai_df <- select(hai_df, STUDYID = study_id, USUBJID = subject_id, ZDSEQ = sequence, ZDTEST = experiment_title, 
                          ZDCAT = assay_purpose, ZDMETHOD = measurement_technique, ZDSTRAIN = virus_strain, 
                          ZDORRES = result_in_original_units, ZDORRESU = original_units,  
                          ZDSPEC = specimen_type, ZDSPECSB = specimen_subtype, 
                           ZDSPTRT = specimen_treatment, 
                           ZDTRTAMV = treatment_amount_value, ZDTRTAMU = treatment_amount_unit,
                           ZDTRTDUV = treatment_duration_value, ZDTRTDUU = treatment_duration_unit,
                           ZDTRTTMV = treatment_temperature_value, ZDTRTTMU = treatment_temperature_unit,
                          VISIT = visit_name, VISITNUM = visit_order,  VISITMIN = visit_min_start_day, VISITMAX = visit_max_start_day, 
                          ZDELTM = elapsed_time_of_specimen_collection, ZDTPTREF = time_point_reference, 
                          ZDREFID = experiment_sample_accession, ZDREFIDP = biosample_accession)
        
        hai_df$DOMAIN <- "ZD"
        
        qnam_values = c("ZDSPECSB", "ZDREFIDP",
                        "VISITMIN", "VISITMAX",
                        "ZDSPTRT", 
                        "ZDTRTAMV", "ZDTRTAMU",
                        "ZDTRTDUV", "ZDTRTDUU",
                        "ZDTRTTMV", "ZDTRTTMU")
        qlabel_values= c("Specimen Subtype", "Source Specimen Identifier",
                         "Planned Visit Minimum Start Day", "Planned Visit Maximum Start Day",
                         "Specimen Treatment", 
                         "Specimen Treatment Amount Value", "Specimen Treatment Amount Unit",
                         "Specimen Treatment Duration Value", "Specimen Treatment Duration Unit", 
                         "Specimen Treatment Temperature Value", "Specimen Treatment Temperature Unit")
        
        supphai_df <- melt(hai_df, 
                           id = c("STUDYID", "DOMAIN", "USUBJID", "ZDSEQ"), 
                           measure = qnam_values, 
                           variable.name = "QNAM", 
                           value.name = "QVAL")
        
        supphai_df <- transform(supphai_df, QLABEL = unlist(qlabel_values[QNAM]))
        supphai_df <- plyr::rename(supphai_df, c("DOMAIN" = "RDOMAIN", "ZDSEQ" = "IDVARVAL"))
        supphai_df$IDVAR <- "ZDSEQ"
        
        
        supphai_df <- supphai_df[suppta_cols]
        
        # remove rows that have empty QVAL values
        supphai_df <- subset(supphai_df,QVAL!="")      
        
        hai_df <- subset(hai_df, select = -c(ZDSPECSB, ZDREFIDP,
                                             VISITMIN, VISITMAX,
                                             ZDSPTRT, 
                                             ZDTRTAMV, ZDTRTAMU,
                                             ZDTRTDUV, ZDTRTDUU,
                                             ZDTRTTMV, ZDTRTTMU))
                
        hai_df <- hai_df[, ta_cols]
    
        ta_df <- rbind(ta_df, hai_df)
        suppta_df <- rbind(suppta_df, supphai_df)
        
      }
    }
    
    if ((assay_type == "ALL") || (assay_type =="Neut Ab Titer")) {
      # get Neut Ab Titer results
      
      nat_df <- getNeutAbTiterResults(data_src, study_id, "")
      if (nrow(nat_df) > 0) {
        nat_df <- select(nat_df, STUDYID = study_id, USUBJID = subject_id, ZDSEQ = sequence, ZDTEST = experiment_title, 
                         ZDCAT = assay_purpose, ZDMETHOD = measurement_technique, ZDSTRAIN = virus_strain, 
                         ZDORRES = result_in_original_units, ZDORRESU = original_units,  
                         ZDSPEC = specimen_type, ZDSPECSB = specimen_subtype, 
                         ZDSPTRT = specimen_treatment, 
                         ZDTRTAMV = treatment_amount_value, ZDTRTAMU = treatment_amount_unit,
                         ZDTRTDUV = treatment_duration_value, ZDTRTDUU = treatment_duration_unit,
                         ZDTRTTMV = treatment_temperature_value, ZDTRTTMU = treatment_temperature_unit,
                         VISIT = visit_name, VISITNUM = visit_order,  VISITMIN = visit_min_start_day, VISITMAX = visit_max_start_day, 
                         ZDELTM = elapsed_time_of_specimen_collection, ZDTPTREF = time_point_reference, 
                         ZDREFID = experiment_sample_accession, ZDREFIDP = biosample_accession)
        
        nat_df$DOMAIN <- "ZD"

        qnam_values = c("ZDSPECSB", "ZDREFIDP",
                        "VISITMIN", "VISITMAX",
                        "ZDSPTRT", 
                        "ZDTRTAMV", "ZDTRTAMU",
                        "ZDTRTDUV", "ZDTRTDUU",
                        "ZDTRTTMV", "ZDTRTTMU")
        qlabel_values= c("Specimen Subtype", "Source Specimen Identifier",
                         "Planned Visit Minimum Start Day", "Planned Visit Maximum Start Day",
                         "Specimen Treatment", 
                         "Specimen Treatment Amount Value", "Specimen Treatment Amount Unit",
                         "Specimen Treatment Duration Value", "Specimen Treatment Duration Unit", 
                         "Specimen Treatment Temperature Value", "Specimen Treatment Temperature Unit")
        
        suppnat_df <- melt(nat_df, 
                           id = c("STUDYID", "DOMAIN", "USUBJID", "ZDSEQ"), 
                           measure = qnam_values, 
                           variable.name = "QNAM", 
                           value.name = "QVAL")
        
        suppnat_df <- transform(suppnat_df, QLABEL = unlist(qlabel_values[QNAM]))
        suppnat_df <- plyr::rename(suppnat_df, c("DOMAIN" = "RDOMAIN", "ZDSEQ" = "IDVARVAL"))
        suppnat_df$IDVAR <- "ZDSEQ"
        
        
        suppnat_df <- suppnat_df[suppta_cols]
        
        # remove rows that have empty QVAL values
        suppnat_df <- subset(suppnat_df,QVAL!="")      
        
        nat_df <- subset(nat_df, select = -c(ZDSPECSB, ZDREFIDP,
                                             VISITMIN, VISITMAX,
                                             ZDSPTRT, 
                                             ZDTRTAMV, ZDTRTAMU,
                                             ZDTRTDUV, ZDTRTDUU,
                                             ZDTRTTMV, ZDTRTTMU))
        
        nat_df <- nat_df[, ta_cols]
        
        ta_df <- rbind(ta_df, nat_df)
        suppta_df <- rbind(suppta_df, suppnat_df)
        
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
##'     ZDREFID \tab Specimen Identifier \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     ZDELTM \tab Planned Elapsed Time from Time Point Ref \cr
##'     ZDTPTREF \tab Time Point Reference \cr
##'     ZDXFN \tab Raw Data File or Life Science Identifier
##'   }
##' }
NULL
#> NULL 

##' Titer Assay Results Domain Supplemental Variables
##' @name SUPPZD
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name} \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     RDOMAIN  \tab Related Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     IDVAR \tab Identifying Variable \cr
##'     IDVARVAL \tab Identifying Variable Value \cr
##'     QNAM \tab Qualifier Variable Name \cr
##'     QLABEL \tab Qualifier Variable Label \cr
##'     QVAL \tab Data Value
##'   }
##' }
##' @note The following table enumerates the values in QNAM and QLABEL variables {
##'   \tabular{ll}{
##'     \strong{QNAM} \tab \strong{QLABEL} \cr
##'     ZDSPECSB \tab Specimen Subtype \cr
##'     ZDREFIDP \tab Source Specimen Identifier \cr
##'     VISITMIN \tab Planned Visit Minimum Start Day \cr
##'     VISITMAX \tab Planned Visit Maximum Start Day \cr
##'     ZDSPTRT \tab Specimen Treatment \cr
##'     ZDTRTAMV \tab Specimen Treatment Amount Value \cr
##'     ZDTRTAMU \tab Specimen Treatment Amount Unit \cr
##'     ZDTRTDUV \tab Specimen Treatment Duration Value \cr
##'     ZDTRTDUU \tab Specimen Treatment Duration Unit \cr
##'     ZDTRTTMV \tab Specimen Treatment Temperature Value \cr
##'     ZDTRTTMU \tab Specimen Treatment Temperature Unit
##'   }
##' }
NULL
#> NULL


