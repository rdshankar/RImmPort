#' Genetics Findings Domain
#' 
#' @name Genetics Findings Domain
#' @description The ImmPort study data generated from assays of types: HLA Typing and Array are grouped into the 
#' Genetics Findings Domain. The data is reformated to  a PharmacoGenomics and Genetics Findings (PF) model in 
#' CDISC SDTM standards, and is a list of 2 data frames containing 1) Genetics Findings data \code{\link{PF}} 
#' and 2) any supplemental Genetics Findings data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("subject_id", "result_id", "result_set_id", "experiment_title", "assay_purpose", "measurement_technique",
                  "locus_name", "allele_1", "allele_2",
                  "pop_area_name", "specimen_type", "specimen_subtype",
                  "specimen_treatment", 
                  "treatment_amount_value", "treatment_amount_unit",
                  "treatment_duration_value", "treatment_duration_unit",
                  "treatment_temperature_value", "treatment_temperature_unit",
                  "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order", 
                  "elapsed_time_of_specimen_collection", "time_point_reference",
                  "biosample_accession", "repository_id", 
                  "QNAM", "QVAL", "PFSPECSB", "PFREFIDP", "PFPOPAR", 
                  "VISITMIN", "VISITMAX",
                  "PFSPTRT", 
                  "PFTRTAMV", "PFTRTAMU",
                  "PFTRTDUV", "PFTRTDUU",
                  "PFTRTTMV", "PFTRTTMU"))

# Get Genetics Findings data of a specific study
# 
# The function \code{getGeneticsFindings} queries the ImmPort database for Genetics Findings data and 
# reformats it to a PharmacoGenomics and Genetics Findings (PF) domain model in CDISC SDTM standards. 
# 
# @param data_src A data_srcection handle to ImmPort (MySQL or SQLite) database instance or 
# a directory handle to folder where study RDS files are located
# @param assay_type Assay Type **optional**
# @param study_id Identifier of a specific study
# @return a list of 2 data frames containing 1) Genetics Findings data \code{\link{PF}} and 2) any supplemental 
#   Genetics Findings data \code{\link{SUPPPF}}
# @examples
# \dontrun{
#   # get all of study SDY1's Genetics Findings data
#   gf_l <- getGeneticsFindings(data_src, "SDY1")
#   
#   # get study SDY1's Genetics Findings data that was generated using Array assay
#   gf_l <- getGeneticsFindings(data_src,  "SDY1", "Array")
# }
#' @importFrom reshape2 melt 
#' @importFrom plyr rename
#' @importFrom dplyr %>% mutate
getGeneticsFindings <- function(data_src, study_id, assay_type="ALL") {
  cat("loading Genetics Findings data....")

  gf_cols <- c("STUDYID", "DOMAIN", "USUBJID", "PFSEQ", "PFGRPID", "PFTEST","PFCAT", "PFMETHOD", "PFGENRI", "PFORRES",  
               "PFALLELC", "PFXFN", "PFSPEC", "PFREFID", "VISITNUM", "VISIT", "PFELTM", "PFTPTREF")
  
  suppgf_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
  
  gf_df = data.frame()
  suppgf_df = data.frame()
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    if ((assay_type == "ALL") || (assay_type =="HLA Typing")) {
      # get HLA Typing results
        
      #   hla_column_names <- c("study_id", "subject_id", "sequence", "result_set_id",
      #                         "allele_1", "allele_2", 
      #                         "locus_name", "pop_area_name", 
      #                         "experiment_title", "assay_purpose", "measurement_technique",
      #                         "biosample_accession", "specimen_type", "specimen_subtype",
      #                         "visit_name", "elapsed_time_of_specimen_collection", "time_point_reference",
      #                         "study_time_t0_event", "study_time_t0_event_specify")
    
      hla_df <- getHlaTypingResults(data_src, study_id, "")
      if (nrow(hla_df) > 0) {
        hla_df <- select(hla_df, STUDYID = study_id, USUBJID = subject_id, PFSEQ = result_id, PFGRPID = result_set_id,
                         PFTEST = experiment_title, PFCAT = assay_purpose, PFMETHOD = measurement_technique, 
                         PFGENRI = locus_name, PFALLEL1 = allele_1, PFALLEL2 = allele_2, PFPOPAR = pop_area_name, 
                         PFSPEC = specimen_type, PFSPECSB = specimen_subtype, 
                         PFSPTRT = specimen_treatment, 
                         PFTRTAMV = treatment_amount_value, PFTRTAMU = treatment_amount_unit,
                         PFTRTDUV = treatment_duration_value, PFTRTDUU = treatment_duration_unit,
                         PFTRTTMV = treatment_temperature_value, PFTRTTMU = treatment_temperature_unit,
                         VISIT = visit_name, VISITNUM = visit_order,  VISITMIN = visit_min_start_day, VISITMAX = visit_max_start_day, 
                         PFELTM = elapsed_time_of_specimen_collection, PFTPTREF = time_point_reference,
                         PFREFID = experiment_sample_accession, PFREFIDP = biosample_accession )
    
        hla_df$DOMAIN <- "PF"
        hla_df$PFXFN <- ""
        
        qnam_values = c("PFSPECSB", "PFREFIDP", "PFPOPAR", 
                        "VISITMIN", "VISITMAX",
                        "PFSPTRT", 
                        "PFTRTAMV", "PFTRTAMU",
                        "PFTRTDUV", "PFTRTDUU",
                        "PFTRTTMV", "PFTRTTMU")
        qlabel_values= c("Specimen Subtype", "Source Specimen Identifier", "Geographic Area of the Population", 
                         "Planned Visit Minimum Start Day", "Planned Visit Maximum Start Day",
                         "Specimen Treatment", 
                         "Specimen Treatment Amount Value", "Specimen Treatment Amount Unit",
                         "Specimen Treatment Duration Value", "Specimen Treatment Duration Unit", 
                         "Specimen Treatment Temperature Value", "Specimen Treatment Temperature Unit")
        
        supphla_df <- melt(hla_df, 
                                    id = c("STUDYID", "DOMAIN", "USUBJID", "PFSEQ"), 
                                    measure = qnam_values, 
                                    variable.name = "QNAM", 
                                    value.name = "QVAL")
        
        supphla_df <- transform(supphla_df, QLABEL = unlist(qlabel_values[QNAM]))
        supphla_df <- plyr::rename(supphla_df, c("DOMAIN" = "RDOMAIN", "PFSEQ" = "IDVARVAL"))
        supphla_df$IDVAR <- "PFSEQ"
        
        
        supphla_df <- supphla_df[suppgf_cols]
        
        # remove rows that have empty QVAL values
        supphla_df <- subset(supphla_df,QVAL!="")      
        
        hla_df <- subset(hla_df, select = -c(PFSPECSB, PFREFIDP, PFPOPAR, 
                                             VISITMIN, VISITMAX,
                                             PFSPTRT, 
                                             PFTRTAMV, PFTRTAMU,
                                             PFTRTDUV, PFTRTDUU,
                                             PFTRTTMV, PFTRTTMU))
        
        hla_df <- melt(hla_df, 
                                     id = c("STUDYID", "DOMAIN", "USUBJID", "PFSEQ", "PFGRPID", "PFTEST", "PFGENRI",  "PFCAT", "PFMETHOD", 
                                            "PFSPEC", "VISITNUM", "VISIT", "PFELTM", "PFTPTREF", "PFREFID", "PFXFN"), 
                                     measure = c("PFALLEL1", "PFALLEL2"), 
                                     variable.name = "PFALLELC", 
                                     value.name = "PFORRES")
        hla_df <- hla_df[with(hla_df, order(PFSEQ)), ]
        rownames(hla_df) <- c()
        hla_df <- hla_df[, gf_cols]
    
        gf_df <- rbind(gf_df, hla_df)
        suppgf_df <- rbind(suppgf_df, supphla_df)
        
      }
    }
    
    if ((assay_type == "ALL") || (assay_type =="Array")) {
      # get Array results
      
      #   array_column_names <- c("study_id", "subject_id", "result_id", "dataset_id", 
      #                           "experiment_title", "assay_purpose", "measurement_technique",
      #                           "biosample_accession", "specimen_type", "specimen_subtype", 
      #                           "visit_name", "elapsed_time_of_specimen_collection", "time_point_reference")
  
      arr_df <- getArrayResults(data_src, study_id, "")
      if (nrow(arr_df) > 0) {
        arr_df <- arr_df %>% 
          select(STUDYID = study_id, USUBJID = subject_id, PFSEQ = sequence, PFXFN = dataset_id,
                         PFTEST = experiment_title, PFCAT = assay_purpose, PFMETHOD = measurement_technique, 
                         PFSPEC = specimen_type, PFSPECSB = specimen_subtype, 
                         PFSPTRT = specimen_treatment, 
                         PFTRTAMV = treatment_amount_value, PFTRTAMU = treatment_amount_unit,
                         PFTRTDUV = treatment_duration_value, PFTRTDUU = treatment_duration_unit,
                         PFTRTTMV = treatment_temperature_value, PFTRTTMU = treatment_temperature_unit,
                         VISIT = visit_name, VISITNUM = visit_order,  VISITMIN = visit_min_start_day, VISITMAX = visit_max_start_day, 
                         PFELTM = elapsed_time_of_specimen_collection, PFTPTREF = time_point_reference,
                         PFREFID = experiment_sample_accession, PFREFIDP = biosample_accession )
        arr_df$DOMAIN <- "PF"
        arr_df$PFGRPID <- ""
        arr_df$PFGENRI <- ""
        arr_df$PFORRES <- ""
        arr_df$PFALLELC <- ""
    
        qnam_values = c("PFSPECSB", "PFREFIDP",
                        "VISITMIN", "VISITMAX",
                        "PFSPTRT", 
                        "PFTRTAMV", "PFTRTAMU",
                        "PFTRTDUV", "PFTRTDUU",
                        "PFTRTTMV", "PFTRTTMU")
        qlabel_values= c("Specimen Subtype", "Source Specimen Identifier", 
                         "Planned Visit Minimum Start Day", "Planned Visit Maximum Start Day",
                         "Specimen Treatment", 
                         "Specimen Treatment Amount Value", "Specimen Treatment Amount Unit",
                         "Specimen Treatment Duration Value", "Specimen Treatment Duration Unit", 
                         "Specimen Treatment Temperature Value", "Specimen Treatment Temperature Unit")
        
        supparr_df <- melt(arr_df, 
                                     id = c("STUDYID", "DOMAIN", "USUBJID", "PFSEQ"), 
                                     measure = qnam_values, 
                                     variable.name = "QNAM", 
                                     value.name = "QVAL")
        
        supparr_df <- transform(supparr_df, QLABEL = unlist(qlabel_values[QNAM]))
        supparr_df <- plyr::rename(supparr_df, c("DOMAIN" = "RDOMAIN", "PFSEQ" = "IDVARVAL"))
        supparr_df$IDVAR <- "PFSEQ"
        
        
        supparr_df <- supparr_df[suppgf_cols]
        
        # remove rows that have empty QVAL values
        supparr_df <- subset(supparr_df,QVAL!="")      
        
        arr_df <- subset(arr_df, select = -c(PFSPECSB, PFREFIDP, PFSPTRT, 
                                             VISITMIN, VISITMAX,
                                             PFTRTAMV, PFTRTAMU,
                                             PFTRTDUV, PFTRTDUU,
                                             PFTRTTMV, PFTRTTMU))
        
        arr_df <- arr_df[, gf_cols]
    
        gf_df <- rbind(gf_df, arr_df)
        suppgf_df <- rbind(suppgf_df, supparr_df)
        
      }  
    }
  } else {
    l <- loadSerializedStudyData(data_src, study_id, "Genetics Findings")
    if (assay_type == "ALL") {
      gf_df <- l[[1]]
      suppgf_df <- l[[2]]
    } else {
      
    }
  }   
  
  cat("done", "\n")

  gf_l <- list()
  if (nrow(gf_df) > 0)
    gf_l <- list(pf_df=gf_df, supppf_df=suppgf_df)
  
  gf_l
} 

# Get count of Genetics Findings data of a specific study
# 
# The function \code{getCountOfGeneticsFindings} queries the ImmPort database for count 
# of Genetics Findings data 
# 
# @param data_src A connection handle to ImmPort database instance
# @param assay_type Assay Type **optional**
# @param study_id Identifier of a specific study
# @return a count of Genetics Findings data 
# @examples
# \dontrun{
#   # get count of all of study SDY1's Genetics Findings data
#   count <- getCountOfGeneticsFindings(data_src, "SDY1")
#   
#   # get count of study SDY1's Genetics Findings data that was generated using Array assay
#   count <- getCountOfGeneticsFindings(data_src, "SDY1", "Array")
# }
getCountOfGeneticsFindings <- function(data_src, study_id, assay_type="ALL") {
  count <- 0
  if ((assay_type == "ALL") || (assay_type =="HLA Typing")) 
    count <- count + getCountOfHlaTypingResults(data_src, study_id)
  if ((assay_type == "ALL") || (assay_type =="Array")) 
    count <- count + getCountOfArrayResults(data_src, study_id)
  
  count
}

##' Genetics Findings Domain Variables
##' @name PF
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     PFSEQ \tab Sequence Number \cr
##'     PFGRPID \tab Group Identifier \cr
##'     PFTEST \tab  Test Name \cr
##'     PFCAT \tab Category for Test \cr
##'     PFMETHOD \tab Method of the Test \cr
##'     PFGENRI \tab Genetic Region of Interest \cr
##'     PFORRES \tab Result or Finding in Original Units \cr
##'     PFALLELC \tab Allele (Chromosome) Identifier \cr
##'     PFSPEC \tab Specimen Type \cr
##'     PFREFID \tab Reference ID (Specimen Identifier) \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     PFELTM \tab Planned Elapsed Time from Time Point Ref \cr
##'     PFTPTREF \tab Time Point Reference
##'   }
##' }
NULL
#> NULL 

##' Genetics Findings Domain Supplemental Variables
##' @name SUPPPF
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
##'     PFPOPAR \tab Geographic Area of the Population \cr
##'     PFSPECSB \tab Specimen Subtype \cr
##'     PFREFIDP \tab Source Specimen Identifier \cr
##'     VISITMIN \tab Planned Visit Minimum Start Day \cr
##'     VISITMAX \tab Planned Visit Maximum Start Day \cr
##'     PFSPTRT \tab Specimen Treatment \cr
##'     PFTRTAMV \tab Specimen Treatment Amount Value \cr
##'     PFTRTAMU \tab Specimen Treatment Amount Unit \cr
##'     PFTRTDUV \tab Specimen Treatment Duration Value \cr
##'     PFTRTDUU \tab Specimen Treatment Duration Unit \cr
##'     PFTRTTMV \tab Specimen Treatment Temperature Value \cr
##'     PFTRTTMU \tab Specimen Treatment Temperature Unit
##'   }
##' }
NULL
#> NULL

