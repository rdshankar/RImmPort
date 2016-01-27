#' Nucleic Acid Quantification Domain
#' 
#' @name Nucleic Acid Quantification Domain
#' @description The ImmPort study data generated from assays of types: PCR are grouped into the 
#' Nucleic Acid Quantification Domain. The data is reformated to a custom Nucleic Acid Quantification domain model in 
#' CDISC SDTM standards, and is a list of 2 data frames containing 1) Nucleic Acid Quantification data \code{\link{ZC}} and 2) any supplemental 
#'   Nucleic Acid Quantification data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("subject_id", "experiment_title", "assay_purpose", "measurement_technique",
                  "entrez_gene_id", "gene_name", "gene_symbol",
                  "threshold_cycles", "value_reported", "unit_reported", "specimen_type",
                  "specimen_subtype", "specimen_treatment", 
                  "treatment_amount_value", "treatment_amount_unit",
                  "treatment_duration_value", "treatment_duration_unit",
                  "treatment_temperature_value", "treatment_temperature_unit",
                  "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                  "elapsed_time_of_specimen_collection", "time_point_reference",
                  "biosample_accession", "ZCSPECSB",
                  "VISITMIN", "VISITMAX",
                  "ZCSPTRT", 
                  "ZCTRTAMV", "ZCTRTAMU",
                  "ZCTRTDUV", "ZCTRTDUU",
                  "ZCTRTTMV", "ZCTRTTMU"))

## Get Nucleic Acid Quantification data of a specific study
## 
## The function \code{getNucleicAcidQuantification} queries the ImmPort database for Nucleic Acid Quantification data and 
## reformats it to a custom Nucleic Acid Quantification domain model in CDISC SDTM standards. 
## 
## @param data_src A data_srcection handle to ImmPort (MySQL or SQLite) database instance or 
## a directory handle to folder where study RDS files are located
## @param assay_type Assay Type **optional**
## @param study_id Identifier of a specific study
## @return a list of 2 data frames containing 1) Nucleic Acid Quantification data \code{\link{ZC}} and 2) any supplemental 
##   Nucleic Acid Quantification data \code{\link{SUPP}}
## @examples
## \dontrun{
##   # get all of study SDY1's Nucleic Acid Quantification data
##   nq_l <- getNucleicAcidQuantification(data_src, "SDY1")
##   
##   # get study SDY1's Nucleic Acid Quantification data that was generated using PCR technnology
##   nq_l <- getNucleicAcidQuantification(data_src, "SDY1", "PCR")
## }
##' @importFrom dplyr select
getNucleicAcidQuantification <- function(data_src, study_id, assay_type="ALL") {
  cat("loading Nucleic Acid Quantification data....")

  nq_cols <- c("STUDYID", "DOMAIN", "USUBJID", "ZCSEQ", 
               "ZCTEST", "ZCCAT", "ZCMETHOD", 
               "ZCENTRZD", "ZCGENNAM", "ZCGENSYM",
               "ZCTHRESH", "ZCORRES", "ZCORRESU", 
               "ZCSPEC", "ZCREFID", 
               "VISITNUM", "VISIT", "ZCELTM", "ZCTPTREF")
  
  suppnq_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
  nq_df = data.frame()
  suppnq_df = data.frame()
  
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    if ((assay_type == "ALL") || (assay_type =="PCR")) {
      
      # get PCR results

    #   pcr_column_names <- c("study_id", "subject_id", "result_id",
    #                         "entrez_gene_id", "gene_name", "gene_symbol", 
    #                         "threshold_cycles", "value_reported", "unit_reported",
    #                         "experiment_title", "assay_purpose", "measurement_technique",
    #                         "biosample_accession", "specimen_type", "specimen_subtype",
    #                         "visit_name", "elapsed_time_of_specimen_collection", "time_point_reference",
    #                         "study_time_t0_event", "study_time_t0_event_specify")
      
      pcr_df <- getPcrResults(data_src, study_id, "")
      if (nrow(pcr_df) > 0) {
        pcr_df <- select(pcr_df, STUDYID = study_id, USUBJID = subject_id, ZCSEQ = result_id, ZCTEST = experiment_title, 
                         ZCCAT = assay_purpose, ZCMETHOD = measurement_technique, 
                         ZCENTRZD = entrez_gene_id, ZCGENNAM = gene_name, ZCGENSYM = gene_symbol,
                         ZCTHRESH = threshold_cycles, ZCORRES = value_reported, ZCORRESU = unit_reported,
                         ZCSPEC = specimen_type, ZCSPECSB = specimen_subtype,
                         ZCSPTRT = specimen_treatment, 
                         ZCTRTAMV = treatment_amount_value, ZCTRTAMU = treatment_amount_unit,
                         ZCTRTDUV = treatment_duration_value, ZCTRTDUU = treatment_duration_unit,
                         ZCTRTTMV = treatment_temperature_value, ZCTRTTMU = treatment_temperature_unit,
                         VISIT = visit_name, VISITNUM = visit_order,  VISITMIN = visit_min_start_day, VISITMAX = visit_max_start_day, 
                         ZCELTM = elapsed_time_of_specimen_collection, 
                         ZCTPTREF = time_point_reference, ZCREFID = biosample_accession) 
        
        pcr_df$DOMAIN <- "ZC"
        
        qnam_values = c("ZCSPECSB",
                        "VISITMIN", "VISITMAX",
                        "ZCSPTRT", 
                        "ZCTRTAMV", "ZCTRTAMU",
                        "ZCTRTDUV", "ZCTRTDUU",
                        "ZCTRTTMV", "ZCTRTTMU")
        qlabel_values= c("Specimen Subtype",
                         "Planned Visit Minimum Start Day", "Planned Visit Maximum Start Day",
                         "Specimen Treatment", 
                         "Specimen Treatment Amount Value", "Specimen Treatment Amount Unit",
                         "Specimen Treatment Duration Value", "Specimen Treatment Duration Unit", 
                         "Specimen Treatment Temperature Value", "Specimen Treatment Temperature Unit")

        supppcr_df <- melt(pcr_df, 
                           id = c("STUDYID", "DOMAIN", "USUBJID", "ZCSEQ"), 
                           measure = qnam_values, 
                           variable.name = "QNAM", 
                           value.name = "QVAL")
        
        supppcr_df <- transform(supppcr_df, QLABEL = unlist(qlabel_values[QNAM]))
        supppcr_df <- rename(supppcr_df, c("DOMAIN" = "RDOMAIN", "ZCSEQ" = "IDVARVAL"))
        supppcr_df$IDVAR <- "ZCSEQ"
        
        
        supppcr_df <- supppcr_df[suppnq_cols]
        
        # remove rows that have empty QVAL values
        supppcr_df <- subset(supppcr_df,QVAL!="")      
        
        pcr_df <- subset(pcr_df, select = -c(ZCSPECSB, ZCSPTRT, 
                                             VISITMIN, VISITMAX,
                                             ZCTRTAMV, ZCTRTAMU,
                                             ZCTRTDUV, ZCTRTDUU,
                                             ZCTRTTMV, ZCTRTTMU))
        
        pcr_df <- pcr_df[, nq_cols]
        
        nq_df <- rbind(nq_df, pcr_df)
        suppnq_df <- rbind(suppnq_df, supppcr_df)
        
      }
    }
  } else {
    l <- loadSerializedStudyData(data_src, study_id, "Nucleic Acid Quantification")
    if (assay_type == "ALL") {
      nq_df <- l[[1]]
      suppnq_df <- l[[2]]
    } else {
      
    }
  }   
  
  cat("done", "\n")

  nq_l <- list()
  if (nrow(nq_df) > 0)
    nq_l <- list(zc_df=nq_df, suppzc_df=suppnq_df)
  
  nq_l
} 

## Get count of Nucleic Acid Quantification data of a specific study
## 
## The function \code{getCountOfNucleicAcidQuantification} queries the ImmPort database for count 
## of Nucleic Acid Quantification data 
## 
## @param data_src A connection handle to ImmPort database instance
## @param assay_type Assay Type **optional**
## @param study_id Identifier of a specific study
## @return a count of Nucleic Acid Quantification data 
## @examples
## \dontrun{
##   # get count of all of study SDY1's Nucleic Acid Quantification data
##   count <- getCountOfNucleicAcidQuantification(data_src, "SDY1")
##   
##   # get count of study SDY1's Nucleic Acid Quantification data that was generated using PCR assay
##   count <- getCountOfNucleicAcidQuantification(data_src, "SDY1", "PCR")
## }
getCountOfNucleicAcidQuantification <- function(data_src, study_id, assay_type="ALL") {
  count <- 0
  if ((assay_type == "ALL") || (assay_type =="PCR")) 
    count <- count + getCountOfPcrResults(data_src, study_id)
  
  count
}

##' Nucleic Acid Quantification Domain Variables
##' @name ZC
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     ZCSEQ \tab Sequence Number \cr
##'     ZCTEST \tab Nucleic Acid Quantification Test Name \cr
##'     ZCCAT \tab Category for Nucleic Acid Quantification \cr
##'     ZCMETHOD \tab Measurement Technique \cr
##'     ZCPOPDEF \tab Cell Population Definition \cr
##'     ZCPOPNAM \tab Cell Population Name \cr
##'     ZCORRES \tab Result or Finding in Original Units \cr
##'     ZCORRESU \tab Original Units \cr
##'     ZCBASPOP \tab Base Parent Population \cr
##'     ZCSPEC \tab Specimen Type \cr
##'     ZCREFID \tab Specimen Identifier \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     ZCELTM \tab Planned Elapsed Time from Time Point Ref \cr
##'     ZCTPTREF \tab Time Point Reference
##'   }
##' }
NULL
#> NULL 

##' Nucleic Acid Quantification Domain Supplemental Variables
##' @name SUPPZC
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
##'     ZCSPECSB \tab Specimen Subtype \cr
##'     VISITMIN \tab Planned Visit Minimum Start Day \cr
##'     VISITMAX \tab Planned Visit Maximum Start Day \cr
##'     ZCSPTRT \tab Specimen Treatment \cr
##'     ZCTRTAMV \tab Specimen Treatment Amount Value \cr
##'     ZCTRTAMU \tab Specimen Treatment Amount Unit \cr
##'     ZCTRTDUV \tab Specimen Treatment Duration Value \cr
##'     ZCTRTDUU \tab Specimen Treatment Duration Unit \cr
##'     ZCTRTTMV \tab Specimen Treatment Temperature Value \cr
##'     ZCTRTTMU \tab Specimen Treatment Temperature Unit
##'   }
##' }
NULL
#> NULL

