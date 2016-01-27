#' Cellular Quantification Domain
#' 
#' @name Cellular Quantification Domain
#' @description The ImmPort study data generated from assays of types: Flow and ELISPOT are grouped into the 
#' Cellular Quantification Domain. The data is reformated to a custom Cellular Quantification domain model in 
#' CDISC SDTM standards, and is a list of 2 data frames containing 1) Cellular Quantification data \code{\link{ZB}} and 2) any supplemental 
#'   Cellular Quantification data \code{\link{SUPP}}
NULL
#> NULL 

# call to globalVariables to prevent from generating NOTE: no visible binding for global variable <variable name>
# this hack is to satisfy CRAN (http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when)
globalVariables(c("subject_id", "experiment_title", "assay_purpose", "measurement_technique",
                  "base_parent_population", "population_cell_number", "population_cell_number_unit",
                  "population_defnition_reported", "population_name_reported", "specimen_type",
                  "specimen_subtype", "specimen_treatment", 
                  "treatment_amount_value", "treatment_amount_unit",
                  "treatment_duration_value", "treatment_duration_unit",
                  "treatment_temperature_value", "treatment_temperature_unit",
                  "visit_name", "visit_min_start_day", "visit_max_start_day", "visit_order",
                  "elapsed_time_of_specimen_collection", "time_point_reference",
                  "biosample_accession", "file_name", "ZBSEQ", "result_id", "cell_type", "spot_number",
                  "analyte", "cell_number", "ZBBASPOP", "ZBPOPDEF", "ZBSPECSB",
                  "VISITMIN", "VISITMAX",
                  "ZBSPTRT", 
                  "ZBTRTAMV", "ZBTRTAMU",
                  "ZBTRTDUV", "ZBTRTDUU",
                  "ZBTRTTMV", "ZBTRTTMU"))

## Get Cellular Quantification data of a specific study
## 
## The function \code{getCellularQuantification} queries the ImmPort database for Cellular Quantification data and 
## reformats it to a custom Cellular Quantification domain model in CDISC SDTM standards. 
## 
## @param data_src A data_srcection handle to ImmPort (MySQL or SQLite) database instance or 
## a directory handle to folder where study RDS files are located
## @param assay_type Assay Type **optional**
## @param study_id Identifier of a specific study
## @return a list of 2 data frames containing 1) Cellular Quantification data \code{\link{ZB}} and 2) any supplemental 
##   Cellular Quantification data \code{\link{SUPP}}
## @examples
## \dontrun{
##   # get all of study SDY1's Cellular Quantification data
##   cq_l <- getCellularQuantification(data_src, "SDY1")
##   
##   # get study SDY1's Cellular Quantification data that was generated using Flow assay
##   cq_l <- getCellularQuantification(data_src, "SDY1", "Flow")
## }
##' @importFrom dplyr %>%
##' @importFrom data.table as.data.table is.data.table setDT := .N 
getCellularQuantification <- function(data_src, study_id, assay_type="ALL") {
    cat("loading Cellular Quantification data....")
  
    cq_cols <- c("STUDYID", "DOMAIN", "USUBJID", "ZBSEQ", "ZBTEST", "ZBCAT", "ZBMETHOD", "ZBPOPDEF", "ZBPOPNAM", "ZBORRES", 
               "ZBORRESU", "ZBBASPOP", "ZBSPEC", "VISITNUM", "VISIT", "ZBELTM", "ZBTPTREF", "ZBREFID", 
               "ZBXFN")
  
    suppcq_cols <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL")
  
  
    cq_df = data.frame()
    suppcq_df = data.frame()
    
    if ((class(data_src)[1] == 'MySQLConnection') || 
        (class(data_src)[1] == 'SQLiteConnection')) {
      if ((assay_type == "ALL") || (assay_type =="Flow")) {
        # get Flow results
        flow_df <- getFcsResults(data_src, study_id, "")
        if (nrow(flow_df) > 0) {
            flow_df <- select(flow_df, STUDYID = study_id, USUBJID = subject_id, ZBSEQ = sequence, ZBTEST = experiment_title, 
                ZBCAT = assay_purpose, ZBMETHOD = measurement_technique, ZBBASPOP = base_parent_population, ZBORRES = population_cell_number, 
                ZBORRESU = population_cell_number_unit, ZBPOPDEF = population_defnition_reported, ZBPOPNAM = population_name_reported, 
                ZBSPEC = specimen_type, ZBSPECSB = specimen_subtype, 
                ZBSPTRT = specimen_treatment, 
                ZBTRTAMV = treatment_amount_value, ZBTRTAMU = treatment_amount_unit,
                ZBTRTDUV = treatment_duration_value, ZBTRTDUU = treatment_duration_unit,
                ZBTRTTMV = treatment_temperature_value, ZBTRTTMU = treatment_temperature_unit,
                VISIT = visit_name, VISITNUM = visit_order,  VISITMIN = visit_min_start_day, VISITMAX = visit_max_start_day, 
                ZBELTM = elapsed_time_of_specimen_collection, ZBTPTREF = time_point_reference, 
                ZBREFID = biosample_accession, ZBXFN = file_name)
            
            flow_df$DOMAIN <- "ZB"
            
            qnam_values = c("ZBSPECSB",
                            "VISITMIN", "VISITMAX",
                            "ZBSPTRT", 
                            "ZBTRTAMV", "ZBTRTAMU",
                            "ZBTRTDUV", "ZBTRTDUU",
                            "ZBTRTTMV", "ZBTRTTMU")
            qlabel_values= c("Specimen Subtype",
                             "Planned Visit Minimum Start Day", "Planned Visit Maximum Start Day",
                             "Specimen Treatment", 
                             "Specimen Treatment Amount Value", "Specimen Treatment Amount Unit",
                             "Specimen Treatment Duration Value", "Specimen Treatment Duration Unit", 
                             "Specimen Treatment Temperature Value", "Specimen Treatment Temperature Unit")
            
            
#             flow_df <- flow_df[, c("STUDYID", "DOMAIN", "USUBJID", "ZBSEQ", "ZBTEST", "ZBCAT", "ZBMETHOD", "ZBPOPDEF", "ZBPOPNAM", "ZBORRES", 
#                                    "ZBORRESU", "ZBBASPOP", "ZBSPEC", "ZBSPECSB", "VISIT", "ZBELTM", "ZBTPTREF", "ZBREFID", 
#                                    "ZBXFN")]
            flow_df <- transform(flow_df, ZBSEQ = as.integer(ZBSEQ))
            setDT(flow_df)[, `:=`(ZBSEQ, seq_len(.N)), by = "USUBJID"]
            flow_df <- as.data.frame(flow_df)
            
            suppflow_df <- melt(flow_df, 
                               id = c("STUDYID", "DOMAIN", "USUBJID", "ZBSEQ"), 
                               measure = qnam_values, 
                               variable.name = "QNAM", 
                               value.name = "QVAL")
            
            suppflow_df <- transform(suppflow_df, QLABEL = unlist(qlabel_values[QNAM]))
            suppflow_df <- rename(suppflow_df, c("DOMAIN" = "RDOMAIN", "ZBSEQ" = "IDVARVAL"))
            suppflow_df$IDVAR <- "ZBSEQ"
            
            
            suppflow_df <- suppflow_df[suppcq_cols]
            
            # remove rows that have empty QVAL values
            suppflow_df <- subset(suppflow_df,QVAL!="")      
            
            flow_df <- subset(flow_df, select = -c(ZBSPECSB, 
                                                   VISITMIN, VISITMAX,
                                                   ZBSPTRT, 
                                                   ZBTRTAMV, ZBTRTAMU,
                                                   ZBTRTDUV, ZBTRTDUU,
                                                   ZBTRTTMV, ZBTRTTMU))
            
            flow_df <- flow_df[, cq_cols]
            
            cq_df <- rbind(cq_df, flow_df)
            suppcq_df <- rbind(suppcq_df, suppflow_df)
            
        }
      }

      
      if ((assay_type == "ALL") || (assay_type =="ELISPOT")) {
        # get ELISPOT results
        
        # elispot_column_names <- c('study_id', 'subject_id', 'result_id', 'analyte', 'comments', 'cell_number',
        # 'cell_type', 'spot_number', 'experiment_title', 'assay_purpose', 'measurement_technique', 'biosample_accession',
        # 'specimen_type', 'specimen_subtype', 'visit_name', 'study_time_of_specimen_collection',
        # 'unit_of_study_time_of_specimen_collection', 'study_time_t0_event', 'study_time_t0_event_specify', 'file_name')
        
        measurement_types <- list("Protein_Quantification", "Cytokine_Quantification")
        elp_df <- getElispotResults(data_src, study_id, "")
        if (nrow(elp_df) > 0) {
          elp_df <- elp_df %>% 
            select(STUDYID = study_id, USUBJID = subject_id, ZBSEQ = result_id, ZBTEST = experiment_title, 
                                      ZBCAT = assay_purpose, ZBMETHOD = measurement_technique, ZBBASPOP=cell_type, ZBORRES = spot_number, 
                                      ZBPOPDEF = analyte, cell_number, 
                                      ZBSPEC = specimen_type, ZBSPECSB = specimen_subtype,
                                      ZBSPTRT = specimen_treatment, 
                                      ZBTRTAMV = treatment_amount_value, ZBTRTAMU = treatment_amount_unit,
                                      ZBTRTDUV = treatment_duration_value, ZBTRTDUU = treatment_duration_unit,
                                      ZBTRTTMV = treatment_temperature_value, ZBTRTTMU = treatment_temperature_unit,
                                      VISIT = visit_name, VISITNUM = visit_order,  VISITMIN = visit_min_start_day, VISITMAX = visit_max_start_day, 
                                      ZBELTM = elapsed_time_of_specimen_collection, ZBTPTREF = time_point_reference,  
                                      ZBREFID = biosample_accession, ZBXFN = file_name) %>% 
            mutate(ZBORRESU = paste(cell_number, ZBBASPOP))  %>% 
            mutate(ZBPOPNAM = ZBPOPDEF) 
            
          elp_df$DOMAIN <- "ZB"

          qnam_values = c("ZBSPECSB",
                          "VISITMIN", "VISITMAX",
                          "ZBSPTRT", 
                          "ZBTRTAMV", "ZBTRTAMU",
                          "ZBTRTDUV", "ZBTRTDUU",
                          "ZBTRTTMV", "ZBTRTTMU")
          qlabel_values= c("Specimen Subtype",
                           "Planned Visit Minimum Start Day", "Planned Visit Maximum Start Day",
                           "Specimen Treatment", 
                           "Specimen Treatment Amount Value", "Specimen Treatment Amount Unit",
                           "Specimen Treatment Duration Value", "Specimen Treatment Duration Unit", 
                           "Specimen Treatment Temperature Value", "Specimen Treatment Temperature Unit")
          
          
#           elp_df <- elp_df[, c("STUDYID", "DOMAIN", "USUBJID", "ZBSEQ", "ZBTEST", "ZBCAT", "ZBMETHOD", "ZBPOPDEF", "ZBPOPNAM", "ZBORRES", 
#                                "ZBORRESU", "ZBBASPOP", "ZBSPEC", "VISIT", "ZBELTM", "ZBTPTREF", "ZBREFID", 
#                                "ZBXFN")]

          suppelp_df <- melt(elp_df, 
                              id = c("STUDYID", "DOMAIN", "USUBJID", "ZBSEQ"), 
                              measure = qnam_values, 
                              variable.name = "QNAM", 
                              value.name = "QVAL")
          
          suppelp_df <- transform(suppelp_df, QLABEL = unlist(qlabel_values[QNAM]))
          suppelp_df <- rename(suppelp_df, c("DOMAIN" = "RDOMAIN", "ZBSEQ" = "IDVARVAL"))
          suppelp_df$IDVAR <- "ZBSEQ"
          
          
          suppelp_df <- suppelp_df[suppcq_cols]
          
          # remove rows that have empty QVAL values
          suppelp_df <- subset(suppelp_df,QVAL!="")      
          
          elp_df <- subset(elp_df, select = -c(ZBSPECSB, 
                                               VISITMIN, VISITMAX,
                                               ZBSPTRT, 
                                               ZBTRTAMV, ZBTRTAMU,
                                               ZBTRTDUV, ZBTRTDUU,
                                               ZBTRTTMV, ZBTRTTMU))
          
          elp_df <- elp_df[, cq_cols]
          
          cq_df <- rbind(cq_df, elp_df)
          suppcq_df <- rbind(suppcq_df, suppelp_df)
          
        }
      }
    } else {
      l <- loadSerializedStudyData(data_src, study_id, "Cellular Quantification")
      if (assay_type == "ALL") {
        cq_df <- l[[1]]
        suppcq_df <- l[[2]]
      } else {
        
      }
    }   
    
    
    cat("done", "\n")
    
    cq_l <- list()
    if (nrow(cq_df) > 0)
      cq_l <- list(zb_df=cq_df, suppzb_df=suppcq_df)
    
    cq_l
} 

## Get count of Cellular Quantification data of a specific study
## 
## The function \code{getCountOfCellularQuantification} queries the ImmPort database for count 
## of Cellular Quantification data 
## 
## @param data_src A connection handle to ImmPort database instance
## @param assay_type Assay Type **optional**
## @param study_id Identifier of a specific study
## @return a count of Cellular Quantification data 
## @examples
## \dontrun{
##   # get count of all of study SDY1's Cellular Quantification data
##   count <- getCountOfCellularQuantification(data_src, "SDY1")
##   
##   # get count of study SDY1's Cellular Quantification data that was generated using Flow assay
##   count <- getCountOfCellularQuantification(data_src,"SDY1",  "Flow")
## }
getCountOfCellularQuantification <- function(data_src, study_id, assay_type="ALL") {
  count <- 0
  if ((assay_type == "ALL") || (assay_type =="Flow")) 
    count <- count + getCountOfFcsResults(data_src, study_id)
  if ((assay_type == "ALL") || (assay_type =="ELISPOT")) 
    count <- count + getCountOfElispotResults(data_src, study_id)
  
  count
}
  

##' Cellular Quantification Domain Variables
##' @name ZB
##' @description {
##'   \tabular{ll}{
##'     \strong{Variable Name  } \tab \strong{Variable Label} \cr
##'     STUDYID \tab Study Identifier \cr
##'     DOMAIN  \tab Domain Abbreviation \cr
##'     USUBJID \tab Unique Subject Identifier \cr
##'     ZBSEQ \tab Sequence Number \cr
##'     ZBTEST \tab Cellular Quantification Test Name \cr
##'     ZBCAT \tab Category for Cellular Quantification \cr
##'     ZBMETHOD \tab Measurement Technique \cr
##'     ZBPOPDEF \tab Cell Population Definition \cr
##'     ZBPOPNAM \tab Cell Population Name \cr
##'     ZBORRES \tab Result or Finding in Original Units \cr
##'     ZBORRESU \tab Original Units \cr
##'     ZBBASPOP \tab Base Parent Population \cr
##'     ZBSPEC \tab Specimen Type \cr
##'     VISITNUM \tab Visit Number \cr
##'     VISIT \tab Visit Name \cr
##'     ZBELTM \tab Planned Elapsed Time from Time Point Ref \cr
##'     ZBTPTREF \tab Time Point Reference \cr
##'     ZBREFID \tab Specimen Identifier \cr
##'     ZBXFN \tab Raw Data File or Life Science Identifier
##'   }
##' }
NULL
#> NULL 

##' Cellular Quantification Domain Supplemental Variables
##' @name SUPPZB
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
##'     ZBSPECSB \tab Specimen Subtype \cr
##'     VISITMIN \tab Planned Visit Minimum Start Day \cr
##'     VISITMAX \tab Planned Visit Maximum Start Day \cr
##'     ZBSPTRT \tab Specimen Treatment \cr
##'     ZBTRTAMV \tab Specimen Treatment Amount Value \cr
##'     ZBTRTAMU \tab Specimen Treatment Amount Unit \cr
##'     ZBTRTDUV \tab Specimen Treatment Duration Value \cr
##'     ZBTRTDUU \tab Specimen Treatment Duration Unit \cr
##'     ZBTRTTMV \tab Specimen Treatment Temperature Value \cr
##'     ZBTRTTMU \tab Specimen Treatment Temperature Unit
##'   }
##' }
NULL
#> NULL

