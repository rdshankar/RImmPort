domain_names_codes <- c(
  c("Adverse Events", "AE"),
  c("Concomitant Medications", "CM"),
  c("Demographics", "DM"),
  c("Exposure", "EX"),
  c("Medical History", "MH"),
  c("Associated Persons Medical History", "APMH"),
  c("Laboratory Test Results", "LB"),
  c("Physical Examination", "PE"),
  c("Protocol Deviations", "DV"),
  c("Trial Arms", "TA"),
  c("Trial Inclusion Exclusion Criteria", "TI"),
  c("Trial Summary", "TS"),
  c("Subject Visits", "SV"),
  c("Substance Use", "SU"),
  c("Vital Signs", "VS"),
  c("Questionnaires", "QS"),
  c("Findings About", "FA"),
  c("Skin Response", "SR"),
  c("Genetics Findings", "PF"),
  c("Protein Quantification", "ZA"),
  c("Cellular Quantification", "ZB"),
  c("Nucleic Acid Quantification", "ZC"),
  c("Titer Assay Results", "ZD")
)

RImmPort.env <- new.env()
.datatable.aware=TRUE

domain_info <- structure(data.frame(
  matrix(unlist(domain_names_codes),length(domain_names_codes)/2,2,TRUE), stringsAsFactors = FALSE),
  names=c("Domain Name","Domain Code"))

##' Set ImmPort data ource
##' 
##' The function \code{setImmPortDataSource} sets the data source variable in RImmPort environment, 
##' to the connection handle to the MySQL or SQLite database, or to the file directory where the 
##' pre-created RImmPort-formatted files are stored.
##' 
##' @param data_src A connection handle to ImmPort (MySQL or SQLite) database instance or 
##' a directory handle to folder where study RImmPort-formatted (.rds) files located
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' @export
setImmPortDataSource <- function(data_src){
  RImmPort.env$data_src <- data_src
  
  if (class(data_src) == "character") {
    if (file.exists(data_src) == FALSE) {
      stop("data source = ", data_src, " not found")
    }  
    # unzip study files into data directory
    zipfiles <- list.files(path = data_src, pattern = "\\.zip$")
    for (zf in zipfiles) {
      unzip(file.path(data_src, zf), exdir = data_src)
    }
  }
}

##' Special Purpose class
##' 
##' @field dm_l Demographics data \code{\link{DM}} and supplemental Demographics data \code{\link{SUPP}}
##' @field sv_l Subject Visits data \code{\link{SV}} and supplemental Subject Visits data \code{\link{SUPP}}
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' sdy139 <- getStudy("SDY139")
##' dm_df <- sdy139$special_purpose$dm_l$dm_df
##' @importFrom methods  setRefClass
##' @importFrom methods  new
##' @export SpecialPurpose
##' @exportClass SpecialPurpose
SpecialPurpose <- setRefClass("SpecialPurpose", 
                              fields = list( 
                                dm_l="list",
                                sv_l="list" 
                              ),
                              methods = list(
                                getSpecialPurpose = function(data_src, study_id) {
                                  dm_l <<- getDemographics(data_src, study_id)
                                  sv_l <<- getSubjectVisits(data_src, study_id)
                                }
                              ))
##' Interventions class
##' 
##' @field cm_l Concomitant Medications data \code{\link{CM}} and supplemental Concomitant Medications data \code{\link{SUPP}}
##' @field ex_l Exposure data \code{\link{EX}} and supplemental Exposure data \code{\link{SUPP}}
##' @field su_l Substance Use data \code{\link{SU}} and supplemental Substance Use data \code{\link{SUPP}}
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' sdy139 <- getStudy("SDY139")
##' cm_df <- sdy139$interventions$cm_l$cm_df
##' @export Interventions
##' @exportClass Interventions
Interventions <- setRefClass("Interventions", 
                             fields = list(
                               cm_l="list",
                               ex_l="list",
                               su_l="list"
                             ),
                             methods = list(
                               getInterventions = function(data_src, study_id) {
                                 cm_l <<- getConcomitantMedications(data_src, study_id)
                                 ex_l <<- getExposure(data_src, study_id)
                                 su_l <<- getSubstanceUse(data_src, study_id)
                               }
                             ))
##' Events class
##' 
##' @field ae_l Adverse Events data \code{\link{AE}} and supplemental Adverse Events data \code{\link{SUPP}}
##' @field dv_l Protocol Deviations data \code{\link{DV}} and supplemental Protocol Deviations data \code{\link{SUPPDV}}
##' @field mh_l Medical History data \code{\link{MH}} and supplemental Medical History data \code{\link{SUPPMH}}
##' @field apmh_l Associated Persons Medical History data \code{\link{APMH}} and supplemental Associated Persons Medical History data \code{\link{SUPP}}
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' sdy139 <- getStudy("SDY139")
##' ae_df <- sdy139$events$ae_l$ae_df
##' @export Events
##' @exportClass Events
Events <- setRefClass("Events", fields = list(
  ae_l="list",
  dv_l="list",
  mh_l="list",
  apmh_l="list"
),
methods = list(
  getEvents = function(data_src, study_id) {
    ae_l <<- getAdverseEvents(data_src, study_id)
    dv_l <<- getProtocolDeviations(data_src, study_id)
    mh_l <<- getMedicalHistory(data_src, study_id)
    apmh_l <<- getAssociatedPersonsMedicalHistory(data_src, study_id)
  }
))
##' Findings class
##' 
##' @field lb_l Laboratory Test Results data \code{\link{LB}} and supplemental Laboratory Test Results data \code{\link{SUPPLB}}
##' @field pe_l Physical Examination data \code{\link{PE}} and supplemental Physical Examination data \code{\link{SUPPPE}}
##' @field vs_l Vital Signs data \code{\link{VS}} and supplemental Vital Signs data \code{\link{SUPPVS}}
##' @field qs_l Questionnaires data \code{\link{QS}} and supplemental Questionnaires data \code{\link{SUPP}}
##' @field fa_l Findings About data \code{\link{FA}} and supplemental Findings About data \code{\link{SUPPFA}}
##' @field sr_l Skin Response data \code{\link{SR}} and supplemental Skin Response data \code{\link{SUPP}}
##' @field pf_l Genetics Findings data \code{\link{PF}} and supplemental Genetics Findings data \code{\link{SUPPPF}}
##' @field za_l Protein Quantification data \code{\link{ZA}} and supplemental Protein Quantification data \code{\link{SUPPZA}}
##' @field zb_l Cellular Quantification data \code{\link{ZB}} and supplemental Cellular Quantification data \code{\link{SUPP}}
##' @field zc_l Nucleic Acid Quantification data \code{\link{ZC}} and supplemental Nucleic Acid Quantification data \code{\link{SUPP}}
##' @field zd_l Titer Assay Results data \code{\link{ZD}} and supplemental Titer Assay Results data \code{\link{SUPP}}
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' sdy139 <- getStudy("SDY139")
##' zb_df <- sdy139$findings$zb_l$zb_df
##' @export Findings
##' @exportClass Findings
Findings <- setRefClass("Findings", 
                        fields = list(
                          lb_l="list",
                          pe_l="list",
                          vs_l="list",
                          qs_l="list",
                          fa_l="list",
                          sr_l="list",
                          pf_l="list",
                          za_l="list",
                          zb_l="list",
                          zc_l="list",
                          zd_l="list"
                        ),
                        methods = list(
                          getFindings = function(data_src, study_id) {
                            lb_l <<- getLaboratoryTestResults(data_src, study_id)
                            pe_l <<- getPhysicalExamination(data_src, study_id)
                            vs_l <<- getVitalSigns(data_src, study_id)
                            qs_l <<- getQuestionnaires(data_src, study_id)
                            fa_l <<- getFindingsAbout(data_src, study_id)
                            sr_l <<- getSkinResponse(data_src, study_id)
                            pf_l <<- getGeneticsFindings(data_src, study_id)
                            za_l <<- getProteinQuantification(data_src, study_id)
                            zb_l <<- getCellularQuantification(data_src, study_id)
                            zc_l <<- getNucleicAcidQuantification(data_src, study_id)
                            zd_l <<- getTiterAssayResults(data_src, study_id)
                          }
                        ))
##' Trial Design class
##' 
##' @field ta_l Trial Arms data \code{\link{TA}} and supplemental Trial Arms data \code{\link{SUPP}}
##' @field ti_l Trial Inclusion Exclusion Criteria data \code{\link{TI}} and supplemental Trial Inclusion Exclusion Criteria data \code{\link{SUPP}}
##' @field ts_l Trial Summary data \code{\link{TS}} and supplemental Trial Summary data \code{\link{SUPP}}
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' sdy139 <- getStudy("SDY139")
##' ts_df <- sdy139$trial_design$ts_l$ts_df
##' @export TrialDesign
##' @exportClass TrialDesign
TrialDesign <- setRefClass("TrialDesign", 
                           fields = list(
                             ta_l="list",
                             ti_l="list",
                             ts_l="list"
                           ),
                           methods = list(
                             getTrialDesign = function(data_src, study_id) {
                               ta_l <<- getTrialArms(data_src, study_id)
                               ti_l <<- getTrialInclusionExclusionCriteria(data_src, study_id)
                               ts_l <<- getTrialSummary(data_src, study_id)
                             }
                           ))

##' Study class
##' 
##' @field special_purpose \code{\link{SpecialPurpose}} 
##' @field interventions \code{\link{Interventions}} 
##' @field events \code{\link{Events}} 
##' @field findings \code{\link{Findings}} 
##' @field trial_design \code{\link{TrialDesign}} 
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' sdy139 <- getStudy("SDY139")
##' @importFrom plyr ldply
##' @export Study
##' @exportClass Study
Study <- setRefClass("Study", fields = list(
  special_purpose="SpecialPurpose",
  interventions="Interventions",
  events="Events",
  findings="Findings",
  trial_design="TrialDesign"),
  methods = list(
    getTableOfContents = function() {
      contents_l <- list(
        r1=list(c1="Special Purpose", c2="Demographics", c3=""), 
        r2=list(c1="Special Purpose", c2="Subject Visits", c3=""), 
        r4=list(c1="Interventions", c2="Concomitant Medications", c3=""), 
        r5=list(c1="Interventions", c2="Exposure", c3=""), 
        r7=list(c1="Interventions", c2="Substance Use", c3=""), 
        r8=list(c1="Events", c2="Adverse Events", c3=""), 
        r11=list(c1="Events", c2="Protocol Deviations", c3=""), 
        r13=list(c1="Events", c2="Medical History", c3=""), 
        r14=list(c1="Events", c2="Associated Persons Medical History", c3=""), 
        r20=list(c1="Findings", c2="Laboratory Test Results", c3=""),  
        r29=list(c1="Findings", c2="Vital Signs", c3=""), 
        r3o=list(c1="Findings", c2="Questionnaires", c3=""), 
        r31=list(c1="Findings", c2="Findings About", c3=""), 
        r32=list(c1="Findings", c2="Skin Response", c3=""), 
        r33=list(c1="Findings", c2="Physical Examination", c3=""), 
        r34=list(c1="Findings", c2="Genetics Findings", c3=""), 
        r35=list(c1="Findings", c2="Protein Quantification", c3=""), 
        r36=list(c1="Findings", c2="Cellular Quantification", c3=""), 
        r37=list(c1="Findings", c2="Nucleic Acid Quantification", c3=""), 
        r38=list(c1="Findings", c2="Titer Assay Results", c3=""), 
        r45=list(c1="Trial Design", c2="Trial Inclusion Exclusion Criteria", c3=""), 
        r46=list(c1="Trial Design", c2="Trial Arms", c3=""), 
        r47=list(c1="Trial Design", c2="Trial Summary", c3=""))
      
      contents_df <- ldply (contents_l, data.frame, stringsAsFactors = FALSE)
      
      contents_df$.id <- NULL
      colnames(contents_df) <- c("Class", "Domain", "Included?")
      
      
      
      if (length(special_purpose$dm_l) > 0)
        contents_df[contents_df$Domain == "Demographics",3] <- "yes"
      if (length(special_purpose$sv_l) > 0)
        contents_df[contents_df$Domain == "Subject Visits",3] <- "yes"
      if (length(interventions$cm_l) > 0)
        contents_df[contents_df$Domain == "Concomitant Medications",3] <- "yes"
      if (length(interventions$ex_l) > 0)
        contents_df[contents_df$Domain == "Exposure",3] <- "yes"
      if (length(interventions$su_l) > 0)
        contents_df[contents_df$Domain == "Substance Use",3] <- "yes"
      if (length(events$ae_l) > 0)
        contents_df[contents_df$Domain == "Adverse Events",3] <- "yes"
      if (length(events$dv_l) > 0)
        contents_df[contents_df$Domain == "Protocol Deviations",3] <- "yes"
      if (length(events$mh_l) > 0)
        contents_df[contents_df$Domain == "Medical History",3] <- "yes"
      if (length(events$apmh_l) > 0)
        contents_df[contents_df$Domain == "Associated Persons Medical History",3] <- "yes"
      if (length(findings$lb_l) > 0)
        contents_df[contents_df$Domain == "Laboratory Test Results",3] <- "yes"
      if (length(findings$vs_l) > 0)
        contents_df[contents_df$Domain == "Vital Signs",3] <- "yes"
      if (length(findings$qs_l) > 0)
        contents_df[contents_df$Domain == "Questionnaires",3] <- "yes"
      if (length(findings$fa_l) > 0)
        contents_df[contents_df$Domain == "Findings About",3] <- "yes"
      if (length(findings$sr_l) > 0)
        contents_df[contents_df$Domain == "Skin Response",3] <- "yes"
      if (length(findings$pe_l) > 0)
        contents_df[contents_df$Domain == "Physical Examination",3] <- "yes"
      if (length(findings$pf_l) > 0)
        contents_df[contents_df$Domain == "Genetics Findings",3] <- "yes"
      if (length(findings$za_l) > 0)
        contents_df[contents_df$Domain == "Protein Quantification",3] <- "yes"      
      if (length(findings$zb_l) > 0)
        contents_df[contents_df$Domain == "Cellular Quantification",3] <- "yes"
      if (length(findings$zc_l) > 0)
        contents_df[contents_df$Domain == "Nucleic Acid Quantification",3] <- "yes"
      if (length(findings$zd_l) > 0)
        contents_df[contents_df$Domain == "Titer Assay Results",3] <- "yes"
      if (length(trial_design$ti_l) > 0)
        contents_df[contents_df$Domain == "Trial Inclusion Exclusion Criteria",3] <- "yes"      
      if (length(trial_design$ta_l) > 0)
        contents_df[contents_df$Domain == "Trial Arms",3] <- "yes"
      if (length(trial_design$ts_l) > 0)
        contents_df[contents_df$Domain == "Trial Summary",3] <- "yes"
      
      contents_df
    }
  ))

########################################################################


########################################################################

##' Get all data of a specific study from the ImmPort data source
##' 
##' The function \code{getStudy} queries the ImmPort data source for data of a specific study in 
##' all domains. The data is then structured into \code{Study} as classes, domains, variables and values.
##' 
##' @param study_id Identifier of a specific study
##' @return A study data object where in all data are structured as classes, domains, variables and values (in CDISC format)
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' sdy139 <- getStudy("SDY139")
##' @export
getStudy <- function(study_id) {
  if (is.null(r <- get0("data_src", envir = RImmPort.env))) {
    stop("ImmPort data source is not set: See ?setImmPortDataSource() ")
  }
  
  data_src <- RImmPort.env$data_src
  
  cat("loading Study ID = ", study_id, "\n")
  
  study <- Study$new()
  
  study$special_purpose$getSpecialPurpose(data_src, study_id)
  study$interventions$getInterventions(data_src, study_id) 
  study$events$getEvents(data_src, study_id)
  study$findings$getFindings(data_src, study_id)
  study$trial_design$getTrialDesign(data_src, study_id)  
  
  cat("done loading Study ID = ", study_id, "\n")
  
  study
}

##' Get names of all domains and their codes
##' 
##' The function \code{getListOfDomains} returns a list of all domain names and codes
##' 
##' @return A list of of all domain names and codes
##' @examples
##'  domains_df <- getListOfDomains()
##' @export
getListOfDomains <- function() {
  domain_info
}

##' Get code of a specific domain 
##' 
##' The function \code{getListOfDomains} returns the code of a specific domain
##' 
##' @param domain Name of a specific domain
##' @return A list of of all domain names and codes
##' @examples
##'  domain <- "Demographics"
##'  code <- getDomainCode(domain)
##' @export
getDomainCode <- function(domain) {
  domain_info[which(domain_info[,1]==domain), 2]
}

##' Get specific domain data of one or more studies from the ImmPort database
##' 
##' @param domain Name of a specific domain
##' @param study_ids List of study indentifiers
##' @return a list of 1) domain data and 2) supplemental domain data of the studies
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' dm_df <- getDomainDataOfStudies("Demographics", "SDY139")
##' @export
getDomainDataOfStudies <- function(domain, study_ids) {
  if (is.null(r <- get0("data_src", envir = RImmPort.env))) {
    stop("ImmPort data source is not set: See ?setImmPortDataSource() ")
  }
  
  data_src <- RImmPort.env$data_src
  
  if (domain %in% getListOfDomains()[,'Domain Name'] == FALSE) {
    stop(paste("domain = ", domain, " is not found in the list of supported domains; 
               Run getListOfDomains() for a complete list"))
  }
  
  data_l= list()
  data_df = data.frame()
  switch (domain,          
          "Adverse Events" = {
            data_l <- getAdverseEventsOfStudies(data_src, study_ids)
          },
          "Concomitant Medications" = {
            data_l <- getConcomitantMedicationsOfStudies(data_src, study_ids)
          },
          "Demographics" = {
            data_l <- getDemographicsOfStudies(data_src, study_ids)
          },
          "Exposure" = {
            data_l <- getExposureOfStudies(data_src, study_ids)
          },
          "Laboratory Test Results" = {
            data_l <- getLaboratoryTestResultsOfStudies(data_src, study_ids)
          },
          "Medical History" = {
            data_l <- getMedicalHistoryOfStudies(data_src, study_ids)
          },
          "Associated Persons Medical History" = {
            data_l <- getAssociatedPersonsMedicalHistoryOfStudies(data_src, study_ids)
          },
          "Physical Examination" = {
            data_l <- getPhysicalExaminationOfStudies(data_src, study_ids)
          },
          "Protocol Deviations" = {
            data_l <- getProtocolDeviationsOfStudies(data_src, study_ids)
          },
          "Trial Arms" = {
            data_l <- getTrialArmsOfStudies(data_src, study_ids)
          },
          "Trial Inclusion Exclusion Criteria" = {
            data_l <- getTrialInclusionExclusionCriteriaOfStudies(data_src, study_ids)
          },
          "Trial Summary" = {
            data_l <- getTrialSummaryOfStudies(data_src, study_ids)
          },
          "Subject Visits" = {
            data_l <- getSubjectVisitsOfStudies(data_src, study_ids)
          },
          "Substance Use" = {
            data_l <- getSubstanceUseOfStudies(data_src, study_ids)
          },
          "Vital Signs" = {
            data_l <- getVitalSignsOfStudies(data_src, study_ids)
          },
          "Questionnaires" = {
            data_l <- getQuestionnairesOfStudies(data_src, study_ids)
          },
          "Findings About" = {
            data_l <- getFindingsAboutOfStudies(data_src, study_ids)
          },
          "Skin Response" = {
            data_l <- getSkinResponseOfStudies(data_src, study_ids)
          },
          "Genetics Findings" = {
            data_l <- getGeneticsFindingsOfStudies(data_src, study_ids)
          },
          "Protein Quantification" = {
            data_l <- getProteinQuantificationOfStudies(data_src, study_ids)
          },
          "Cellular Quantification" = {
            data_l <- getCellularQuantificationOfStudies(data_src, study_ids)
          },
          "Nucleic Acid Quantification" = {
            data_l <- getNucleicAcidQuantificationOfStudies(data_src, study_ids)
          },
          "Titer Assay Results" = {
            data_l <- getTiterAssayResultsOfStudies(data_src, study_ids)
          }
  )
  data_l
  }

getAdverseEventsOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getAdverseEvents(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(ae_df=data_df, suppae_df=suppdata_df)
  
  data_l
}

getTrialArmsOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getTrialArms(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(ta_df=data_df, suppta_df=suppdata_df)
  
  data_l
}

getConcomitantMedicationsOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getConcomitantMedications(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(cm_df=data_df, suppcm_df=suppdata_df)
  
  data_l
}

getDemographicsOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getDemographics(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(dm_df=data_df, suppdm_df=suppdata_df)
  
  data_l
}

getTrialInclusionExclusionCriteriaOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getTrialInclusionExclusionCriteria(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(ti_df=data_df, suppti_df=suppdata_df)
  
  data_l
}

getExposureOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getExposure(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(ex_df=data_df, suppex_df=suppdata_df)
  
  data_l
}

getLaboratoryTestResultsOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getLaboratoryTestResults(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(lb_df=data_df, supplb_df=suppdata_df)
  
  data_l
}

getMedicalHistoryOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getMedicalHistory(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(mh_df=data_df, suppmh_df=suppdata_df)
  
  data_l
}

getAssociatedPersonsMedicalHistoryOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getAssociatedPersonsMedicalHistory(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(apmh_df=data_df, suppapmh_df=suppdata_df)
  
  data_l
}

getPhysicalExaminationOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getPhysicalExamination(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(pe_df=data_df, supppe_df=suppdata_df)
  
  data_l
}

getProtocolDeviationsOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getProtocolDeviations(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(dv_df=data_df, suppdv_df=suppdata_df)
  
  data_l
}

getTrialSummaryOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getTrialSummary(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(ts_df=data_df, suppts_df=suppdata_df)
  
  data_l
}

getSubjectVisitsOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getSubjectVisits(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(sv_df=data_df, suppsv_df=suppdata_df)
  
  data_l
}

getSubstanceUseOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getSubstanceUse(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(su_df=data_df, suppsu_df=suppdata_df)
  
  data_l
}

getVitalSignsOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getVitalSigns(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(vs_df=data_df, suppvs_df=suppdata_df)
  
  data_l
}

getQuestionnairesOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getQuestionnaires(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(qs_df=data_df, suppqs_df=suppdata_df)
  
  data_l
}

getFindingsAboutOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getFindingsAbout(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(fa_df=data_df, suppfa_df=suppdata_df)
  
  data_l
}

getSkinResponseOfStudies <- function(data_src, study_ids) {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getSkinResponse(data_src, study_id)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(sr_df=data_df, suppsr_df=suppdata_df)
  
  data_l
}

getGeneticsFindingsOfStudies <- function(data_src, study_ids, assay_type="ALL") {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getGeneticsFindings(data_src, study_id, assay_type)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(pf_df=data_df, supppf_df=suppdata_df)
  
  data_l
}

getProteinQuantificationOfStudies <- function(data_src, study_ids, assay_type="ALL") {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getProteinQuantification(data_src, study_id, assay_type)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(za_df=data_df, suppza_df=suppdata_df)
  
  data_l
}

getCellularQuantificationOfStudies <- function(data_src, study_ids, assay_type="ALL") {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getCellularQuantification(data_src, study_id, assay_type)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(zb_df=data_df, suppzb_df=suppdata_df)
  
  data_l
}

getNucleicAcidQuantificationOfStudies <- function(data_src, study_ids, assay_type="ALL") {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getNucleicAcidQuantification(data_src, study_id, assay_type)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(zc_df=data_df, suppzc_df=suppdata_df)
  
  data_l
}

getTiterAssayResultsOfStudies <- function(data_src, study_ids, assay_type="ALL") {
  data_df <- data.frame()
  suppdata_df <- data.frame()
  for (study_id in study_ids) {
    data_l <- getTiterAssayResults(data_src, study_id, assay_type)
    if (length(data_l) > 0) {
      data_df <- rbind(data_df, data_l[[1]])
      suppdata_df <- rbind(suppdata_df, data_l[[2]])
    }
  }
  
  data_l <- list()
  if (nrow(data_df) > 0)
    data_l <- list(zd_df=data_df, suppzd_df=suppdata_df)
  
  data_l
}


##' Get a list of Assay Types
##' 
##' The function \code{getListOfAssayTypes} returns a list of assay types that ImmPort studies have employed in their experimetal assays
##' 
##' @return A list of assay types
##' @examples
##'  at_l <- getListOfAssayTypes()
##' @export
getListOfAssayTypes <- function() {
  assay_type <- c(
    "ELISA",
    "ELISPOT",    
    "Array",    
    "PCR",    
    "HLA Typing",    
    "MBAA",    
    "HAI", 
    "Neut Ab Titer",
    "Flow"
  )
  assay_type
}


##' Get specific assay data of one or more studies from the ImmPort database
##' 
##' @param assay_type Assay Type
##' @param study_ids List of study indentifiers
##' @return a list of 1) domain data of specicifc assay technology and 2) any supplemental domain data of the studies
##' @author Ravi Shankar
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' elispot_l <- getAssayDataOfStudies("SDY139", "ELISPOT")
##' if (length(elispot_l) > 0)
##'   names(elispot_l)
##' head(elispot_l$zb_df)
##' @export
getAssayDataOfStudies <- function(study_ids, assay_type) {
  if (is.null(r <- get0("data_src", envir = RImmPort.env))) {
    stop("ImmPort data source is not set: See ?setImmPortDataSource() ")
  }
  
  data_src <- RImmPort.env$data_src
  
  if (assay_type %in% getListOfAssayTypes() == FALSE) {
    stop("assay type is not found in the list of supported assay types; 
         Run getListOfAssayTypes() for a complete list")
  }
  
  pq_l <- getProteinQuantificationOfStudies(data_src, study_ids, assay_type)
  cq_l <- getCellularQuantificationOfStudies(data_src, study_ids, assay_type)
  nq_l <- getNucleicAcidQuantificationOfStudies(data_src, study_ids, assay_type)
  ta_l <- getTiterAssayResultsOfStudies(data_src, study_ids, assay_type)
  gf_l <- getGeneticsFindingsOfStudies(data_src, study_ids, assay_type)
  
  data_l <- c(pq_l, cq_l, nq_l, ta_l, gf_l)
  
  data_l
  }


########################################################################
##' Get a list of studies that have specific domain data
##' 
##' @param domain Name of a specific domain
##' @param all_study_ids List of study indentifiers to search on
##' @return List of study indentifiers
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' study_ids <- getStudiesWithSpecificDomainData("Demographics")
##' @export
getStudiesWithSpecificDomainData <- function(domain, all_study_ids = c("ALL")) {
  study_ids = c()
  
  if (is.null(r <- get0("data_src", envir = RImmPort.env))) {
    stop("ImmPort data source is not set: See ?setImmPortDataSource() ")
  }
  
  data_src <- RImmPort.env$data_src
  
  if (domain %in% getListOfDomains()[,'Domain Name'] == FALSE) {
    stop(paste("domain = ", domain, " is not found in the list of supported domains; 
               Run getListOfDomains() for a complete list"))
  }
  
  if (class(data_src) == "character") { # data source is .rds files
    if (file.exists(data_src) == FALSE) {
      stop("data source = ", data_src, " not found")
    }  
    
    domain_code <- getDomainCode(domain)
    if (all_study_ids[1] == "ALL")
      all_study_ids <- getListOfStudies()
    
    for (study_id in all_study_ids) {
      study_path <- file.path(data_src, study_id)
      if (file.exists(file.path(study_path, paste(tolower(domain_code),".rds", sep=""))))
        study_ids <- c(study_ids, study_id)
    }
  } else { # data source is MySQL db or SQLite db
    switch (domain,          
            "Adverse Events" = {
              study_ids <- getStudiesWithAdverseEvents(data_src)
            },
            "Trial Arms" = {
              study_ids <- getStudiesWithTrialArms(data_src)
            },
            "Concomitant Medications" = {
              study_ids <- getStudiesWithConcomitantMedications(data_src)
            },
            "Demographics" = {
              study_ids <- getStudiesWithDemographics(data_src)
            },
            "Trial Inclusion Exclusion Criteria" = {
              study_ids <- getStudiesWithTrialInclusionExclusionCriteria(data_src)
            },
            "Exposure" = {
              study_ids <- getStudiesWithExposure(data_src)
            },
            "Laboratory Test Results" = {
              study_ids <- getStudiesWithLaboratoryTestResults(data_src)
            },
            "Medical History" = {
              study_ids <- getStudiesWithMedicalHistory(data_src)
            },
            "Associated Persons Medical History" = {
              study_ids <- getStudiesWithAssociatedPersonsMedicalHistory(data_src)
            },
            "Physical Examination" = {
              study_ids <- getStudiesWithPhysicalExamination(data_src)
            },
            "Protocol Deviations" = {
              study_ids <- getStudiesWithProtocolDeviations(data_src)
            },
            "Trial Summary" = {
              study_ids <- getStudiesWithTrialSummary(data_src)
            },
            "Subject Visits" = {
              study_ids <- getStudiesWithSubjectVisits(data_src)
            },
            "Substance Use" = {
              study_ids <- getStudiesWithSubstanceUse(data_src)
            },
            "Vital Signs" = {
              study_ids <- getStudiesWithVitalSigns(data_src)
            },
            "Questionnaires" = {
              study_ids <- getStudiesWithQuestionnaires(data_src)
            },
            "Findings About" = {
              study_ids <- getStudiesWithFindingsAbout(data_src)
            },
            "Skin Response" = {
              study_ids <- getStudiesWithSkinResponse(data_src)
            },        
            "Genetics Findings" = {
              study_ids <- getStudiesWithGeneticsFindings(data_src)
            },
            "Protein Quantification" = {
              study_ids <- getStudiesWithProteinQuantification(data_src)
            },
            "Cellular Quantification" = {
              study_ids <- getStudiesWithCellularQuantification(data_src)
            },
            "Nucleic Acid Quantification" = {
              study_ids <- getStudiesWithNucleicAcidQuantification(data_src)
            },
            "Titer Assay Results" = {
              study_ids <- getStudiesWithTiterAssayResults(data_src)
            }
    )
  }
  
  study_ids
  }

getStudiesWithAdverseEvents <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfAdverseEvents(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithTrialArms <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfTrialArms(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}


getStudiesWithConcomitantMedications <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfConcomitantMedications(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithDemographics <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfDemographics(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithTrialInclusionExclusionCriteria <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfTrialInclusionExclusionCriteria(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithExposure <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfExposure(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithLaboratoryTestResults <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfLaboratoryTestResults(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithMedicalHistory <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfMedicalHistory(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithAssociatedPersonsMedicalHistory <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfAssociatedPersonsMedicalHistory(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}
getStudiesWithPhysicalExamination <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfPhysicalExamination(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithProtocolDeviations <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfProtocolDeviations(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithTrialSummary <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfTrialSummary(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithSubjectVisits <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfSubjectVisits(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithSubstanceUse <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfSubstanceUse(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithVitalSigns <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfVitalSigns(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithQuestionnaires <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfQuestionnaires(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithFindingsAbout <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfFindingsAbout(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithSkinResponse <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfSkinResponse(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithGeneticsFindings <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfGeneticsFindings (data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithProteinQuantification <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfProteinQuantification(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithCellularQuantification <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfCellularQuantification(data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithNucleicAcidQuantification <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfNucleicAcidQuantification (data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

getStudiesWithTiterAssayResults <- function(data_src) {
  all_study_ids <- getListOfStudies()
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfTiterAssayResults (data_src, study_id) > 0)
      # add to list
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
}

########################################################################
##' Get a list of studies that have specific assay type data
##' 
##' @param assay_type Assay Type
##' @param all_study_ids List of study indentifiers to search on
##' @return List of study indentifiers
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' study_ids <- getStudiesWithSpecificAssayData("ELISPOT")
##' @export
getStudiesWithSpecificAssayData <- function(assay_type, all_study_ids = c("ALL")) {
  if (is.null(r <- get0("data_src", envir = RImmPort.env))) {
    stop("ImmPort data source is not set: See ?setImmPortDataSource() ")
  }
  
  data_src <- RImmPort.env$data_src
  
  if (assay_type %in% getListOfAssayTypes() == FALSE) {
    stop("assay type is not found in the list of supported assay types; 
         Run getListOfAssayTypes() for a complete list")
  }
  
  if (all_study_ids[1] == "ALL")
    all_study_ids <- getListOfStudies()
  
  study_ids = c()
  for (study_id in all_study_ids) {
    if (getCountOfCellularQuantification(data_src, study_id, assay_type) > 0)
      study_ids = c(study_ids,study_id)
    if (getCountOfGeneticsFindings(data_src, study_id, assay_type) > 0)
      study_ids = c(study_ids,study_id)
    if (getCountOfNucleicAcidQuantification(data_src, study_id, assay_type) > 0)
      study_ids = c(study_ids,study_id)
    if (getCountOfProteinQuantification(data_src, study_id, assay_type) > 0)
      study_ids = c(study_ids,study_id)
    if (getCountOfTiterAssayResults(data_src, study_id, assay_type) > 0)
      study_ids = c(study_ids,study_id)
  }
  
  study_ids
  }



########################################################################
########################################################################

##' Get a list of all studies 
##' 
##' @return List of study indentifiers
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' study_ids <- getListOfStudies()
##' @export
getListOfStudies <- function() {
  if (is.null(r <- get0("data_src", envir = RImmPort.env))) {
    stop("ImmPort data source is not set: See ?setImmPortDataSource() ")
  }
  
  data_src <- RImmPort.env$data_src
  if ((class(data_src)[1] == 'MySQLConnection') || 
      (class(data_src)[1] == 'SQLiteConnection')) {
    sql_stmt <- paste("
                      SELECT distinct
                      s.study_accession
                      FROM study s", sep="")
    
    study_ids_df <- dbGetQuery(data_src,statement=sql_stmt)
    study_ids <- study_ids_df[['study_accession']]
  } else {
    zip_files <- list.files(path = data_src, pattern = "\\.zip$")
    zip_files
    study_ids <- c()
    for (f in zip_files) {
      fname <- file_path_sans_ext(f)
      study_ids <- c(study_ids, fname)
    }
    
    #     study_dirs <- list.files(path = data_src, pattern = "_R$")
    #     study_ids <- c()
    #     for (sd in study_dirs) {
    #       study_id <- strsplit(sd, "-")[[1]][1]
    #       study_ids <- c(study_ids, study_id)
    #     }
  }
  
  study_ids
} 

##' Merge the Domain dataframe and Supplemental dataframe (long form) 
##' 
##' The Domain data list comprises of the the Domain datafrome that is in wide form, and any Supplemental
##' dataframe that is in long form. The function \code{mergeDomainAndSupplemental} transposes the Supplemental
##' dataframe into a wide form, and merges it with the Domain dataframe.
##' 
##' @param data_list A list of 1) Domain dataframe and 2) any Supplemental dataframe 
##' @return The merged dataframe
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' l <- getDomainDataOfStudies("Cellular Quantification", "SDY208")
##' df <- mergeDomainAndSupplemental(l)
##' @importFrom reshape2 dcast 
##' @importFrom dplyr full_join 
##' @export
mergeDomainAndSupplemental <- function(data_list) {
  merged_df <- data.frame()
  
  if (length(data_list) > 0) {
    merged_df <- data_list[[1]]
    if (length(data_list) == 2) {
      supp_df <- data_list[[2]]
      if (nrow(supp_df) > 0) {
        wide_supp_df <- dcast(supp_df, STUDYID + RDOMAIN + USUBJID + IDVARVAL ~ QNAM, value.var="QVAL")
        names(wide_supp_df)[names(wide_supp_df)=="RDOMAIN"] <- "DOMAIN"
        names(wide_supp_df)[names(wide_supp_df)=="IDVARVAL"] <- supp_df$IDVAR[1]
        merged_df <- full_join(merged_df, wide_supp_df)
      }
    } 
  }
  
  merged_df
}

########################################################################
########################################################################

##' Serialize the Study Data
##' 
##' Load specific studies from the database and save it in .rds format in 
##' a local file directory 
##' 
##' @param study_ids List of study indentifiers
##' @param data_dir Path to a file folder where the .rds study files will be saved into
##' @examples
##' library(DBI)
##' library(sqldf)
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' db_dir <- file.path(studies_dir, "Db")
##' sqlite_conn <- dbConnect(SQLite(), dbname=file.path(db_dir, "ImmPort.sqlite"))
##' setImmPortDataSource(sqlite_conn)
##' # the folder where the .rds files will be stored
##' rds_dir <- file.path(studies_dir, "Rds")
##' study_ids <- c('SDY139', 'SDY208')
##' serialzeStudyData(study_ids, rds_dir)
##' @export
serialzeStudyData  <- function(study_ids, data_dir) {
  if (is.null(r <- get0("data_src", envir = RImmPort.env))) {
    stop("ImmPort data source is not set: See ?setImmPortDataSource() ")
  }
  
  data_src <- RImmPort.env$data_src
  
  domains <- getListOfDomains()[,'Domain Name']
  for (study_id in study_ids) {
    cat("Study ID = ", study_id, "\n")
    for (domain in domains) {
      domain_code <- getDomainCode(domain)
      l <- getDomainDataOfStudies(domain, study_id)
      study_path <- file.path(data_dir, study_id)
      d <- dir.create(study_path, showWarnings = FALSE)
      
      if (length(l) > 0) {
        saveRDS(l[[1]], file=file.path(study_path, paste(tolower(domain_code),".rds", sep=""))) 
        saveRDS(l[[2]], file=file.path(study_path, paste("supp", tolower(domain_code),".rds", sep="")))
      }
    } # next domain
  } # next study_id
}

##' Load the Serialized Data of a Study
##' 
##' Load the serialzed data (.rds) file of a specific domain of a study study from the directory where 
##' the file is located
##' 
##' @param study_id Study indentifier
##' @param data_dir Path to a file folder where the .rds study files reside
##' @param domain Domain of interest
##' @return A study data object where in all data are structured as classes, domains, variables and values (in CDISC format)
##' 
##' @examples
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' # the folder where the .rds files will be stored
##' rds_dir <- file.path(studies_dir, "Rds")
##' # load the serialized data of study `SDY208` 
##' loadSerializedStudyData(rds_dir, 'SDY208', "Demographics")
##' @export
loadSerializedStudyData <- function(data_dir, study_id, domain) {
  study_path <- file.path(data_dir, study_id)
  if (file.exists(study_path) == FALSE) {
    stop("study data for id = ", study_id, " not found")
  }  
  if (domain %in% getListOfDomains()[,'Domain Name'] == FALSE) {
    stop("domain is not found in the list of supported domains; 
         Run getListOfDomains() for a complete list")
  }  
  domain_df <- data.frame()
  suppdomain_df <- data.frame()
  
  domain_code <- getDomainCode(domain)
  
  domain_file_path <- file.path(study_path, paste(tolower(domain_code),".rds", sep=""))
  cat("\ndomain_file_path = ", file.path(study_path, paste(tolower(domain_code),".rds", sep="")), "\n")
  if (file.exists(domain_file_path))
    domain_df <- readRDS(file=file.path(study_path, paste(tolower(domain_code),".rds", sep="")))
  
  suppdomain_file_path <- file.path(study_path, paste("supp", tolower(domain_code),".rds", sep=""))
  cat("suppdomain_file_path = ", file.path(study_path, paste("supp", tolower(domain_code),".rds", sep="")), "\n")
  if (file.exists(suppdomain_file_path))
    suppdomain_df <- readRDS(file=file.path(study_path, paste("supp", tolower(domain_code),".rds", sep="")))
  
  list(domain_df, suppdomain_df)
}

covertElaspsedTimeToISO8601Format <- function(time, time_unit) {
  if (is.na(time) | is.na(time_unit)) 
    return(NA)
  
  if (time_unit %in% c("Years", "Months", "Days")) {
    if (time < 0) {
      eltm <- "-P"
      time <- abs(time)
    } else
      eltm <- "P"
  }  
  
  if (time_unit %in% c("Hours", "Minutes", "Seconds")) {
    if (time < 0) {
      eltm <- "-PT"
      time <- abs(time)
    } else
      eltm <- "PT"
  }

  switch (time_unit,          
          "Years" = {
            if (time%%1 == 0) { 
              # no fraction
              eltm <- paste(eltm, as.integer(time), "Y", sep="")
            } else { 
              # has fraction
              eltm <- paste(eltm, time, "Y", sep="")
            }
          },  
          "Months" = {
            if (time%%1 == 0) { 
              # no fraction
              eltm <- paste(eltm, as.integer(time), "M", sep="")
            } else { 
              # has fraction
              eltm <- paste(eltm, time, "M", sep="")
            }
          },  
          "Days" = {
            if (time%%1 == 0) { 
              # no fraction
              eltm <- paste(eltm, as.integer(time), "D", sep="")
            } else { 
              # has fraction
              eltm <- paste(eltm, time, "D", sep="")
            }
          },  
          "Hours" = {
            if (time%%1 == 0) { 
              # no fraction
              eltm <- paste(eltm, as.integer(time), "H", sep="")
            } else { 
              # has fraction
              eltm <- paste(eltm, time, "H", sep="")
            }
          },
          "Minutes" = {
            if (time%%1 == 0) { 
              # no fraction
              eltm <- paste(eltm, as.integer(time), "M", sep="")
            } else { 
              # has fraction
              eltm <- paste(eltm, time, "M", sep="")
            }
          },
          "Seconds" = {
            if (time%%1 == 0) { 
              # no fraction
              eltm <- paste(eltm, as.integer(time), "S", sep="")
            } else { 
              # has fraction
              eltm <- paste(eltm, time, "S", sep="")
            }
          },
          {
            eltm <- NA
          }
  )
  
  eltm
}

getTimePointReference <- function(time_event, time_event_specify) {
  if (time_event == "Other") 
    return(time_event_specify)
  else
    return(time_event)
}

