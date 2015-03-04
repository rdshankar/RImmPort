library(RMySQL)
library (plyr)              

SpecialPurpose <- setRefClass("SpecialPurpose", 
                             fields = list(
                               demographics="data.frame",
                               subject_visits="data.frame"),
                             methods = list(
                               getSpecialPurpose = function(conn,study_id) {
                                 demographics <<- getDemographics(conn,study_id)  
                                 subject_visits <<- getSubjectVisits(conn,study_id)                                 
                               }
                             ))

Interventions <- setRefClass("Interventions", 
                                fields = list(
                                  concomitant_medications="data.frame",
                                  exposure="data.frame",
                                  procedures="data.frame",
                                  substance_use="data.frame"),
                                methods = list(
                                  getInterventions = function(conn,study_id) {
                                    exposure <<- getExposure(conn, study_id)
                                    concomitant_medications <<- getConcomitantMedications(conn, study_id)
                                    substance_use <<- getSubstanceUse(conn, study_id)
                                  }
                              ))

Events <- setRefClass("Events", fields = list(
                                  adverse_events="data.frame",
                                  clinical_events="data.frame",
                                  disposition="data.frame",
                                  protocol_deviations="data.frame",
                                  healthcare_encounters="data.frame",
                                  medical_history ="data.frame"),
                                methods = list(
                                  getEvents = function(conn,study_id) {
                                    adverse_events <<- getAdverseEvents(conn, study_id)
                                    protocol_deviations <<- getProtocolDeviations(conn, study_id)
                                    medical_history <<- getMedicalHistory(conn, study_id)
                                  }
                                ))

Findings <- setRefClass("Findings", 
                        fields = list(
                                drug_accountability="data.frame",
                                death_details="data.frame",
                                ecg_test_results="data.frame",
                                ie_not_met="data.frame",
                                immunogenicity_specimen_assessment="data.frame",
                                laboratory_test_results="data.frame",
                                microbiology_specimen="data.frame",
                                microbiology_susceptibility="data.frame",
                                microscopic_findings="data.frame",
                                pharmacokinetics_concentrations="data.frame",
                                pharmacokinetics_parameters="data.frame",
                                physical_examination="data.frame",
                                questionnaires="data.frame",
                                reproductive_system_findings="data.frame", 
                                subject_characteristics="data.frame",
                                subject_status="data.frame",
                                vital_signs="data.frame",
                                findings_about="data.frame",
                                skin_response="data.frame",
                                hai_results="data.frame",
                                elisa_results="data.frame",
                                elispot_results="data.frame",
                                neut_ab_titer_results="data.frame",
                                hla_typing_results="data.frame",
                                mbaa_results="data.frame",
                                pcr_results="data.frame",
                                fcs_results="data.frame",
                                array_results="data.frame"),
                        methods = list(
                          getFindings = function(conn,study_id) {
                            laboratory_test_results <<- getLaboratoryTestResults(conn, study_id)
                            vital_signs <<- getVitalSigns(conn, study_id)
                            physical_examination <<- getPhysicalExamination(conn, study_id)
                            findings_about <<- getFindingsAbout(conn, study_id)
                            hai_results <<- getHaiResults(conn, study_id)
                            elisa_results <<- getElisaResults(conn, study_id)
                            elispot_results <<- getElispotResults(conn, study_id)
                            neut_ab_titer_results <<- getNeutAbTiterResults(conn, study_id)
                            hla_typing_results <<- getHlaTypingResults(conn, study_id)
                            mbaa_results <<- getMbaaResults(conn, study_id)
                            pcr_results <<- getPcrResults(conn, study_id)
                            fcs_results <<- getFcsResults(conn, study_id)
                            array_results <<- getArrayResults(conn, study_id)
                          }
                        ))

StudyDetails <- setRefClass("StudyDetails", 
                        fields = list(
                          arms="data.frame",
                          summary="data.frame",
                          eligibility_criteria="data.frame"),
                        methods = list(
                          getStudyDetails = function(conn,study_id) {
                            arms <<- getArms(conn, study_id)
                            summary <<- getStudySummary(conn, study_id) 
                            eligibility_criteria <<- getEligibilityCriteria(conn, study_id) 
                          }
                        ))
Analyses <- setRefClass("Analyses", 
                        fields = list(
                          subject_basic_analyses="data.frame"),
                        methods = list(
                          getAnalyses = function(conn,study_id) {
                            subject_basic_analyses <<- getSubjectBasicAnalyses(conn, study_id)
                          }
                        ))

Study <- setRefClass("Study", fields = list(
  special_purpose="SpecialPurpose",
  interventions="Interventions",
  events="Events",
  findings="Findings",
  study_details="StudyDetails",
  analyses="Analyses"),
  methods = list(
    getTableOfContents = function() {
      contents.l <- list(
                    r1=list(c1="Special Purpose", c2="Demographics", c3=""), 
                    r2=list(c1="Special Purpose", c2="Subject Visits", c3=""), 
                    r3=list(c1="Special Purpose", c2="Study Elements", c3=""), 
                    r4=list(c1="Interventions", c2="Concomitant Medications", c3=""), 
                    r5=list(c1="Interventions", c2="Exposure", c3=""), 
                    r6=list(c1="Interventions", c2="Procedures", c3=""), 
                    r7=list(c1="Interventions", c2="Substance Use", c3=""), 
                    r8=list(c1="Events", c2="Adverse Events", c3=""), 
                    r9=list(c1="Events", c2="Clinical Events", c3=""), 
                    r10=list(c1="Events", c2="Disposition", c3=""), 
                    r11=list(c1="Events", c2="Protocol Deviations", c3=""), 
                    r12=list(c1="Events", c2="Healthcare Encounters", c3=""), 
                    r13=list(c1="Events", c2="Medical History", c3=""), 
                    r14=list(c1="Events", c2="Family History", c3=""), 
                    r15=list(c1="Findings", c2="Drug Accountability", c3=""), 
                    r16=list(c1="Findings", c2="Death Details", c3=""), 
                    r17=list(c1="Findings", c2="ECG Test Results", c3=""), 
                    r18=list(c1="Findings", c2="Inclusion/Exclusion Criteria Met", c3=""), 
                    r19=list(c1="Findings", c2="Immunogenicity Specimen Assessments", c3=""), 
                    r20=list(c1="Findings", c2="Laboratory Test Results", c3=""), 
                    r21=list(c1="Findings", c2="Microbiology Specimen", c3=""), 
                    r22=list(c1="Findings", c2="Microbiology Susceptibility", c3=""), 
                    r23=list(c1="Findings", c2="Microscopic Findings", c3=""), 
                    r24=list(c1="Findings", c2="Morphology", c3=""), 
                    r25=list(c1="Findings", c2="Pharmacokinetics Concentrations", c3=""), 
                    r26=list(c1="Findings", c2="Pharmacokinetics Parameters", c3=""), 
                    r27=list(c1="Findings", c2="Physical Examination", c3=""), 
                    r28=list(c1="Findings", c2="Questionnaires", c3=""), 
                    r29=list(c1="Findings", c2="Reproductive System Findings", c3=""), 
                    r30=list(c1="Findings", c2="Subject Characteristics", c3=""), 
                    r31=list(c1="Findings", c2="Subject Status", c3=""), 
                    r32=list(c1="Findings", c2="Vital Signs", c3=""), 
                    r33=list(c1="Findings", c2="Findings About", c3=""), 
                    r34=list(c1="Findings", c2="HAI Results", c3=""), 
                    r35=list(c1="Findings", c2="ELISA Results", c3=""), 
                    r36=list(c1="Findings", c2="ELISPOT Results", c3=""), 
                    r37=list(c1="Findings", c2="Neut Ab Titer Results", c3=""),                     
                    r38=list(c1="Findings", c2="HLA Typing Results", c3=""), 
                    r39=list(c1="Findings", c2="PCR Results", c3=""), 
                    r40=list(c1="Findings", c2="FCS Results", c3=""), 
                    r41=list(c1="Findings", c2="Array Results", c3=""), 
                    r42=list(c1="Analyses", c2="Subject Basic Analyses", c3=""), 
                    r43=list(c1="Study Details", c2="Study Visits", c3=""), 
                    r44=list(c1="Study Details", c2="Study Assessments", c3=""), 
                    r45=list(c1="Study Details", c2="Study Eligibility Criteria", c3=""), 
                    r46=list(c1="Study Details", c2="Study Arms", c3=""), 
                    r47=list(c1="Study Details", c2="Study Summary", c3=""))
    
      contents.df <- ldply (contents.l, data.frame, stringsAsFactors = FALSE)
      
      contents.df$.id <- NULL
      colnames(contents.df) <- c("Class", "Domain", "Included?")
      
      
      
      if (nrow(special_purpose$demographics) > 0)
        contents.df[contents.df$Domain == "Demographics",3] <- "yes"
      if (nrow(special_purpose$subject_visits) > 0)
        contents.df[contents.df$Domain == "Subject Visits",3] <- "yes"
      if (nrow(interventions$concomitant_medications) > 0)
        contents.df[contents.df$Domain == "Concomitant Medications",3] <- "yes"
      if (nrow(interventions$exposure) > 0)
        contents.df[contents.df$Domain == "Exposure",3] <- "yes"
      if (nrow(interventions$procedures) > 0)
        contents.df[contents.df$Domain == "Procedures",3] <- "yes"
      if (nrow(interventions$substance_use) > 0)
        contents.df[contents.df$Domain == "Substance Use",3] <- "yes"
      if (nrow(events$adverse_events) > 0)
        contents.df[contents.df$Domain == "Adverse Events",3] <- "yes"
      if (nrow(events$protocol_deviations) > 0)
        contents.df[contents.df$Domain == "Protocol Deviations",3] <- "yes"
      if (nrow(events$medical_history) > 0)
        contents.df[contents.df$Domain == "Medical History",3] <- "yes"
      if (nrow(findings$laboratory_test_results) > 0)
        contents.df[contents.df$Domain == "Laboratory Test Results",3] <- "yes"
      if (nrow(findings$vital_signs) > 0)
        contents.df[contents.df$Domain == "Vital Signs",3] <- "yes"
      if (nrow(findings$findings_about) > 0)
        contents.df[contents.df$Domain == "Findings About",3] <- "yes"
      if (nrow(findings$hai_results) > 0)
        contents.df[contents.df$Domain == "HAI Results",3] <- "yes"      
      if (nrow(findings$elisa_results) > 0)
        contents.df[contents.df$Domain == "ELISA Results",3] <- "yes"
      if (nrow(findings$elispot_results) > 0)
        contents.df[contents.df$Domain == "ELISPOT Results",3] <- "yes"      
      if (nrow(findings$neut_ab_titer_results) > 0)
        contents.df[contents.df$Domain == "Neut Ab Titer Results",3] <- "yes"      
      if (nrow(findings$hla_typing_results) > 0)
        contents.df[contents.df$Domain == "HLA Typing Results",3] <- "yes"      
      if (nrow(findings$pcr_results) > 0)
        contents.df[contents.df$Domain == "PCR Results",3] <- "yes"      
      if (nrow(findings$fcs_results) > 0)
        contents.df[contents.df$Domain == "FCS Results",3] <- "yes"            
      if (nrow(findings$array_results) > 0)
        contents.df[contents.df$Domain == "Array Results",3] <- "yes"      
      if (nrow(analyses$subject_basic_analyses) > 0)
        contents.df[contents.df$Domain == "Subject Basic Analyses",3] <- "yes"
      if (nrow(study_details$eligibility_criteria) > 0)
        contents.df[contents.df$Domain == "Study Eligibility Criteria",3] <- "yes"      
      if (nrow(study_details$arms) > 0)
        contents.df[contents.df$Domain == "Study Arms",3] <- "yes"
      if (nrow(study_details$summary) > 0)
        contents.df[contents.df$Domain == "Study Summary",3] <- "yes"
      
      contents.df
    }
))

getStudyFromDatabase <- function(conn,study_id) {
  cat("loading Study ID = ", study_id, "\n")
  
  study <- Study$new()
  
  study$special_purpose$getSpecialPurpose(conn, study_id)
  study$interventions$getInterventions(conn, study_id) 
  study$events$getEvents(conn, study_id)
  study$findings$getFindings(conn, study_id)
  study$study_details$getStudyDetails(conn, study_id)  
  study$analyses$getAnalyses(conn, study_id)
  
cat("done loading Study ID = ", study_id, "\n")

  study
}

getDemographicsInStudies <- function(conn,study_ids) {
  all.df <- data.frame()
  for (study_id in study_ids) {
    all.df <- rbind(all.df, getDemographics(conn, study_id))
  }
  all.df
}

getStudySummaryInStudies <- function(conn,study_ids) {
  all.ss <- data.frame()
  for (study_id in study_ids) {
    all.ss <- rbind(all.ss, getStudySummary(conn, study_id))
  }
  all.ss
}

getListOfStudyIds <- function(conn) {
  sql_stmt <- paste("
                SELECT distinct
                  s.study_accession
                  FROM study s", sep="")
  
  study_ids <- dbGetQuery(conn,statement=sql_stmt)
  study_ids[['study_accession']]
} 

loadAndSaveStudyData  <- function(conn,study_ids, data_dir) {
  for (study_id in study_ids) {
    study = getStudyFromDatabase(conn, study_id) 
    saveRDS(study, file=paste(data_dir, gsub("'", '', study_id),".rds", sep="")) 
  }
}

getStudy <- function(study_id, data_dir) {
  readRDS(paste(data_dir, gsub("'", '', study_id),".rds", sep=""))
}
