db_idx_stmts <- c(
  "CREATE INDEX idx_actual_visit_subject ON actual_visit(subject_accession)", 
  "CREATE INDEX idx_actual_visit_study ON actual_visit(study_accession)", 
  "CREATE INDEX idx_actual_visit_planned_visit ON actual_visit(planned_visit_accession)", 
  "CREATE INDEX idx_actual_visit_workspace ON actual_visit(workspace_id)", 
  "CREATE INDEX idx_adverse_event_subject ON adverse_event(subject_accession)", 
  "CREATE INDEX idx_adverse_event_study ON adverse_event(study_accession)", 
  "CREATE INDEX idx_adverse_event_workspace ON adverse_event(workspace_id)", 
  "CREATE INDEX idx_arm_2_subject_1 ON arm_2_subject(subject_accession, arm_accession)", 
  "CREATE INDEX idx_arm_or_cohort_study ON arm_or_cohort(study_accession, arm_accession)", 
  "CREATE INDEX idx_arm_or_cohort_workspace ON arm_or_cohort(workspace_id)", 
  "CREATE INDEX idx_assessment_2_file_info ON assessment_2_file_info(file_info_id, assessment_panel_accession)", 
  "CREATE INDEX idx_assessment_study ON assessment(study_accession)", 
  "CREATE INDEX idx_assessment_subject ON assessment(subject_accession)", 
  "CREATE INDEX idx_assessment_actual_visit ON assessment(actual_visit_accession)", 
  "CREATE INDEX idx_assessment_workspace ON assessment(workspace_id)", 
  "CREATE INDEX idx_biosample_2_expsample ON biosample_2_expsample(expsample_accession, biosample_accession)", 
  "CREATE INDEX idx_biosample_2_protocol ON biosample_2_protocol(protocol_accession, biosample_accession)", 
  "CREATE INDEX idx_biosample_2_treatment ON biosample_2_treatment(treatment_accession, biosample_accession)", 
  "CREATE INDEX idx_biosample_subject ON biosample(subject_accession)", 
  "CREATE INDEX idx_biosample_study ON biosample(study_accession)", 
  "CREATE INDEX idx_biosample_actual_visit ON biosample(actual_visit_accession)", 
  "CREATE INDEX idx_biosample_workspace ON biosample(workspace_id)", 
  "CREATE INDEX idx_contract_program ON contract_grant(program_id)", 
  "CREATE INDEX idx_ctrlsample_2_file_info ON control_sample_2_file_info(file_info_id, control_sample_accession)", 
  "CREATE INDEX idx_ctrlsample_experiment ON control_sample(experiment_accession)", 
  "CREATE INDEX idx_ctrlsample_workspace ON control_sample(workspace_id)", 
  "CREATE INDEX idx_elias_study_accession ON elisa_result(study_accession)", 
  "CREATE INDEX idx_elias_arm_accession ON elisa_result(arm_accession)", 
  "CREATE INDEX idx_elisa_expsample_accession ON elisa_result(expsample_accession)", 
  "CREATE INDEX idx_elisa_experiment_accession ON elisa_result(experiment_accession)", 
  "CREATE INDEX idx_elisa_subject_accession ON elisa_result(subject_accession)", 
  "CREATE INDEX idx_elisa_biosample_accession ON elisa_result(biosample_accession)", 
  "CREATE INDEX idx_elisa_analyte ON elisa_result(analyte)", 
  "CREATE INDEX idx_elisa_workspace ON elisa_result(workspace_id)", 
  "CREATE INDEX idx_elispot_study_accession ON elispot_result(study_accession)", 
  "CREATE INDEX idx_elispot_arm_accession ON elispot_result(arm_accession)", 
  "CREATE INDEX idx_elispot_expsample_accession ON elispot_result(expsample_accession)", 
  "CREATE INDEX idx_elispot_experiment_accession ON elispot_result(experiment_accession)", 
  "CREATE INDEX idx_elispot_subject_accession ON elispot_result(subject_accession)", 
  "CREATE INDEX idx_elispot_biosample_accession ON elispot_result(biosample_accession)", 
  "CREATE INDEX idx_elispot_analyte ON elispot_result(analyte)", 
  "CREATE INDEX idx_elispot_workspace ON elispot_result(workspace_id)", 
  "CREATE INDEX idx_experiment_2_protocol ON experiment_2_protocol(protocol_accession, experiment_accession)", 
  "CREATE INDEX idx_experiment_workspace ON experiment(workspace_id)", 
  "CREATE INDEX idx_expsample_2_file_info ON expsample_2_file_info(file_info_id, expsample_accession)", 
  "CREATE INDEX idx_expsample_2_reagent ON expsample_2_reagent(reagent_accession, expsample_accession)", 
  "CREATE INDEX idx_expsample_2_treatment ON expsample_2_treatment(treatment_accession, expsample_accession)", 
  "CREATE INDEX idx_expsample_mbaa_experiment ON expsample_mbaa_detail(experiment_accession, expsample_accession)", 
  "CREATE INDEX idx_expsample_mbaa_workspace ON expsample_mbaa_detail(workspace_id)", 
  "CREATE INDEX idx_expsample_public ON expsample_public_repository(experiment_accession, expsample_accession)", 
  "CREATE INDEX idx_expsample_experiment ON expsample(experiment_accession, expsample_accession)", 
  "CREATE INDEX idx_expsample_workspace ON expsample(workspace_id)", 
  "CREATE INDEX idx_fcs_study_accession ON fcs_analyzed_result(study_accession)", 
  "CREATE INDEX idx_fcs_arm_accession ON fcs_analyzed_result(arm_accession)", 
  "CREATE INDEX idx_fcs_expsample_accession ON fcs_analyzed_result(expsample_accession)", 
  "CREATE INDEX idx_fcs_experiment_accession ON fcs_analyzed_result(experiment_accession)", 
  "CREATE INDEX idx_fcs_subject_accession ON fcs_analyzed_result(subject_accession)", 
  "CREATE INDEX idx_fcs_biosample_accession ON fcs_analyzed_result(biosample_accession)", 
  "CREATE INDEX idx_fcs_experiment_workspace ON fcs_analyzed_result(workspace_id)", 
  "CREATE INDEX idx_fcs_header_expsample_accession ON fcs_header(expsample_accession)", 
  "CREATE INDEX idx_fcs_header_experiment_accession ON fcs_header(experiment_accession)", 
  "CREATE INDEX idx_fcs_header_file_info_id ON fcs_header(file_info_id)", 
  "CREATE INDEX idx_file_info_workspace ON file_info(workspace_id)", 
  "CREATE INDEX idx_hai_study_accession ON hai_result(study_accession)", 
  "CREATE INDEX idx_hai_arm_accession ON hai_result(arm_accession)", 
  "CREATE INDEX idx_hai_expsample_accession ON hai_result(expsample_accession)", 
  "CREATE INDEX idx_hai_experiment_accession ON hai_result(experiment_accession)", 
  "CREATE INDEX idx_hai_subject_accession ON hai_result(subject_accession)", 
  "CREATE INDEX idx_hai_biosample_accession ON hai_result(biosample_accession)", 
  "CREATE INDEX idx_hai_virus_strain ON hai_result(virus_strain)", 
  "CREATE INDEX idx_hai_workspace ON hai_result(workspace_id)", 
  "CREATE INDEX idx_hla_allele_reagent_accession ON hla_allele_status(reagent_accession)", 
  "CREATE INDEX idx_hla_study_accession ON hla_typing_result(study_accession)", 
  "CREATE INDEX idx_hla_arm_accession ON hla_typing_result(arm_accession)", 
  "CREATE INDEX idx_hla_expsample_accession ON hla_typing_result(expsample_accession)", 
  "CREATE INDEX idx_hla_experiment_accession ON hla_typing_result(experiment_accession)", 
  "CREATE INDEX idx_hla_subject_accession ON hla_typing_result(subject_accession)", 
  "CREATE INDEX idx_hla_biosample_accession ON hla_typing_result(biosample_accession)", 
  "CREATE INDEX idx_hla_workspace ON hla_typing_result(workspace_id)", 
  "CREATE INDEX idx_hla_typing_reagent_accession ON hla_typing_sys_feature(reagent_accession)", 
  "CREATE INDEX idx_hla_system_reagent_accession ON hla_typing_system(reagent_accession)", 
  "CREATE INDEX idx_inc_exc_study ON inclusion_exclusion(study_accession)", 
  "CREATE INDEX idx_inc_exc_workspace ON inclusion_exclusion(workspace_id)", 
  "CREATE INDEX idx_kir_study_accession ON kir_typing_result(study_accession)", 
  "CREATE INDEX idx_kir_arm_accession ON kir_typing_result(arm_accession)", 
  "CREATE INDEX idx_kir_expsample_accession ON kir_typing_result(expsample_accession)", 
  "CREATE INDEX idx_kir_experiment_accession ON kir_typing_result(experiment_accession)", 
  "CREATE INDEX idx_kir_subject_accession ON kir_typing_result(subject_accession)", 
  "CREATE INDEX idx_kir_biosample_accession ON kir_typing_result(biosample_accession)", 
  "CREATE INDEX idx_kir_workspace ON kir_typing_result(workspace_id)", 
  "CREATE INDEX idx_kir_system_reagent_accession ON kir_typing_system(reagent_accession)", 
  "CREATE INDEX idx_lab_test_2_protocol ON lab_test_2_protocol(protocol_accession, lab_test_accession)", 
  "CREATE INDEX idx_lab_test_study ON lab_test(study_accession)", 
  "CREATE INDEX idx_lab_test_biosample ON lab_test(biosample_accession)", 
  "CREATE INDEX idx_lab_test_workspace ON lab_test(workspace_id)", 
  "CREATE INDEX idx_mbaa_study_accession ON mbaa_result(study_accession)", 
  "CREATE INDEX idx_mbaa_arm_accession ON mbaa_result(arm_accession)", 
  "CREATE INDEX idx_mbaa_source_accession ON mbaa_result(source_accession)", 
  "CREATE INDEX idx_mbaa_experiment_accession ON mbaa_result(experiment_accession)", 
  "CREATE INDEX idx_mbaa_subject_accession ON mbaa_result(subject_accession)", 
  "CREATE INDEX idx_mbaa_biosample_accession ON mbaa_result(biosample_accession)", 
  "CREATE INDEX idx_mbaa_workspace ON mbaa_result(workspace_id)", 
  "CREATE INDEX idx_neut_study_accession ON neut_ab_titer_result(study_accession)", 
  "CREATE INDEX idx_neut_arm_accession ON neut_ab_titer_result(arm_accession)", 
  "CREATE INDEX idx_neut_expsample_accession ON neut_ab_titer_result(expsample_accession)", 
  "CREATE INDEX idx_neut_experiment_accession ON neut_ab_titer_result(experiment_accession)", 
  "CREATE INDEX idx_neut_subject_accession ON neut_ab_titer_result(subject_accession)", 
  "CREATE INDEX idx_neut_biosample_accession ON neut_ab_titer_result(biosample_accession)", 
  "CREATE INDEX idx_nuet_virus_strain ON neut_ab_titer_result(virus_strain)", 
  "CREATE INDEX idx__workspace ON neut_ab_titer_result(workspace_id)", 
  "CREATE INDEX idx_pcr_study_accession ON pcr_result(study_accession)", 
  "CREATE INDEX idx_pcr_arm_accession ON pcr_result(arm_accession)", 
  "CREATE INDEX idx_pcr_expsample_accession ON pcr_result(expsample_accession)", 
  "CREATE INDEX idx_pcr_experiment_accession ON pcr_result(experiment_accession)", 
  "CREATE INDEX idx_pcr_subject_accession ON pcr_result(subject_accession)", 
  "CREATE INDEX idx_pcr_biosample_accession ON pcr_result(biosample_accession)", 
  "CREATE INDEX idx_pcr_entrez ON pcr_result(entrez_gene_id)", 
  "CREATE INDEX idx_pcr_workspace ON pcr_result(workspace_id)", 
  "CREATE INDEX idx_period_study ON period(study_accession)", 
  "CREATE INDEX idx_period_workspace ON period(workspace_id)", 
  "CREATE INDEX idx_planned_visit_2_arm ON planned_visit_2_arm(arm_accession)", 
  "CREATE INDEX idx_planned_visit_workspace ON planned_visit(workspace_id)", 
  "CREATE INDEX idx_planned_visit_period ON planned_visit(period_accession)", 
  "CREATE INDEX idx_procotol_deviation_workspace ON protocol_deviation(workspace_id)", 
  "CREATE INDEX idx_procotol_deviation_subject ON protocol_deviation(subject_accession)", 
  "CREATE INDEX idx_procotol_deviation_study ON protocol_deviation(study_accession)", 
  "CREATE INDEX idx_protocol_type ON protocol(type)", 
  "CREATE INDEX idx_protocol_workspace ON protocol(workspace_id)", 
  "CREATE INDEX idx_reagent_set_reagent ON reagent_set_2_reagent(reagent_accession)", 
  "CREATE INDEX idx_reagent_set_workspace ON reagent_set_2_reagent(workspace_id)", 
  "CREATE INDEX idx_reagent_workspace ON reagent(workspace_id)", 
  "CREATE INDEX idx_reference_range_workspace ON reference_range(workspace_id)", 
  "CREATE INDEX idx_reference_range_study ON reference_range(study_accession)", 
  "CREATE INDEX idx_early_termination_workspace ON reported_early_termination(workspace_id)", 
  "CREATE INDEX idx_early_termination_study ON reported_early_termination(study_accession)", 
  "CREATE INDEX idx_early_termination_subject ON reported_early_termination(subject_accession)", 
  "CREATE INDEX idx_stdcurve_2_file_info ON standard_curve_2_file_info(file_info_id, standard_curve_accession)", 
  "CREATE INDEX idx_stdcurve_experiment ON standard_curve(experiment_accession)", 
  "CREATE INDEX idx_stdcurve_workspace ON standard_curve(workspace_id)", 
  "CREATE INDEX idx_study_2_protocol ON study_2_protocol(protocol_accession, study_accession)", 
  "CREATE INDEX idx_study_file_type ON study_file(study_file_type)", 
  "CREATE INDEX idx_study_file_workspace ON study_file(workspace_id)", 
  "CREATE INDEX idx_study_file_study ON study_file(study_accession)", 
  "CREATE INDEX idx_study_image_workspace ON study_image(workspace_id)", 
  "CREATE INDEX idx_study_image_study ON study_image(study_accession)", 
  "CREATE INDEX idx_study_link_workspace ON study_link(workspace_id)", 
  "CREATE INDEX idx_study_link_study ON study_link(study_accession)", 
  "CREATE INDEX idx_study_personnel_workspace ON study_personnel(workspace_id)", 
  "CREATE INDEX idx_study_personnel_study ON study_personnel(study_accession)", 
  "CREATE INDEX idx_study_pubmed_workspace ON study_pubmed(workspace_id)", 
  "CREATE INDEX idx_study_pubmed_pubmed_id ON study_pubmed(pubmed_id)", 
  "CREATE INDEX idx_study_type ON study(type)", 
  "CREATE INDEX idx_study_workspace ON study(workspace_id)", 
  "CREATE INDEX idx_subject_measure_workspace ON subject_measure_definition(workspace_id)", 
  "CREATE INDEX idx_subject_measure_study ON subject_measure_definition(study_accession)", 
  "CREATE INDEX idx_subject_measure_result_workspace ON subject_measure_result(workspace_id)", 
  "CREATE INDEX idx_subject_measure_result_study ON subject_measure_result(study_accession)", 
  "CREATE INDEX idx_subject_gender ON subject(gender)", 
  "CREATE INDEX idx_subject_race ON subject(race)", 
  "CREATE INDEX idx_subject_species ON subject(species)", 
  "CREATE INDEX idx_subject_workspace ON subject(workspace_id)", 
  "CREATE INDEX idx_substance_merge_study ON substance_merge(study_accession)", 
  "CREATE INDEX idx_substance_merge_workspace ON substance_merge(workspace_id)", 
  "CREATE INDEX idx_substance_subject ON substance_merge(subject_accession)", 
  "CREATE INDEX idx_treatment_workspace ON treatment(workspace_id)", 
  "CREATE INDEX idx_workspace_contract ON workspace(contract_id)")

##' buildNewSqliteDb
##' 
##' The function \code{buildSqliteDb} builds a sqlite db of ImmPort study data. It takes in as input the 
##' study files in the TSV (Tab) format. 
##' 
##' @param data_dir File directory where the study TSV files are stored
##' @param db_dir File directory where the sqlite database will be stored
##' @examples
##' studies_dir <- system.file("extdata", "ImmPortStudies", package = "RImmPort")
##' # set tab_dir to the folder where the zip files are located
##' tab_dir <- file.path(studies_dir, "Tab")
##' # set db_dir to the folder where the database file 'ImmPort.sqlite' should be stored
##' db_dir <- file.path(studies_dir, "Db")
##' # build a new ImmPort SQLite database with the data in the downloaded zip files
##' # buildNewSqliteDb(tab_dir, db_dir) 
##' @importFrom DBI dbGetQuery dbConnect
##' @importFrom RSQLite SQLite
##' @importFrom RSQLite dbWriteTable
##' @importFrom sqldf sqldf
##' @importFrom tools file_path_sans_ext
##' @importFrom data.table setnames
##' @export
##' 
buildNewSqliteDb <- function(data_dir, db_dir) {
  dbname="ImmPort.sqlite"
  
  prevWD <- getwd()
  on.exit(setwd(prevWD))
  
  setwd(db_dir)
  if (file.exists(dbname)) {
     stop(paste("ImmPort.sqlite database already exists. Please remove the file ImmPort.sqlite in ", 
                db_dir, " before building a new ImmPort database"))
  }
  
  sqldf(paste("attach \'", dbname, "\' as new", sep=""), drv = "SQLite") 
  
  db <- dbConnect(SQLite(), dbname)
  
  setwd(data_dir)
  
  # unzip study files into data directory
  zipfiles <- list.files(pattern = "Tab\\.zip$")
  for (zf in zipfiles) {
#      zipname <- file_path_sans_ext(zf)
#      study_dir <- file.path(data_dir, zipname)
#      dir.create(study_dir)
    unzip(zf)
  }
     
  # process each study dir and load data into SQLite db
  study_dirs <- list.files(pattern = "Tab$")
  for (sd in study_dirs) {
    study_dir <- file.path(data_dir, sd)
    cat("study dir = ", study_dir, "\n")
    if (dir.exists(study_dir) == FALSE) {
      cat("skipping = ", study_dir, "\n")
      next
    }
    
    if (dir.exists(file.path(study_dir, "Tab")))
      setwd(file.path(study_dir, "Tab"))
    else
      setwd(study_dir)
    cat("processing files of study dir = ", study_dir, "\n")
    files <- list.files(pattern = "\\.txt$")
    for (f in files) {
      fname <- file_path_sans_ext(f)
      df <- read.table(f, sep="\t", header=TRUE, fill=TRUE, quote = "", stringsAsFactors = FALSE)
      if (fname == 'assessment') {
        if ("AGE_AT_ONSET_REPOPRTED" %in% colnames(df)) 
          setnames(df, "AGE_AT_ONSET_REPOPRTED", "AGE_AT_ONSET_REPORTED")
        if ("PANEL_NAME_PREFERED" %in% colnames(df)) 
          setnames(df, "PANEL_NAME_PREFERED", "PANEL_NAME_PREFERRED")
      }
      if (fname == 'fcs_analyzed_result') {
        if ("STUDY_ACCSSION" %in% colnames(df)) 
          setnames(df, "STUDY_ACCSSION", "STUDY_ACCESSION")
#         if ("POPULATION_DEFNITION_REPORTED" %in% colnames(df)) 
#           setnames(df, "POPULATION_DEFNITION_REPORTED", "POPULATION_DEFINITION_REPORTED")
#         if ("POPULATION_DEFNITION_PREFERRED" %in% colnames(df)) 
#           setnames(df, "POPULATION_DEFNITION_PREFERRED", "POPULATION_DEFINITION_PREFERRED")
      }
      if (fname == 'file_info') {
        if ("ORIGINAL_FILE_NAME" %in% colnames(df)) 
          setnames(df, "ORIGINAL_FILE_NAME", "ORIGINAL_NAME")
      }
      if (fname == 'mbaa_result') {
        if ("MFI_CORRDINATE" %in% colnames(df)) 
          setnames(df, "MFI_CORRDINATE", "MFI_COORDINATE")
      }
      if (fname == 'pcr_result') {
        if ("PROJECT_ID" %in% colnames(df)) 
          setnames(df, "PROJECT_ID", "WORKSPACE_ID")
      }
      if (fname == 'protocol_deviation') {
        if ("DESCRIPTION" %in% colnames(df)) 
          setnames(df, "DESCRIPTION", "DEVIATION_DESCRIPTION")
      }
      if  (fname == 'study_file') {
        if ("STUDY_ACC_NUM" %in% colnames(df)) 
          setnames(df, "STUDY_ACC_NUM", "STUDY_ACCESSION")
        if ("STUDY_FILE_ACC_NUM" %in% colnames(df)) 
          setnames(df, "STUDY_FILE_ACC_NUM", "STUDY_FILE_ACCESSION")
        if ("PROJECT_ID" %in% colnames(df)) 
          setnames(df, "PROJECT_ID", "WORKSPACE_ID")
      }
      if (fname == 'study_glossary') {
        if ("STUDY_ACC_NUM" %in% colnames(df)) 
          setnames(df, "STUDY_ACC_NUM", "STUDY_ACCESSION")
        if ("PROJECT_ID" %in% colnames(df)) 
          setnames(df, "PROJECT_ID", "WORKSPACE_ID")
      }
      if (fname == 'workspace') {
        if ("PROJECT_ID" %in% colnames(df)) 
          setnames(df, "PROJECT_ID", "WORKSPACE_ID")
        if ("CG_ID" %in% colnames(df)) 
          setnames(df, "CG_ID", "CONTRACT_ID")
      }
      
      if (fname == 'reference_range') {
        if ("REFERENCE_RANGE_ACC_NUM" %in% colnames(df)) 
          setnames(df, "REFERENCE_RANGE_ACC_NUM", "REFERENCE_RANGE_ACCESSION")
        if ("STUDY_ACC_NUM" %in% colnames(df)) 
          setnames(df, "STUDY_ACC_NUM", "STUDY_ACCESSION")
        if ("PROJECT_ID" %in% colnames(df)) 
          setnames(df, "PROJECT_ID", "WORKSPACE_ID")
        if ("LAB_OR_STUDY" %in% colnames(df)) 
          setnames(df, "LAB_OR_STUDY", "LAB_OR_STUDY_SOURCE")
      }
      
      if (fname == 'neut_ab_titer_result') {
        if ("STUDY_TIME_COLLECTET_UNIT" %in% colnames(df)) 
          setnames(df, "STUDY_TIME_COLLECTET_UNIT", "STUDY_TIME_COLLECTED_UNIT")
      }
      if (fname == 'study_2_protocol') {
        if ("STUDY_ACC_NUM" %in% colnames(df)) 
          setnames(df, "STUDY_ACC_NUM", "STUDY_ACCESSION")
        if ("PROT_ACC_NUM" %in% colnames(df)) 
          setnames(df, "PROT_ACC_NUM", "PROTOCOL_ACCESSION")
      }
      colnames(df) <- tolower(colnames(df))
      dbWriteTable(conn = db, name = fname, value = df, append = TRUE, row.names = FALSE)
    }
  }
  
  
  for (i in db_idx_stmts) {
    dbGetQuery(db, i)
  }
  
}

