############################################################

# Annotate HNSCC Tumor Progressors and NonProgressors

# Original criteria: Patients were first classified as progressor or nonprogressor based on follow-up annotation, specifically the presence or absence of a new tumor event. We required annotation to confirm the tumor event (days to new tumor and/or new tumor anatomical location). All patients were required to have treatment annotation in addition to the follow-up data. https://www.ncbi.nlm.nih.gov/pubmed/26747525

############################################################

# Read in cleaned clinical data
read.delim(file="raw_clinical_data.txt", sep="\t", header=T, stringsAsFactors = F) -> hnsc_data

# Add new column for annotation
hnsc_data$Progression_FINAL_new = NA

# Criteria:

# Progressor if they had any one of these (all occurrences of these columns v1-6):
# "days_to_new_tumor_event_after_initial_treatment" not NA
# "new_tumor_event_after_initial_treatment" = YES
# "new_neoplasm_event_occurrence_anatomic_site" not NA
# "followup_treatment_success" = Progressive Disease or Persistent Disease

# Nonprogressor if they had any one of these (all occurrences of these columns v1-6) and weren't annotated as Progressor based on above criteria:
# "days_to_new_tumor_event_after_initial_treatment" = NA
# "new_tumor_event_after_initial_treatment"  = NO
# "new_neoplasm_event_occurrence_anatomic_site" = NA
# "followup_treatment_success"  = Complete Remission/Response, Stable Disease, Partial Remission/Response, or NA

# Observe all occurrences of the above columns:
names(hnsc_data)[grep("days_to_new_tumor_event_after_initial_treatment", names(hnsc_data))]

names(hnsc_data)[grep("new_tumor_event_after_initial_treatment", names(hnsc_data))][-grep("days",names(hnsc_data)[grep("new_tumor_event_after_initial_treatment", names(hnsc_data))])]

names(hnsc_data)[grep("new_neoplasm_event_occurrence_anatomic_site", names(hnsc_data))]

names(hnsc_data)[grep("followup_treatment_success", names(hnsc_data))]

# Annotate Progressors
hnsc_data[
  
  which(!is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment"]) | 
          
          !is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment1"]) |
          
          !is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment2"]) |
          
          !is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment3"]) |
          
          !is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment4"]) |
          
          !is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment5"]) |
          
          hnsc_data[,"new_tumor_event_after_initial_treatment"] == "YES" | 
          
          hnsc_data[,"new_tumor_event_after_initial_treatment1"] == "YES" |
          
          hnsc_data[,"new_tumor_event_after_initial_treatment2"] == "YES" |
          
          hnsc_data[,"new_tumor_event_after_initial_treatment3"] == "YES" |
          
          hnsc_data[,"new_tumor_event_after_initial_treatment4"] == "YES" |
          
          hnsc_data[,"new_tumor_event_after_initial_treatment5"] == "YES" |
          
          hnsc_data[,"new_tumor_event_after_initial_treatment6"] == "YES" |
          
          !is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site"]) | 
          
          !is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site1"]) |  
          
          !is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site2"]) |
          
          !is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site3"]) |
          
          !is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site4"]) |
          
          !is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site5"]) |
          
          hnsc_data[,"followup_treatment_success"] == "Progressive Disease" | 
          
          hnsc_data[,"followup_treatment_success"] == "Persistent Disease" |
          
          hnsc_data[,"followup_treatment_success1"] == "Progressive Disease" | 
          
          hnsc_data[,"followup_treatment_success1"] == "Persistent Disease" | 
          
          hnsc_data[,"followup_treatment_success2"] == "Progressive Disease" | 
          
          hnsc_data[,"followup_treatment_success2"] == "Persistent Disease" |
          
          hnsc_data[,"followup_treatment_success3"] == "Progressive Disease" | 
          
          hnsc_data[,"followup_treatment_success3"] == "Persistent Disease" |
          
          hnsc_data[,"followup_treatment_success4"] == "Progressive Disease" | 
          
          hnsc_data[,"followup_treatment_success4"] == "Persistent Disease" |
          
          hnsc_data[,"followup_treatment_success5"] == "Progressive Disease" | 
          
          hnsc_data[,"followup_treatment_success5"] == "Persistent Disease"),
  
  "Progression_FINAL_new"] <- "Progressor"


# Annotate NonProgressors
hnsc_data[which((
  
  is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment"]) | 
    
    is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment1"]) | 
    
    is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment2"]) | 
    
    is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment3"]) | 
    
    is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment4"]) | 
    
    is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment5"]) | 
    
    hnsc_data[,"new_tumor_event_after_initial_treatment"] == "NO" | 
    
    hnsc_data[,"new_tumor_event_after_initial_treatment1"] == "NO" | 
    
    hnsc_data[,"new_tumor_event_after_initial_treatment2"] == "NO" | 
    
    hnsc_data[,"new_tumor_event_after_initial_treatment3"] == "NO" | 
    
    hnsc_data[,"new_tumor_event_after_initial_treatment4"] == "NO" | 
    
    hnsc_data[,"new_tumor_event_after_initial_treatment5"] == "NO" | 
    
    hnsc_data[,"new_tumor_event_after_initial_treatment6"] == "NO" | 
    
    is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site"]) | 
    
    is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site1"]) | 
    
    is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site2"]) | 
    
    is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site3"]) | 
    
    is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site4"]) | 
    
    is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site5"]) | 
    
    hnsc_data[,"followup_treatment_success"] == "Complete Remission/Response" |
    
    hnsc_data[,"followup_treatment_success"] == "Stable Disease" | 
    
    hnsc_data[,"followup_treatment_success"] == "Partial Remission/Response" | 
    
    is.na(hnsc_data[,"followup_treatment_success"]) |
    
    hnsc_data[,"followup_treatment_success1"] == "Complete Remission/Response" |
    
    hnsc_data[,"followup_treatment_success1"] == "Stable Disease" | 
    
    hnsc_data[,"followup_treatment_success1"] == "Partial Remission/Response" | 
    
    is.na(hnsc_data[,"followup_treatment_success1"]) |
    
    hnsc_data[,"followup_treatment_success2"] == "Complete Remission/Response" |
    
    hnsc_data[,"followup_treatment_success2"] == "Stable Disease" | 
    
    hnsc_data[,"followup_treatment_success2"] == "Partial Remission/Response" | 
    
    is.na(hnsc_data[,"followup_treatment_success2"]) |
    
    hnsc_data[,"followup_treatment_success3"] == "Complete Remission/Response" |
    
    hnsc_data[,"followup_treatment_success3"] == "Stable Disease" | 
    
    hnsc_data[,"followup_treatment_success3"] == "Partial Remission/Response" | 
    
    is.na(hnsc_data[,"followup_treatment_success3"]) |
    
    hnsc_data[,"followup_treatment_success4"] == "Complete Remission/Response" |
    
    hnsc_data[,"followup_treatment_success4"] == "Stable Disease" | 
    
    hnsc_data[,"followup_treatment_success4"] == "Partial Remission/Response" | 
    
    is.na(hnsc_data[,"followup_treatment_success4"]) |
    
    hnsc_data[,"followup_treatment_success5"] == "Complete Remission/Response" |
    
    hnsc_data[,"followup_treatment_success5"] == "Stable Disease" | 
    
    hnsc_data[,"followup_treatment_success5"] == "Partial Remission/Response" | 
    
    is.na(hnsc_data[,"followup_treatment_success5"])) & is.na(hnsc_data[,"Progression_FINAL_new"])),"Progression_FINAL_new"] <- "NonProgressor"

# Save data
write.table(file="clinical_data_annotated.txt", x=hnsc_data, sep="\t", quote=F, row.names=F)
