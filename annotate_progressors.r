############################################################

# Annotate HNSCC Tumor Progressors and NonProgressors

# Original criteria: Patients were first classified as progressor or nonprogressor based on follow-up annotation, specifically the presence or absence of a new tumor event. We required annotation to confirm the tumor event (days to new tumor and/or new tumor anatomical location). All patients were required to have treatment annotation in addition to the follow-up data. https://www.ncbi.nlm.nih.gov/pubmed/26747525

############################################################

# Read in cleaned clinical data
read.delim(file="raw_clinical_data.txt", sep="\t", header=T, stringsAsFactors = F) -> hnsc_data

# Add new column for annotation
hnsc_data$Progression_FINAL_new = NA

# Criteria:

# Progressor if they had any one of these:
# "days_to_new_tumor_event_after_initial_treatment" not NA
# "new_tumor_event_after_initial_treatment" = YES
# "new_neoplasm_event_occurrence_anatomic_site" not NA
# "followup_treatment_success" = Progressive Disease or Persistent Disease

# Nonprogressor if they had any one of these and weren't annotated as Progressor based on above criteria:
# "days_to_new_tumor_event_after_initial_treatment" = NA
# "new_tumor_event_after_initial_treatment"  = NO
# "new_neoplasm_event_occurrence_anatomic_site" = NA
# "followup_treatment_success"  = Complete Remission/Response, Stable Disease, Partial Remission/Response, or NA

# Annotate Progressors
hnsc_data[which(!is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment"]) | hnsc_data[,"new_tumor_event_after_initial_treatment"] == "YES" | !is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site"]) | hnsc_data[,"followup_treatment_success"] == "Progressive Disease" | hnsc_data[,"followup_treatment_success"] == "Persistent Disease"),"Progression_FINAL_new"] <- "Progressor"

# Annotate NonProgressors
hnsc_data[which((is.na(hnsc_data[, "days_to_new_tumor_event_after_initial_treatment"]) | hnsc_data[,"new_tumor_event_after_initial_treatment"] == "NO" | is.na(hnsc_data[,"new_neoplasm_event_occurrence_anatomic_site"]) | hnsc_data[,"followup_treatment_success"] == "Complete Remission/Response" | hnsc_data[,"followup_treatment_success"] == "Stable Disease" | hnsc_data[,"followup_treatment_success"] == "Partial Remission/Response" | is.na(hnsc_data[,"followup_treatment_success"])) & is.na(hnsc_data[,"Progression_FINAL_new"])),"Progression_FINAL_new"] <- "NonProgressor"

# Save data
write.table(file="clinical_data_annotated.txt", x=hnsc_data, sep="\t", quote=F, row.names=F)
