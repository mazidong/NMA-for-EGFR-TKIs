# file name: 5. Subgroup forest plot.r
# Function: Extraction of summary outcome estimates for head-to-head comparisons in paired meta-analyses, and visualisation for graphs.
# Input: input/Forest plot subgroup.xlsx
# output: outcomes/Subgroup_forest_plot
# Category: input_settings, table_generation, meta-analysis, forest_plot
# Date: 2025-04-24
# Author: Ma zidong


# Load necessary libraries
library(here)
library(meta)
library(readxl)
library(openxlsx)
library(dplyr)

# Summary results of subgroup meta-analysis assessing potential effect modifiers.
#### input_settings ####

# Set the main output directory

main_output_dir <- here("outcomes", "Subgroup_forest_plot")
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)

# The path to the data folder

file <- here("input", "Forest plot subgroup.xlsx")
excel_sheets(file)

cardiac_serious <- read_excel(file, sheet = 'cardiac serious')
vascular_serious <- read_excel(file, sheet = 'vascular serious')
cardiac_total <- read_excel(file, sheet = 'cardiac total')
vascular_total <- read_excel(file, sheet = 'vascular total')

arrhy_serious <- read_excel(file, sheet = "serious-arrhythmias")
hf_serious <- read_excel(file, sheet = "serious-heart failure")
vt_serious <- read_excel(file, sheet = "serious-Vascular Toxicity")
hyper_serious <- read_excel(file, sheet = "serious-hypertension")
arrhy_total <- read_excel(file, sheet = "total-arrhythmias")
hf_total <- read_excel(file, sheet = "total-heart failure")
vt_total <- read_excel(file, sheet = "total-Vascular Toxicity")
hyper_total <- read_excel(file, sheet = "total-hypertension")

arrhy_serious1 <- arrhy_serious[!is.na(arrhy_serious$generation) & arrhy_serious$generation != "", ]
arrhy_total1 <- arrhy_total[!is.na(arrhy_total$generation) & arrhy_total$generation != "", ]
# Excluding the influence of zorifertinib, zorifertinib is not a third-generation EGFR-TKI

cardiac_serious1 <- cardiac_serious[!is.na(cardiac_serious$generation) & cardiac_serious$generation != "", ]
cardiac_total1 <- cardiac_total[!is.na(cardiac_total$generation) & cardiac_total$generation != "", ]
# Excluding the influence of zorifertinib, zorifertinib is not a third-generation EGFR-TKI


# Screening for paired analyses based on primary and secondary outcomes with placebo in the control group.
cardiac_serious_placebo <- subset(cardiac_serious, control == "Placebo")
cardiac_total_placebo <- subset(cardiac_total, control == "Placebo")
vascular_serious_placebo <- subset(vascular_serious, control == "Placebo")
vascular_total_placebo <- subset(vascular_total, control == "Placebo")

arrhy_serious_placebo <- subset(arrhy_serious, control == "Placebo")
hf_serious_placebo <- subset(hf_serious, control == "Placebo")
vt_serious_placebo <- subset(vt_serious, control == "Placebo")
hyper_serious_placebo <- subset(hyper_serious, control == "Placebo")
arrhy_total_placebo <- subset(arrhy_total, control == "Placebo")
hf_total_placebo <- subset(hf_total, control == "Placebo")
vt_total_placebo <- subset(vt_total, control == "Placebo")
hyper_total_placebo <- subset(hyper_total, control == "Placebo")

# Screening for paired analyses based on primary and secondary outcomes with other EGFR TKIs in the control group.
arrhy_serious_other <- subset(arrhy_serious1, control == "other EGFR-TKI")
hf_serious_other <- subset(hf_serious, control == "other EGFR-TKI") 
vt_serious_other <- subset(vt_serious, control == "other EGFR-TKI") 
hyper_serious_other <- subset(hyper_serious, control == "other EGFR-TKI")
arrhy_total_other <- subset(arrhy_total1, control == "other EGFR-TKI")
hf_total_other <- subset(hf_total, control == "other EGFR-TKI")
vt_total_other <- subset(vt_total, control == "other EGFR-TKI")
hyper_total_other <- subset(hyper_total, control == "other EGFR-TKI")

cardiac_serious_other <- subset(cardiac_serious1, control == "other EGFR-TKI")
cardiac_total_other <- subset(cardiac_total1, control == "other EGFR-TKI")
vascular_serious_other <- subset(vascular_serious, control == "other EGFR-TKI")
vascular_total_other <- subset(vascular_total, control == "other EGFR-TKI")

# Paired analysis of the control group screened for chemotherapy based on primary outcome classification.
cardiac_serious_f <- cardiac_serious[!is.na(cardiac_serious$first_line_chemo) & cardiac_serious$first_line_chemo != "", ]
vascular_serious_f <- vascular_serious[!is.na(vascular_serious$first_line_chemo) & vascular_serious$first_line_chemo != "", ]
cardiac_total_f <- cardiac_total[!is.na(cardiac_total$first_line_chemo) & cardiac_total$first_line_chemo != "", ]
vascular_total_f <- vascular_total[!is.na(vascular_total$first_line_chemo) & vascular_total$first_line_chemo != "", ]

cardiac_serious_Chemo <- subset(cardiac_serious_f, control == "Chemotherapy") %>% filter(grepl('1st-generation EGFR TKIs', generation))
cardiac_total_Chemo <- subset(cardiac_total_f, control == "Chemotherapy") %>% filter(grepl('1st-generation EGFR TKIs', generation))
vascular_serious_Chemo <- subset(vascular_serious_f, control == "Chemotherapy") %>% filter(grepl('1st-generation EGFR TKIs', generation))
vascular_total_Chemo <- subset(vascular_total_f, control == "Chemotherapy") %>% filter(grepl('1st-generation EGFR TKIs', generation))


colnames(cardiac_serious)
table(cardiac_serious$median_age)
table(cardiac_serious$Smoker_status)
table(cardiac_serious$location)
table(cardiac_serious$follow_up)
table(cardiac_serious$treat_line)

# Define grouping conditions and dataset information
group_conditions <- list(
  Smoker_status = c("Non-smoker", "Smoker"),
  location = c("Non-Asian", "Asian","Mixed"),
  follow_up = c("<2 years", ">=2 years"),
  median_age = c("<65 years",">=65 years"),
  treat_line = c("priority therapy","non-priority therapy")
)



#### table_generation ####
# Generate head-to-head paired meta-analysis results of different EGFR TKI monotherapy under different conditions and different outcomes, and extract the effect values ​​and generate a table.
datasets1 <- c("cardiac_serious", "cardiac_total", "vascular_serious", "vascular_total")
datasets2 <- c("arrhy_serious","hf_serious","vt_serious","hyper_serious","arrhy_total","hf_total","vt_total","hyper_total")

# Define a function to handle each grouping situation
process_monotherapy_group <- function(data_name, group_var, group_value) {
  # Subsetting data according to conditionsg data according to conditions
  data_subset <- subset(get(data_name), get(group_var) == group_value)
  if (nrow(data_subset) == 0) return(NULL)
  # Run meta analysis
  meta_result <- metabin(
    data_subset$event.e,  data_subset$n.e,  data_subset$event.c,  data_subset$n.c,
    data = data_subset,
    sm = "OR",
    studlab = paste(data_subset$author, data_subset$year, sep = ","),
    random = TRUE,
    robust = TRUE,
    subgroup  = monotherapy,
    incr = 0.5,    
    allstudies = TRUE,  
    method.tau = "DL",  
    warn = FALSE        
  )
  
  # Return meta analysis results
  return(meta_result)
  #return(data_subset)
}

# Initialize the result storage list
results_list <- list()

# Merge two dataset lists
all_datasets <- c(datasets1, datasets2)

# Loop through conditions and datasets
for (i in seq_along(group_conditions)) {
  condition <- names(group_conditions)[i]
  values <- group_conditions[[i]]
  
  tryCatch({
    for (data_name in all_datasets) {
      for (value in values) {
        #print(condition)
        #print(value)
        meta_result <- process_monotherapy_group(data_name, condition, value)
        
        if (!is.null(meta_result)) {
          categories <- meta_result$subgroup.levels
          OR <- exp(meta_result$TE.common.w)
          lower <- exp(meta_result$lower.common.w)
          upper <- exp(meta_result$upper.common.w)
          k <- meta_result$k.all.w
          I2 <- meta_result$I2.w
          tau2 <- meta_result$tau2.w
          pval <- meta_result$pval.common.w

          # Extract the key information of meta result and save it to the list
          results_list[[length(results_list) + 1]] <- data.frame(
            Category = categories,
            Condition = condition,
            Value = value,
            Dataset = data_name,
            OR = OR, 
            CI_low = lower,  
            CI_high = upper,
            K <- k,
            I2 <- I2,
            tau2 <- tau2,
            p_value = pval   
          )
        }
      }
    }
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
    NULL  
  })
}

# If there is a result, save to file
if (length(results_list) > 0) {
  final_results <- do.call(rbind, results_list)
  write.csv(final_results, file.path(main_output_dir,"meta_analysis_results-monotherapy.csv"), row.names = FALSE)
  #print("Results saved to meta_analysis_results.csv")
} else {
  print("No results to save.")
}


#### table_generation ####
# Generate head-to-head paired meta-analysis results of different types of generation EGFR TKIs under different conditions and different outcomes, and extract effect values and generate a table.


datasets1 <- c("cardiac_serious1", "cardiac_total1", "vascular_serious", "vascular_total")
datasets2 <- c("arrhy_serious1","hf_serious","vt_serious","hyper_serious","arrhy_total1","hf_total","vt_total","hyper_total")


process_generation_group <- function(data_name, group_var, group_value) {
  
  data_subset <- subset(get(data_name), get(group_var) == group_value)
  if (nrow(data_subset) == 0) return(NULL)
  
  meta_result <- metabin(
    data_subset$event.e,  data_subset$n.e,  data_subset$event.c,  data_subset$n.c,
    data = data_subset,
    sm = "OR",
    studlab = paste(data_subset$author, data_subset$year, sep = ","),
    random = TRUE,
    robust = TRUE,
    subgroup  = generation,
    incr = 0.5,   
    allstudies = TRUE,  
    method.tau = "DL",  
    warn = FALSE        
  )
  
 
  return(meta_result)
  #return(data_subset)
}


results_list <- list()


all_datasets <- c(datasets1, datasets2)


for (i in seq_along(group_conditions)) {
  condition <- names(group_conditions)[i]
  values <- group_conditions[[i]]
  
  tryCatch({
    for (data_name in all_datasets) {
      for (value in values) {
     
        #print(condition)
        #print(value)
        meta_result <- process_generation_group(data_name, condition, value)
        
       
        if (!is.null(meta_result)) {
          categories <- meta_result$subgroup.levels
          OR <- exp(meta_result$TE.common.w)
          lower <- exp(meta_result$lower.common.w)
          upper <- exp(meta_result$upper.common.w)
          k <- meta_result$k.all.w
          I2 <- meta_result$I2.w
          tau2 <- meta_result$tau2.w
          pval <- meta_result$pval.common.w
         
          results_list[[length(results_list) + 1]] <- data.frame(
            Category = categories,
            Condition = condition,
            Value = value,
            Dataset = data_name,
            OR = OR,  
            CI_low = lower,  
            CI_high = upper,
            K <- k,
            I2 <- I2,
            tau2 <- tau2,
            p_value = pval    
          )
        }
      }
    }
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
    NULL  
  })
}

# If there is a result, save to file
if (length(results_list) > 0) {
  final_results <- do.call(rbind, results_list)
  write.csv(final_results, file.path(main_output_dir,"meta_analysis_results-generation.csv"), row.names = FALSE)
  #print("Results saved to meta_analysis_results.csv")
} else {
  print("No results to save.")
}


#### meta-analysis ####
csm <- metabin(event.e,n.e,event.c,n.c,data=cardiac_serious_placebo,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = generation)
vsm <- metabin(event.e,n.e,event.c,n.c,data=vascular_serious_placebo,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = generation)
ctm <- metabin(event.e,n.e,event.c,n.c,data=cardiac_total_placebo,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = generation)
vtm <- metabin(event.e,n.e,event.c,n.c,data=vascular_total_placebo,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = generation)

csm1 <- metabin(event.e,n.e,event.c,n.c,data=cardiac_serious_other,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = generation)
vsm1 <- metabin(event.e,n.e,event.c,n.c,data=vascular_serious_other,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = generation)
ctm1 <- metabin(event.e,n.e,event.c,n.c,data=cardiac_total_other,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = generation)
vtm1 <- metabin(event.e,n.e,event.c,n.c,data=vascular_total_other,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = generation)

arsm <- metabin(event.e,n.e,event.c,n.c,data=arrhy_serious_placebo,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
hfsm <- metabin(event.e,n.e,event.c,n.c,data=hf_serious_placebo,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
vtsm <- metabin(event.e,n.e,event.c,n.c,data=vt_serious_placebo,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
hysm <- metabin(event.e,n.e,event.c,n.c,data=hyper_serious_placebo,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
artm <- metabin(event.e,n.e,event.c,n.c,data=arrhy_total_placebo,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
hftm <- metabin(event.e,n.e,event.c,n.c,data=hf_total_placebo,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
vttm <- metabin(event.e,n.e,event.c,n.c,data=vt_total_placebo,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
hytm <- metabin(event.e,n.e,event.c,n.c,data=hyper_total_placebo,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)

arsm1 <- metabin(event.e,n.e,event.c,n.c,data=arrhy_serious_other,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
hfsm1 <- metabin(event.e,n.e,event.c,n.c,data=hf_serious_other,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
vtsm1 <- metabin(event.e,n.e,event.c,n.c,data=vt_serious_other,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
hysm1 <- metabin(event.e,n.e,event.c,n.c,data=hyper_serious_other,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
artm1 <- metabin(event.e,n.e,event.c,n.c,data=arrhy_total_other,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
hftm1 <- metabin(event.e,n.e,event.c,n.c,data=hf_total_other,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
vttm1 <- metabin(event.e,n.e,event.c,n.c,data=vt_total_other,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)
hytm1 <- metabin(event.e,n.e,event.c,n.c,data=hyper_total_other,sm="OR",studlab=paste(author, year, sep = ","),random=TRUE,robust=TRUE, subgroup = generation)

# Comparison between first-generation EGFR TKI and chemotherapy, according to whether the frontline receives chemotherapy as a subgroup
csm4 <- metabin(event.e,n.e,event.c,n.c,data=cardiac_serious_Chemo,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = first_line)
vsm4 <- metabin(event.e,n.e,event.c,n.c,data=vascular_serious_Chemo,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = first_line)
ctm4 <- metabin(event.e,n.e,event.c,n.c,data=cardiac_total_Chemo,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = first_line)
vtm4 <- metabin(event.e,n.e,event.c,n.c,data=vascular_total_Chemo,sm="OR",studlab=paste(author, year, sep = ","), random=TRUE,robust=TRUE, subgroup = first_line)


# forest plots of head-to-head comparisons in pairwise meta-analysis 
#### forest_plot ####

pdf(file.path(main_output_dir,"primary-meta-forest(Three-generation subgroups vs Placebo).pdf"), width = 14, height = 9)
meta::forest(csm, col.study = "#000000", col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#7D5CC6FF", col.diamond.lines = "#7D5CC6FF", sortvar = csm$TE, smlab = "Serious Cardiac \nAdverse Drug Reactions")
meta::forest(vsm, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#7D5CC6FF", col.diamond.lines = "#7D5CC6FF", sortvar = vsm$TE, smlab = "Serious Vascular \nAdverse Drug Reactions")
meta::forest(ctm, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#7D5CC6FF", col.diamond.lines = "#7D5CC6FF", sortvar = ctm$TE, smlab = "Total Cardiac \nAdverse Drug Reactions")
meta::forest(vtm, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#7D5CC6FF", col.diamond.lines = "#7D5CC6FF", sortvar = vtm$TE, smlab = "Total Vascular \nAdverse Drug Reactions")
dev.off()

pdf(file.path(main_output_dir,"secondary-meta-forest(Three-generation subgroups vs Placebo).pdf"), width = 14, height = 9)
meta::forest(arsm, sortvar = arsm$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Serious Arrhythmias \nAdverse Drug Reactions")
meta::forest(hfsm, sortvar = hfsm$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Serious Heart Failure \nAdverse Drug Reactions")
meta::forest(vtsm, sortvar = vtsm$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Serious Vascular Toxicity \nAdverse Drug Reactions")
meta::forest(hysm, sortvar = hysm$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Serious Hypertension \nAdverse Drug Reactions")
meta::forest(artm, sortvar = artm$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Total Arrhythmias \nAdverse Drug Reactions")
meta::forest(hftm, sortvar = hftm$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Total Heart Failure \nAdverse Drug Reactions")
meta::forest(vttm, sortvar = vttm$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Total Vascular Toxicity \nAdverse Drug Reactions")
meta::forest(hytm, sortvar = hytm$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Total Hypertension \nAdverse Drug Reactions")
dev.off()

pdf(file.path(main_output_dir,"primary-meta-forest(Three-generation subgroups vs Other EGFR-TKI).pdf"), width = 14, height =  12)
meta::forest(csm1, col.study = "black", col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#7D5CC6FF", col.diamond.lines = "#7D5CC6FF", sortvar = csm1$TE, smlab = "Serious Cardiac \nAdverse Drug Reactions")
meta::forest(vsm1, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#7D5CC6FF", col.diamond.lines = "#7D5CC6FF", sortvar = vsm1$TE, smlab = "Serious Vascular \nAdverse Drug Reactions")
meta::forest(ctm1, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#7D5CC6FF", col.diamond.lines = "#7D5CC6FF", sortvar = ctm1$TE, smlab = "Total Cardiac \nAdverse Drug Reactions")
meta::forest(vtm1, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#7D5CC6FF", col.diamond.lines = "#7D5CC6FF", sortvar = vtm1$TE, smlab = "Total Vascular \nAdverse Drug Reactions")
dev.off()

pdf(file.path(main_output_dir,"secondary-meta-forest(Three-generation subgroups vs Other EGFR-TKI).pdf"), width = 14, height = 12)
meta::forest(arsm1, sortvar = arsm1$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Serious Arrhythmias \nAdverse Drug Reactions")
meta::forest(hfsm1, sortvar = hfsm1$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Serious Heart Failure \nAdverse Drug Reactions")
meta::forest(vtsm1, sortvar = vtsm1$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Serious Vascular Toxicity \nAdverse Drug Reactions")
meta::forest(hysm1, sortvar = hysm1$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Serious Hypertension \nAdverse Drug Reactions")
meta::forest(artm1, sortvar = artm1$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Total Arrhythmias \nAdverse Drug Reactions")
meta::forest(hftm1, sortvar = hftm1$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Total Heart Failure \nAdverse Drug Reactions")
meta::forest(vttm1, sortvar = vttm1$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Total Vascular Toxicity \nAdverse Drug Reactions")
meta::forest(hytm1, sortvar = hytm1$TE, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#00B2A9FF", col.diamond.lines = "#00B2A9FF", smlab = "Total Hypertension \nAdverse Drug Reactions")
dev.off()

# Comparison of chemotherapy on the frontline
pdf(file.path(main_output_dir,"primary-meta-forest(Gen1-EGFR-TKI vs chemotherapy).pdf"), width = 14)

meta::forest(csm4, col.study = "#000000", col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#C50084FF", col.diamond.lines = "#C50084FF", sortvar = csm4$TE, smlab = "Serious Cardiac \nAdverse Drug Reactions")
meta::forest(vsm4, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#C50084FF", col.diamond.lines = "#C50084FF", sortvar = vsm4$TE, smlab = "Serious Vascular \nAdverse Drug Reactions")
meta::forest(ctm4, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#C50084FF", col.diamond.lines = "#C50084FF", sortvar = ctm4$TE, smlab = "Total Cardiac \nAdverse Drug Reactions")
meta::forest(vtm4, col.square = "#000000", col.square.lines = "#747678FF", col.inside = "#000000", col.diamond = "#C50084FF", col.diamond.lines = "#C50084FF", sortvar = vtm4$TE, smlab = "Total Vascular \nAdverse Drug Reactions")
dev.off()


