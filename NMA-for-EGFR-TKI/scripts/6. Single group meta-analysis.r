# file name: 6. Single group meta-analysis.r
# Function: Extraction of primary outcome and secondary outcome safety data for the EGFR-TKI arm of the RCTs, single arm trials, observational studies and real-world studies, single group meta-analysis, forest plotting.
# Input: input/Single group meta-analysis.xlsx
# output: outcomes/single-group-meta
# Category: input_settings, single group meta-analysis, forest_plot
# Date: 2025-04-24
# Author: Ma zidong



# Load necessary libraries
library(readxl)
library(openxlsx)
library(meta)


# ----------------------------------input_settings  --------------------------------- #

# Set the main output directory
main_output_dir <- "/your_path_to_project/NMA-for-EGFR-TKI/outcomes/single-group-meta/" # Change to your main output directory
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)
setwd(main_output_dir)

# The path to the data folder
file_path <- "/your_path_to_project/NMA-for-EGFR-TKI/input/" # Change to your data folder path

file <- paste0(file_path, "Single group meta-analysis.xlsx")
excel_sheets(file)

# Read in EGFR TKI monotherapy data below
cardiac_serious <- read_excel(file, col_names = T, sheet = "cardiac-serious")
cardiac_total <- read_excel(file, col_names = T, sheet = "cardiac-total")
vascular_serious <- read_excel(file, col_names = T, sheet = "vascular-serious")
vascular_total <- read_excel(file, col_names = T, sheet = "vascular-total")

data_serious <- read_excel(file, col_names = T, sheet = "serious")
data_total <- read_excel(file, col_names = T, sheet = "total")

# Read in EGFR TKI generation data below
cardiac_serious1 <- read_excel(file, col_names = T, sheet = "cardiac-serious-generation")
cardiac_total1 <- read_excel(file, col_names = T, sheet = "cardiac-total-gennertion")
vascular_serious1 <- read_excel(file, col_names = T, sheet = "vascular-serious-generation")
vascular_total1 <- read_excel(file, col_names = T, sheet = "vascular-total-generation")

data_serious1 <- read_excel(file, col_names = T, sheet = "serious-generation")
data_total1 <- read_excel(file, col_names = T, sheet = "total-generation")


colnames(data_serious)
table(data_serious$group)
table(data_total$group)

# Read in EGFR TKI monotherapy data below
qt <- subset(data_serious, group == "arrhythmias QTc-serious")
hf <- subset(data_serious, group == "heart failure-serious")
hypen <- subset(data_serious, group == "hypertension-serious")
vt <- subset(data_serious, group == "Vascular Toxicity-serious")

qt1 <- subset(data_total, group == "arrhythmias QTc-total")
hf1 <- subset(data_total, group == "heart failure-total")
hypen1 <- subset(data_total, group == "hypertension-total")
vt1 <- subset(data_total, group == "Vascular Toxicity-total")

# Read in EGFR TKI generation data below
qt_1 <- subset(data_serious1, group == "arrhythmias QTc-serious")
hf_1 <- subset(data_serious1, group == "heart failure-serious")
hypen_1 <- subset(data_serious1, group == "hypertension-serious")
vt_1 <- subset(data_serious1, group == "Vascular Toxicity-serious")

qt1_1 <- subset(data_total1, group == "arrhythmias QTc-total")
hf1_1 <- subset(data_total1, group == "heart failure-total")
hypen1_1 <- subset(data_total1, group == "hypertension-total")
vt1_1 <- subset(data_total1, group == "Vascular Toxicity-total")


# ------------------------ single group meta-analysis ------------------------ #
#serious
meta_prop_qt <- metaprop(event.e,n.e,data=qt,studlab = paste(qt$author,qt$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_hf <- metaprop(event.e,n.e,data=hf,studlab = paste(hf$author,hf$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_hypen <- metaprop(event.e,n.e,data=hypen,studlab = paste(hypen$author,hypen$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_vts <- metaprop(event.e,n.e,data=vt,studlab = paste(vt$author,vt$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
#total
meta_prop_qt1 <- metaprop(event.e,n.e,data=qt1,studlab = paste(qt1$author,qt1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_hf1 <- metaprop(event.e,n.e,data=hf1,studlab = paste(hf1$author,hf1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_hypen1 <- metaprop(event.e,n.e,data=hypen1,studlab = paste(hypen1$author,hypen1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_vtt <- metaprop(event.e,n.e,data=vt1,studlab = paste(vt1$author,vt1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
#serious-generation
meta_prop_qt_1 <- metaprop(event.e,n.e,data=qt_1,studlab = paste(qt_1$author,qt_1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_hf_1 <- metaprop(event.e,n.e,data=hf_1,studlab = paste(hf_1$author,hf_1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_hypen_1 <- metaprop(event.e,n.e,data=hypen_1,studlab = paste(hypen_1$author,hypen_1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_vt_1 <- metaprop(event.e,n.e,data=vt_1,studlab = paste(vt_1$author,vt_1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
#total-generation
meta_prop_qt1_1 <- metaprop(event.e,n.e,data=qt1_1,studlab = paste(qt1_1$author,qt1_1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_hf1_1 <- metaprop(event.e,n.e,data=hf1_1,studlab = paste(hf1_1$author,hf1_1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_hypen1_1 <- metaprop(event.e,n.e,data=hypen1_1,studlab = paste(hypen1_1$author,hypen1_1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_vt1_1 <- metaprop(event.e,n.e,data=vt1_1,studlab = paste(vt1_1$author,vt1_1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)

meta_prop_cs <- metaprop(event.e,n.e,data=cardiac_serious,studlab = paste(cardiac_serious$author,cardiac_serious$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_ct <- metaprop(event.e,n.e,data=cardiac_total,studlab = paste(cardiac_total$author,cardiac_total$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_vs <- metaprop(event.e,n.e,data=vascular_serious,studlab = paste(vascular_serious$author,vascular_serious$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_vt <- metaprop(event.e,n.e,data=vascular_total,studlab = paste(vascular_total$author,vascular_total$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
#generation
meta_prop_cs1 <- metaprop(event.e,n.e,data=cardiac_serious1,studlab = paste(cardiac_serious1$author,cardiac_serious1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_ct1 <- metaprop(event.e,n.e,data=cardiac_total1,studlab = paste(cardiac_total1$author,cardiac_total1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_vs1 <- metaprop(event.e,n.e,data=vascular_serious1,studlab = paste(vascular_serious1$author,vascular_serious1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)
meta_prop_vt1 <- metaprop(event.e,n.e,data=vascular_total1,studlab = paste(vascular_total1$author,vascular_total1$year,sep=","),sm="PRAW",incr=0.5,allincr=TRUE,addincr=FALSE, byvar = drug)


# -------------------------------- forest_plot ------------------------------- #
forest_list <- list(
  meta_prop_qt = "Serious Arrhythmias QTc",
  meta_prop_hf = "Serious Heart Failure",
  meta_prop_hypen = "Serious Hypertension",
  meta_prop_vts = "Serious Vascular Toxicity",
  meta_prop_qt1 = "Total Arrhythmias QTc",
  meta_prop_hf1 = "Total Heart Failure",
  meta_prop_hypen1 = "Total Hypertension",
  meta_prop_vtt = "Total Vascular Toxicity"
)

for (obj_name in names(forest_list)) {
  obj <- get(obj_name)
  pdf(paste0(obj_name, "_", forest_list[[obj_name]], "_Monotherapy.pdf"), width = 12, height = 0.4 * obj$k + 3)  # Height is automatically adjusted with the number of research
  meta::forest(obj, smlab = forest_list[[obj_name]], layout = "RevMan5", col.square = "#0000ff", col.square.lines = "#000000", col.inside = "#000000", col.diamond = "#000000", col.diamond.lines = "#000000")
  dev.off()
}


forest_list1 <- list(
  meta_prop_qt_1 = "Serious Arrhythmias QTc",
  meta_prop_hf_1 = "Serious Heart Failure",
  meta_prop_hypen_1 = "Serious Hypertension",
  meta_prop_vt_1 = "Serious Vascular Toxicity",
  meta_prop_qt1_1 = "Total Arrhythmias QTc",
  meta_prop_hf1_1 = "Total Heart Failure",
  meta_prop_hypen1_1 = "Total Hypertension",
  meta_prop_vt1_1 = "Total Vascular Toxicity"
)

for (obj_name in names(forest_list1)) {
  obj <- get(obj_name)
  pdf(paste0(obj_name, "_", forest_list1[[obj_name]], "_generation.pdf"), width = 12, height = 0.4 * obj$k + 3)  # Height is automatically adjusted with the number of research
  meta::forest(obj, smlab = forest_list1[[obj_name]], layout = "RevMan5", col.square = "#0000ff", col.square.lines = "#000000", col.inside = "#000000", col.diamond = "#000000", col.diamond.lines = "#000000")
  dev.off()
}


forest_list2 <- list(
  meta_prop_cs = "Serious Cardiac \nAdverse Drug Reactions",
  meta_prop_ct = "Total Cardiac \nAdverse Drug Reactions",
  meta_prop_vs = "Serious Vascular \nAdverse Drug Reactions",
  meta_prop_vt = "Total Vascular \nAdverse Drug Reactions"
)

for (obj_name in names(forest_list2)) {
  obj <- get(obj_name)
  pdf(paste0(obj_name, "_", forest_list2[[obj_name]], "_Monotherapy.pdf"), width = 12, height = 0.4 * obj$k + 3)  # Height is automatically adjusted with the number of research
  meta::forest(obj, smlab = forest_list2[[obj_name]], layout = "RevMan5", col.square = "#0000ff", col.square.lines = "#000000", col.inside = "#000000", col.diamond = "#000000", col.diamond.lines = "#000000")
  dev.off()
}



forest_list3 <- list(
  meta_prop_cs1 = "Serious Cardiac \nAdverse Drug Reactions",
  meta_prop_ct1 = "Total Cardiac \nAdverse Drug Reactions",
  meta_prop_vs1 = "Serious Vascular \nAdverse Drug Reactions",
  meta_prop_vt1 = "Total Vascular \nAdverse Drug Reactions"
)

for (obj_name in names(forest_list3)) {
  obj <- get(obj_name)
  pdf(paste0(obj_name, "_", forest_list3[[obj_name]], "_generation.pdf"), width = 12, height = 0.4 * obj$k + 3)  # Height is automatically adjusted with the number of research
  meta::forest(obj, smlab = forest_list3[[obj_name]], layout = "RevMan5", col.square = "#0000ff", col.square.lines = "#000000", col.inside = "#000000", col.diamond = "#000000", col.diamond.lines = "#000000")
  dev.off()
}

