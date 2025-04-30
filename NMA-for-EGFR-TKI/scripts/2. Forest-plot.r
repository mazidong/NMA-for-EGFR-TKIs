# file name: 2. Forest-plot.r
# Function: Forest plots for comparison of cardiac and vascular adverse drug reactions between EGFR TKIs and placebo.
# Input: input/Figure5-forestplot-data.xlsx, input/Figure5-forestplot-data-appendix.xlsx, input/primary-serious-total-meta.csv, input/secondary-serious-total-meta.csv
# output: outcomes/forest-plot
# Category: input_settings, meta_analysis, output_settings, graph_generation
# Date: 2025-04-24
# Author: Ma zidong


# Load necessary libraries
library(meta)
library(readxl)
library(openxlsx)
library(forestploter)
library(grid)
library(ggsci)
library(dplyr)
# ----------------------------------input_settings  --------------------------------- #

# Set the main output directory
main_output_dir <- "/your_path_to_project/NMA-for-EGFR-TKI/outcomes/forest-plot/" # Change to your main output directory
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)
setwd(main_output_dir)

# The path to the data folder
file_path <- "/your_path_to_project/NMA-for-EGFR-TKI/input/" # Change to your data folder path

file <- paste0(file_path, "Figure5-forestplot-data.xlsx")# Used to analyze primary results
file1 <- paste0(file_path, "Figure5-forestplot-data-appendix.xlsx")# Used to analyze secondary results


cardiac_serious <- read_excel(file, sheet = 'cardiac-serious')
vascular_serious <- read_excel(file, sheet = 'vascular-serious')
cardiac_total <- read_excel(file, sheet = 'cardiac-total')
vascular_total <- read_excel(file, sheet = 'vascular-total')

arrhy_serious <- read_excel(file1,sheet = "serious-arrhythmias")
hf_serious <- read_excel(file1,sheet = "serious-heart failure")
vt_serious <- read_excel(file1,sheet = "serious-Vascular Toxicity")
hyper_serious <- read_excel(file1,sheet = "serious-hypertension")
arrhy_total <- read_excel(file1,sheet = "total-arrhythmias")
hf_total <- read_excel(file1,sheet = "total-heart failure")
vt_total <- read_excel(file1,sheet = "total-Vascular Toxicity")
hyper_total <- read_excel(file1,sheet = "total-hypertension")



# ------------------------------- meta_analysis ------------------------------ #

csm <- metabin(event.e,n.e,event.c,n.c,data=cardiac_serious,sm="OR",studlab=Study,random=TRUE,robust=TRUE)
vsm <- metabin(event.e,n.e,event.c,n.c,data=vascular_serious,sm="OR",studlab=Study,random=TRUE,robust=TRUE)
ctm <- metabin(event.e,n.e,event.c,n.c,data=cardiac_total,sm="OR",studlab=Study,random=TRUE,robust=TRUE)
vtm <- metabin(event.e,n.e,event.c,n.c,data=vascular_total,sm="OR",studlab=Study,random=TRUE,robust=TRUE)

arsm <- metabin(event.e,n.e,event.c,n.c,data=arrhy_serious,sm="OR",studlab=Study,random=FALSE,robust=TRUE)
hfsm <- metabin(event.e,n.e,event.c,n.c,data=hf_serious,sm="OR",studlab=Study,random=FALSE,robust=TRUE)
vtsm <- metabin(event.e,n.e,event.c,n.c,data=vt_serious,sm="OR",studlab=Study,random=FALSE,robust=TRUE)
hysm <- metabin(event.e,n.e,event.c,n.c,data=hyper_serious,sm="OR",studlab=Study,random=FALSE,robust=TRUE)
artm <- metabin(event.e,n.e,event.c,n.c,data=arrhy_total,sm="OR",studlab=Study,random=FALSE,robust=TRUE)
hftm <- metabin(event.e,n.e,event.c,n.c,data=hf_total,sm="OR",studlab=Study,random=FALSE,robust=TRUE)
vttm <- metabin(event.e,n.e,event.c,n.c,data=vt_total,sm="OR",studlab=Study,random=FALSE,robust=TRUE)
hytm <- metabin(event.e,n.e,event.c,n.c,data=hyper_total,sm="OR",studlab=Study,random=FALSE,robust=TRUE)



pdf("primary-serious-total-forestplot.pdf", width = 12)
meta::forest(csm, layout = "RevMan5", sortvar = csm$TE, smlab = "Serious Cardiac \nAdverse Drug Reactions")
meta::forest(vsm, layout = "RevMan5", sortvar = vsm$TE, smlab = "Serious Vascular \nAdverse Drug Reactions")
meta::forest(ctm, layout = "RevMan5", sortvar = ctm$TE, smlab = "Total Cardiac \nAdverse Drug Reactions")
meta::forest(vtm, layout = "RevMan5", sortvar = vtm$TE, smlab = "Total Vascular \nAdverse Drug Reactions")
dev.off()


pdf("secondary-serious-total-forestplot.pdf", width = 12)
meta::forest(arsm, layout = "RevMan5", sortvar = arsm$TE, smlab = "Serious Arrhythmias \nAdverse Drug Reactions")
meta::forest(hfsm, layout = "RevMan5", sortvar = hfsm$TE, smlab = "Serious Heart Failure \nAdverse Drug Reactions")
meta::forest(vtsm, layout = "RevMan5", sortvar = vtsm$TE, smlab = "Serious Vascular Toxicity \nAdverse Drug Reactions")
meta::forest(hysm, layout = "RevMan5", sortvar = hysm$TE, smlab = "Serious Hypertension \nAdverse Drug Reactions")
meta::forest(artm, layout = "RevMan5", sortvar = artm$TE, smlab = "Total Arrhythmias \nAdverse Drug Reactions")
meta::forest(hftm, layout = "RevMan5", sortvar = hftm$TE, smlab = "Total Heart Failure \nAdverse Drug Reactions")
meta::forest(vttm, layout = "RevMan5", sortvar = vttm$TE, smlab = "Total Vascular Toxicity \nAdverse Drug Reactions")
meta::forest(hytm, layout = "RevMan5", sortvar = hytm$TE, smlab = "Total Hypertension \nAdverse Drug Reactions")
dev.off()


# ----------------------------- output_settings ----------------------------- #
# The meta-package was used to calculate ORs and 95% (CIs), and the data were collated into the forestploter plotting format for use in the main figure 5A and the supplementary plots.
default_csm <- meta::forest(csm)
default_vsm <- meta::forest(vsm)
default_ctm <- meta::forest(ctm)
default_vtm <- meta::forest(vtm)

default_arsm <- meta::forest(arsm)
default_hfsm <- meta::forest(hfsm)
default_vtsm <- meta::forest(vtsm)
default_hysm <- meta::forest(hysm)
default_artm <- meta::forest(artm)
default_hftm <- meta::forest(hftm)
default_vttm <- meta::forest(vttm)
default_hytm <- meta::forest(hytm)

arsm_hfsm_vtsm_hysm_data <- data.frame(Study = default_arsm$studlab, Arrhythmias = default_arsm$effect.format[-c(0:3)], Arrhythmias_CI = default_arsm$ci.format[-c(0:3)], Heart_Failure = default_hfsm$effect.format[-c(0:3)], Heart_Failure_CI = default_hfsm$ci.format[-c(0:3)],
                                       Vascular_Toxicity = default_vtsm$effect.format[-c(0:3)], Vascular_Toxicity_CI = default_vtsm$ci.format[-c(0:3)], Hypertension = default_hysm$effect.format[-c(0:3)], Hypertension_CI = default_hysm$ci.format[-c(0:3)])

artm_hftm_vttm_hytm_data <- data.frame(Study = default_artm$studlab, Arrhythmias = default_artm$effect.format[-c(0:3)], Arrhythmias_CI = default_artm$ci.format[-c(0:3)], Heart_Failure = default_hftm$effect.format[-c(0:3)], Heart_Failure_CI = default_hftm$ci.format[-c(0:3)],
                                       Vascular_Toxicity = default_vttm$effect.format[-c(0:3)], Vascular_Toxicity_CI = default_vttm$ci.format[-c(0:3)], Hypertension = default_hytm$effect.format[-c(0:3)], Hypertension_CI = default_hytm$ci.format[-c(0:3)])


csm_vsm_data <- data.frame(Study = default_csm$studlab, Cardiac = default_csm$effect.format[-c(0:3)], cardiac_CI = default_csm$ci.format[-c(0:3)], Vascular = default_vsm$effect.format[-c(0:3)], vascular_CI = default_vsm$ci.format[-c(0:3)])
ctm_vtm_data <- data.frame(Study = default_ctm$studlab, Cardiac = default_ctm$effect.format[-c(0:3)], cardiac_CI = default_ctm$ci.format[-c(0:3)], Vascular = default_vtm$effect.format[-c(0:3)], vascular_CI = default_vtm$ci.format[-c(0:3)])

csm_vsm_data$cardiac_lower <- sapply(strsplit(csm_vsm_data$cardiac_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
csm_vsm_data$cardiac_higher <- sapply(strsplit(csm_vsm_data$cardiac_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
csm_vsm_data$vascular_lower <- sapply(strsplit(csm_vsm_data$vascular_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
csm_vsm_data$vascular_higher <- sapply(strsplit(csm_vsm_data$vascular_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
ctm_vtm_data$cardiac_lower <- sapply(strsplit(ctm_vtm_data$cardiac_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
ctm_vtm_data$cardiac_higher <- sapply(strsplit(ctm_vtm_data$cardiac_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
ctm_vtm_data$vascular_lower <- sapply(strsplit(ctm_vtm_data$vascular_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
ctm_vtm_data$vascular_higher <- sapply(strsplit(ctm_vtm_data$vascular_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))

csm_vsm_data$cardiac_events <- paste(cardiac_serious$event.e, "/", cardiac_serious$n.e)
csm_vsm_data$vascular_events <- paste(vascular_serious$event.e, "/", vascular_serious$n.e)
ctm_vtm_data$cardiac_events <- paste(cardiac_total$event.e, "/", cardiac_total$n.e)
ctm_vtm_data$vascular_events <- paste(vascular_total$event.e, "/", vascular_total$n.e)

write.csv(csm_vsm_data, 'cardiac-vascular-serious-meta.csv')
write.csv(ctm_vtm_data, 'cardiac-vascular-total-meta.csv')

arsm_hfsm_vtsm_hysm_data$Arrhythmias_lower <- sapply(strsplit(arsm_hfsm_vtsm_hysm_data$Arrhythmias_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
arsm_hfsm_vtsm_hysm_data$Arrhythmias_higher <- sapply(strsplit(arsm_hfsm_vtsm_hysm_data$Arrhythmias_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
arsm_hfsm_vtsm_hysm_data$Heart_Failure_lower <- sapply(strsplit(arsm_hfsm_vtsm_hysm_data$Heart_Failure_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
arsm_hfsm_vtsm_hysm_data$Heart_Failure_higher <- sapply(strsplit(arsm_hfsm_vtsm_hysm_data$Heart_Failure_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
arsm_hfsm_vtsm_hysm_data$Vascular_Toxicity_lower <- sapply(strsplit(arsm_hfsm_vtsm_hysm_data$Vascular_Toxicity_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
arsm_hfsm_vtsm_hysm_data$Vascular_Toxicity_higher <- sapply(strsplit(arsm_hfsm_vtsm_hysm_data$Vascular_Toxicity_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
arsm_hfsm_vtsm_hysm_data$Hypertension_lower <- sapply(strsplit(arsm_hfsm_vtsm_hysm_data$Hypertension_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
arsm_hfsm_vtsm_hysm_data$Hypertension_higher <- sapply(strsplit(arsm_hfsm_vtsm_hysm_data$Hypertension_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))

artm_hftm_vttm_hytm_data$Arrhythmias_lower <- sapply(strsplit(artm_hftm_vttm_hytm_data$Arrhythmias_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
artm_hftm_vttm_hytm_data$Arrhythmias_higher <- sapply(strsplit(artm_hftm_vttm_hytm_data$Arrhythmias_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
artm_hftm_vttm_hytm_data$Heart_Failure_lower <- sapply(strsplit(artm_hftm_vttm_hytm_data$Heart_Failure_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
artm_hftm_vttm_hytm_data$Heart_Failure_higher <- sapply(strsplit(artm_hftm_vttm_hytm_data$Heart_Failure_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
artm_hftm_vttm_hytm_data$Vascular_Toxicity_lower <- sapply(strsplit(artm_hftm_vttm_hytm_data$Vascular_Toxicity_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
artm_hftm_vttm_hytm_data$Vascular_Toxicity_higher <- sapply(strsplit(artm_hftm_vttm_hytm_data$Vascular_Toxicity_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))
artm_hftm_vttm_hytm_data$Hypertension_lower <- sapply(strsplit(artm_hftm_vttm_hytm_data$Hypertension_CI, ";|\\[|\\]"), function(x) as.numeric(x[2]))
artm_hftm_vttm_hytm_data$Hypertension_higher <- sapply(strsplit(artm_hftm_vttm_hytm_data$Hypertension_CI, ";|\\[|\\]"), function(x) as.numeric(x[3]))

arsm_hfsm_vtsm_hysm_data$Arrhythmias_events <- paste(arrhy_serious$event.e, "/", arrhy_serious$n.e)
arsm_hfsm_vtsm_hysm_data$Heart_Failure_events <- paste(hf_serious$event.e, "/", hf_serious$n.e)
arsm_hfsm_vtsm_hysm_data$Vascular_Toxicity_events <- paste(vt_serious$event.e, "/", vt_serious$n.e)
arsm_hfsm_vtsm_hysm_data$Hypertension_events <- paste(hyper_serious$event.e, "/", hyper_serious$n.e)

artm_hftm_vttm_hytm_data$Arrhythmias_events <- paste(arrhy_total$event.e, "/", arrhy_total$n.e)
artm_hftm_vttm_hytm_data$Heart_Failure_events <- paste(hf_total$event.e, "/", hf_total$n.e)
artm_hftm_vttm_hytm_data$Vascular_Toxicity_events <- paste(vt_total$event.e, "/", vt_total$n.e)
artm_hftm_vttm_hytm_data$Hypertension_events <- paste(hyper_total$event.e, "/", hyper_total$n.e)

write.csv(arsm_hfsm_vtsm_hysm_data, 'arrhythmias-heart failure-Vascular Toxicity-hypertension-serious-meta.csv')
write.csv(artm_hftm_vttm_hytm_data, 'arrhythmias-heart failure-Vascular Toxicity-hypertension-total-meta.csv')


# ----------------------------- graph_generation ----------------------------- #
# The meta-package was used to calculate ORs and 95% (CIs), and the data were collated into the forestploter plotting format for use in the main figure 5A and the supplementary plots.

# The path to the data folder
file_path <- "/your_path_to_project/NMA-for-EGFR-TKI/input/" # Change to your data folder path
data <- read.csv(paste0(file_path, "primary-serious-total-meta.csv"), header = TRUE, check.names = F)

nrow(data)
colnames(data)

data$"Serious Adverse Drug Reactions" <- paste(rep(" ", 15), collapse = " ")
data$"Total Adverse Drug Reactions" <- paste(rep(" ", 15), collapse = " ")

data$"Serious OR(95%CI)"[grepl("NA", data$"Serious OR(95%CI)")] <- "" 
data$"Total OR(95%CI)"[grepl("NA", data$"Total OR(95%CI)")] <- "" 

data$se_cardiac_s <- (log(data$cardiac_higher_s) - log(data$Cardiac_s))/1.96
data$se_vascular_s <- (log(data$vascular_higher_s) - log(data$Vascular_s))/1.96
data$se_cardiac_t <- (log(data$cardiac_higher_t) - log(data$Cardiac_t))/1.96
data$se_vascular_t <- (log(data$vascular_higher_t) - log(data$Vascular_t))/1.96

tm <- forestploter::forest_theme(
  core=list(bg_params=list(fill = c("#EEE9E9", "white", "white"))), 
      base_size = 10,                
      refline_lty = "solid",        
      ci_pch = c(15, 18),             
      ci_col = c("#00B2A9FF",  "#CD202CFF"),  
      ci_lty = 1,
      ci_lwd = 2.5,
      ci_Theight = 0.35,
      footnote_col = "black",         
      legend_name = "Group",         
      legend_value = c("Cardiac", "Vascular"),  
      refline_lwd = 1,
      vertline_lty = "dashed",
      refline_col = "grey20")

data1 <- as.data.frame(data[, c(6:17)]) %>% mutate_all(as.numeric)
all_p1 <- forestploter::forest(data[, c(1, 2, 18, 4, 3, 19, 5)], 
      est = list(data1$Cardiac_s, data1$Cardiac_t, data1$Vascular_s, data1$Vascular_t), 
      lower = list(data1$cardiac_lower_s, data1$cardiac_lower_t, data1$vascular_lower_s, data1$vascular_lower_t), 
      upper = list(data1$cardiac_higher_s, data1$cardiac_higher_t, data1$vascular_higher_s, data1$vascular_higher_t), 
      ci_column = c(3,6), nudge_y = 0.2, xlim = list(c(0,5),c(0,5)), theme = tm,  ticks_at = list(c(0,1,3,5),c(0,1,3,5)),
      ref_line = 1, arrow_lab = c("Favours Control","Favours Experimental"),line_size = 2.5,
      sizes = list(2.5*data$se_cardiac_s, 2.5*data$se_cardiac_t, 2.5*data$se_vascular_s, 2.5*data$se_vascular_t))

plot(all_p1)
pdf("Figure5-forestplot.pdf", width = 15, height = 6)
plot(all_p1)
dev.off()

# ----------------------------- graph_generation ----------------------------- #

# The path to the data folder
file_path <- "/your_path_to_project/NMA-for-EGFR-TKI/input/" # Change to your data folder path

data <- read.csv(paste0(file_path, "secondary-serious-total-meta.csv"), header = TRUE, check.names = F)

data$"Serious Adverse Drug Reactions" <- paste(rep(" ", 25), collapse = " ")
data$"Total Adverse Drug Reactions" <- paste(rep(" ", 25), collapse = " ")

data$"Serious OR(95%CI)"[grepl("NA", data$"Serious OR(95%CI)")] <- "" # Any NA to blank
data$"Total OR(95%CI)"[grepl("NA", data$"Total OR(95%CI)")] <- "" # Any NA to blank

data$se_Arrhythmias_s <- (log(data$Arrhythmias_higher_s) - log(data$Arrhythmias_s))/1.96
data$se_Heart_Failure_s <- (log(data$Heart_Failure_higher_s) - log(data$Heart_Failure_s))/1.96
data$se_Vascular_Toxicity_s <- (log(data$Vascular_Toxicity_higher_s) - log(data$Vascular_Toxicity_s))/1.96
data$se_Hypertension_s <- (log(data$Hypertension_higher_s) - log(data$Hypertension_s))/1.96

data$se_Arrhythmias_t <- (log(data$Arrhythmias_higher_t) - log(data$Arrhythmias_t))/1.96
data$se_Heart_Failure_t <- (log(data$Heart_Failure_higher_t) - log(data$Heart_Failure_t))/1.96
data$se_Vascular_Toxicity_t <- (log(data$Vascular_Toxicity_higher_t) - log(data$Vascular_Toxicity_t))/1.96
data$se_Hypertension_t <- (log(data$Hypertension_higher_t) - log(data$Hypertension_t))/1.96

tm <- forestploter::forest_theme(
  core=list(bg_params=list(fill = c("#EEE9E9", "white", "white", "white", "white"))), 
  base_size = 10,                
  refline_lty = "solid",        
  ci_pch = c(15, 15, 18, 18),           
  ci_col = c("#00B2A9FF","#2A6EBBFF",  "#CD202CFF","#7D5CC6FF"), 
  ci_lty = 1,
  ci_lwd = 2.5,
  ci_Theight = 0.25,
  footnote_col = "black",         
  legend_name = "Group",         
  legend_value = c("Arrhythmias", "Heart Failure","Vascular Toxicity","Hypertension"),   
  refline_lwd = 1,
  vertline_lty = "dashed",
  refline_col = "grey20")


data1 <- as.data.frame(data[, c(6:29)]) %>% mutate_all(as.numeric)

all_p1 <- forestploter::forest(data[, c(1, 2, 30, 4, 3, 31, 5)], 
                               est = list(data1$Arrhythmias_s, data1$Arrhythmias_t, data1$Heart_Failure_s, data1$Heart_Failure_t,data1$Vascular_Toxicity_s, data1$Vascular_Toxicity_t,data1$Hypertension_s, data1$Hypertension_t),  
                               lower = list(data1$Arrhythmias_lower_s, data1$Arrhythmias_lower_t, data1$Heart_Failure_lower_s, data1$Heart_Failure_lower_t, data1$Vascular_Toxicity_lower_s, data1$Vascular_Toxicity_lower_t, data1$Hypertension_lower_s, data1$Hypertension_lower_t), 
                               upper = list(data1$Arrhythmias_higher_s, data1$Arrhythmias_higher_t, data1$Heart_Failure_higher_s, data1$Heart_Failure_higher_t, data1$Vascular_Toxicity_higher_s, data1$Vascular_Toxicity_higher_t, data1$Hypertension_higher_s, data1$Hypertension_higher_t), 
                               ci_column = c(3,6), nudge_y = 0.2, xlim = list(c(0,5),c(0,6)), theme = tm,  ticks_at = list(c(0,1,3,5),c(0,1,3,6)),
                               ref_line = 1, arrow_lab = c("Favours Control","Favours Experimental"),
                               sizes = list(data$se_Arrhythmias_s, data$se_Arrhythmias_t, data$se_Heart_Failure_s, data$se_Heart_Failure_t,
                                            data$se_Vascular_Toxicity_s, data$se_Vascular_Toxicity_t, data$se_Hypertension_s, data$se_Hypertension_t))

plot(all_p1)

pdf("Figure5-forestplot-appendix.pdf", width = 15, height = 8)
plot(all_p1)
dev.off()
