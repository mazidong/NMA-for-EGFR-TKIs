# file name: 7. Bayesian-network-meta-analysis.r
# Function: Bayesian network meta-analysis was performed based on primary and secondary outcomes, and SUCRA values were collected for each therapy to draw heat maps.
# Input: input/Bayesian-network-meta-data.xlsx
# output: outcomes/Bayesian
# Category: input_settings, Bayesian network meta-analysis, plotting
# Date: 2025-04-24
# Author: Ma zidong




# Load necessary libraries
library(readxl)
library(gemtc)
library(rjags)
library(dplyr)
library(readxl)
library(ComplexHeatmap)
library(circlize)
library(GetoptLong)
library(stringr)


# ----------------------------------input_settings  --------------------------------- #

# Set the main output directory
main_output_dir <- "/your_path_to_project/NMA-for-EGFR-TKI/outcomes/Bayesian/" # Change to your main output directory
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)
setwd(main_output_dir)

# The path to the data folder
file_path <- "/your_path_to_project/NMA-for-EGFR-TKI/input/" # Change to your data folder path
excel_path <- paste(file_path, "Bayesian-network-meta-data.xlsx", sep = "") 


# Custom functions: Breakpoint continuous transmission
skip_if_exists <- function(file, code_block) {
  if (!file.exists(file)) {
    code_block()
  } else {
    message("File already exists, skipping: ", file)
  }
}


primary <- c("Serious Cardiac","Serious Cardiac (Gen)",
             "Total Cardiac","Total Cardiac (Gen)",
             "Serious Vascular","Serious Vascular (Gen)",
             "Total Vascular","Total Vascular (Gen)")
secondary <- c("Serious Arrhythmias","Serious Arrhythmias (Gen)",
               "Total Arrhythmias","Total Arrhythmias (Gen)",
               "Serious Heart Failure", "Serious Heart Failure (Gen)",
               "Total Heart Failure","Total Heart Failure (Gen)",
               "Serious Vascular Toxicity","Serious Vascular Toxicity (Gen)",
               "Total Vascular Toxicity","Total Vascular Toxicity (Gen)",
               "Serious Hypertension", "Serious Hypertension (Gen)", 
               "Total Hypertension", "Total Hypertension (Gen)")

categories <- list(primary = primary, secondary = secondary)

# ----------------------------------Bayesian network meta-analysis --------------------------------- #
for (category_name in names(categories)) {
  cat("Current Category:", category_name, "\n")
  for (i in categories[[category_name]]) {
    timestamp <- Sys.time()
    tryCatch({
      # Read data
      print(paste0("[", timestamp, "]############################",i,"######################################"))
      setwd(main_output_dir)
      excel_path[is.na(excel_path)] <- 0
      data <- read_excel(excel_path, sheet = i)
      data1 <- as.data.frame(data)
      pathway <- paste0(main_output_dir,category_name,'/')
      if (!dir.exists(pathway)) dir.create(pathway)
      setwd(pathway)
      # Create network and model
      network <- mtc.network(data1)
      model <- mtc.model(network, type = "consistency", n.chain = 4, likelihood = "binom", link = "logit", linearModel = "random", dic = TRUE)
      
      # Run MCMC analysis
      results <- mtc.run(model, n.adapt = 100000, n.iter = 200000, thin = 10)
      
      # Save forest plot
      pdf_file_forest <- paste0("Gemtc-forest-", i, ".pdf")
      skip_if_exists(pdf_file_forest, function() {
      pdf(pdf_file_forest)
      forest(relative.effect(results, "Placebo"))
      dev.off()
      message("[", timestamp, "]Saved forest plot: ", pdf_file_forest)
      })

      # Save trace/density plot
      pdf_file_density <- paste0("Trace_density-", i, ".pdf")
      skip_if_exists(pdf_file_density, function() {
      pdf(pdf_file_density)
      plot(results)
      dev.off()
      message("[", timestamp, "]Saved trace/density plot: ", pdf_file_density)
      })
                 
      # Diagnostic diagram
      pdf_file_diagnosis <- paste0("Diagnosis-", i, ".pdf")
      skip_if_exists(pdf_file_diagnosis, function() {
      pdf(pdf_file_diagnosis)
      gelman.plot(results)
      dev.off()
      message("[", timestamp, "]Saved diagnosis plot: ", pdf_file_diagnosis)
      })
      
      
      out_path <- file.path(pathway, paste0(i, "_gelman.diag.txt"))
      sink(out_path)
      print(paste0(i,"[", timestamp, "]: gelman.diag"))
      print(gelman.diag(results))
      sink()
      
      ranks <- rank.probability(results)
      cumrank.prob <- apply(t(ranks), 2, cumsum)
      sucra <- round(colMeans(cumrank.prob[-nrow(cumrank.prob),]),4)
      write.csv(sucra,paste0(i,"_gemtc_sucra.csv"))
      
      result.node <- mtc.nodesplit(network, thin=1)
      summary.ns <- summary(result.node)
      
      out_path1 <- file.path(pathway, paste0(i, "_result.node.txt"))
      sink(out_path1)
      print(paste0(i,"[", timestamp, "]: mtc.nodesplit"))
      print(summary(result.node))
      sink()
      

      # Consistency test
      pdf_file_diagnosis1 <- paste0("Consistency-", i, ".pdf")
      skip_if_exists(pdf_file_diagnosis1 , function() {
      pdf(pdf_file_diagnosis1)
      plot(summary.ns)
      dev.off()
      message("[", timestamp, "]Saved diagnosis1 plot: ", pdf_file_diagnosis1)
      })
      
      resultanohe <- mtc.anohe(network, n.adapt=5000, n.iter=20000, thin=1, n.chain=4, likelihood="binom", link="logit", linearModel="random")
      c <- summary(resultanohe)
      
      out_path2 <- file.path(pathway, paste0(i, "_mtc.anohe.txt"))
      sink(out_path2)
      print(paste0(i,"[", timestamp, "]: mtc.anohe"))
      print(c)
      sink()
      

      # Heterogeneous inspection
      pdf_file_diagnosis2 <- paste0("Heterogeneity-", i, ".pdf")
      skip_if_exists(pdf_file_diagnosis2 , function() {
      pdf(pdf_file_diagnosis2)
      plot(c)
      dev.off()
      message("[", timestamp, "]Saved diagnosis1 plot: ", pdf_file_diagnosis2)
      })
      
      # Make a league table
      a <- relative.effect.table(results)
      write.csv(a,paste0("Gmmtc-matrix_",i,".csv"))
      
    }, error = function(e) {
      # Log errors and continue with the next iteration
      message("[", timestamp, "]Error occurred in sheet ", i, ": ", conditionMessage(e))
    })
  }
}



# --------------------------------- plotting --------------------------------- #

setwd(paste0(main_output_dir, 'primary/'))
list.files()

test <- data.frame(group = character())
list.files(pattern = "*\\gemtc_sucra.*\\.csv$")
for (file in list.files(pattern = "*\\gemtc_sucra.*\\.csv$")){
      if (grepl("csv", file)){
            print(file)
            data <- read.csv(file, header = T)
            colnames(data) <- c('group', str_split(file,'_')[[1]][1])
            test <- full_join(test, data, by = "group")
      }
}
colnames(test)
write.csv(test, "primary outcomes-sucra-heatmap.csv")

setwd(paste0(main_output_dir, 'secondary/'))
list.files()
test <- data.frame(group = character())
list.files(pattern = "*\\gemtc_sucra.*\\.csv$")
for (file in list.files(pattern = "*\\gemtc_sucra.*\\.csv$")){
      if (grepl("csv", file)){
            print(file)
            data <- read.csv(file, header = T)
            colnames(data) <- c('group', str_split(file,'_')[[1]][1])
            test <- full_join(test, data, by = "group")
      }
}
colnames(test)
write.csv(test, "secondary outcomes-sucra-heatmap.csv")

setwd(main_output_dir)

primary <- read.csv(file.path(paste0(main_output_dir, 'primary/'), "primary outcomes-sucra-heatmap.csv"), check.names = F, row.names = 1)
secondary <- read.csv(file.path(paste0(main_output_dir, 'secondary/'), "secondary outcomes-sucra-heatmap.csv"), check.names = F, row.names = 1)
total <- full_join(primary, secondary, by = "group")
write.csv(total, file.path(main_output_dir, "primary-secondary-outcomes-sucra-heatmap.csv"))


# Organize the merged sucra matrix into a format suitable for drawing
file_path <- "/your_path_to_project/NMA-for-EGFR-TKI/input/" # Change to your data folder path
excel_path1 <- paste(file_path, "primary-secondary-outcomes-sucra-heatmap.xlsx", sep = "") 

data <- read_excel(excel_path1, sheet = "EGFR-TKI")
data1 <- read_excel(excel_path1, sheet = "interest-treatment")
data2 <- read_excel(excel_path1, sheet = "all-treatments")

data <- as.data.frame(data)
rownames(data) <- data[,1]
data <- data[,-1]
data1 <- as.data.frame(data1)
rownames(data1) <- data1[,1]
data1 <- data1[,-1]
data2 <- as.data.frame(data2)
rownames(data2) <- data2[,1]
data2 <- data2[,-1]

data[data == 'NA'] <- ''
data_numeric <-as.data.frame(lapply(data, as.numeric))
colnames(data_numeric) <- colnames(data)
rownames(data_numeric) <- rownames(data)
data_scaled <- scale(data_numeric)            


data1[data1 == 'NA'] <- ''
data_numeric1 <-as.data.frame(lapply(data1, as.numeric))
colnames(data_numeric1) <- colnames(data1)
rownames(data_numeric1) <- rownames(data1)
data_scaled1 <- scale(data_numeric1)            


data2[data2 == 'NA'] <- ''
data_numeric2 <-as.data.frame(lapply(data2, as.numeric))
colnames(data_numeric2) <- colnames(data2)
rownames(data_numeric2) <- rownames(data2)
data_scaled2 <- scale(data_numeric2)            

col_fun = colorRamp2(c(-3, 0, 3), c("#2A6EBBFF",  "white", "#CD202CFF"))#BMJ
data_scaled[is.na(data_scaled)] <- 0
data_scaled1[is.na(data_scaled1)] <- 0
data_scaled2[is.na(data_scaled2)] <- 0

EGFR_TKI = Heatmap(data_scaled, col = col_fun, name = "SUCRA", 
             border_gp = gpar(col = "black"), rect_gp = gpar(col = "dimgray", lwd = 0.7), 
             show_row_names = TRUE, row_names_gp = gpar(fontsize = 10), 
             cluster_columns = F, cluster_rows = F, column_names_rot = 45, 
             column_names_gp = gpar(fontsize = 10), heatmap_width = unit(16, "cm"), 
             heatmap_height = unit(16, "cm"), show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.2f", data_numeric[i, j]), x, y, gp = gpar(fontsize = 8))}, row_title = NULL)
plot(EGFR_TKI)

interest = Heatmap(data_scaled1, col = col_fun, name = "SUCRA", 
             border_gp = gpar(col = "black"), rect_gp = gpar(col = "dimgray", lwd = 0.7), 
             show_row_names = TRUE, row_names_gp = gpar(fontsize = 10), 
             cluster_columns = F, cluster_rows = T, column_names_rot = 45, 
             column_names_gp = gpar(fontsize = 10), heatmap_width = unit(16, "cm"), 
             heatmap_height = unit(16, "cm"), show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.2f", data_numeric1[i, j]), x, y, gp = gpar(fontsize = 8))}, row_title = NULL)
plot(interest)

all = Heatmap(data_scaled2, col = col_fun, name = "SUCRA", 
             border_gp = gpar(col = "black"), rect_gp = gpar(col = "dimgray", lwd = 0.7), 
             show_row_names = TRUE, row_names_gp = gpar(fontsize = 10), 
             cluster_columns = F, cluster_rows = T, column_names_rot = 45, 
             column_names_gp = gpar(fontsize = 10), heatmap_width = unit(20, "cm"), 
             heatmap_height = unit(30, "cm"), show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.2f", data_numeric2[i, j]), x, y, gp = gpar(fontsize = 8))}, row_title = NULL)
plot(all)

pdf("SUCRA-heatmap-0418-alltreatments.pdf", height = 14, width = 10)
plot(all)
dev.off()

pdf("SUCRA-heatmap-0418-interest-EGFR-TKI.pdf", height = 8, width = 8)
plot(interest)
plot(EGFR_TKI)
dev.off()
