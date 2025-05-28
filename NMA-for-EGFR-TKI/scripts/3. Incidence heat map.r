# file name: 3. Incidence heat map.r
# Function: The incidence heat map of the first to third generation EGFR TKIs based on primary and secondary outcomes.
# Input: input/Heatmap.xlsx
# output: outcomes/Heatmap
# Category: input_settings, graph_generation
# Date: 2025-04-24
# Author: Ma zidong



# Load necessary libraries
library(here)
library(ComplexHeatmap)
library(circlize)
library(GetoptLong)
library(readxl)

#### input_settings ####

# Set the main output directory
main_output_dir <- here("outcomes", "Heatmap")
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)

# The path to the data folder
file <- here("input", "Heatmap.xlsx")
primary <- read_excel(file, sheet = "primary")
secondary <- read_excel(file, sheet = "secondary")

#### graph_generation ####
# Generation of heat maps of the incidence of first- to third generation EGFR-TKIs, based on primary outcome (Figure 5B) and secondary outcome (Supplementary Material)
data <- primary
data <- as.data.frame(data)
rownames(data) <- data[,1]
data <- data[,-1]
data1=as.data.frame(lapply(data,as.numeric))
rownames(data1) <- rownames(data)
colnames(data1)

serious <- data1[,c(1:2)]
total <- data1[,c(3:4)]
colnames(serious) <- c("Cardiac","Vascular")
colnames(total) <- c("Cardiac","Vascular")
serious1 <- scale(serious)
total1 <- scale(total)

col_fun = colorRamp2(c(-3, 0, 3), c("#2A6EBBFF",  "white", "#CD202CFF"))#BMJ
col = c("#ED000099", "#0099B499")
col1 = c("#925E9F99", "#00468B99", "#FFDC9199")

serious1[is.na(serious1)] <- 0
ht = Heatmap(serious1, col = col_fun, name = "HLGT-serious", 
      border_gp = gpar(col = "black"), rect_gp = gpar(col = "dimgray", lwd = 0.7), 
      show_row_names = FALSE, row_names_gp = gpar(fontsize = 10), 
      cluster_columns = F, cluster_rows = F, column_names_rot = 45, 
      column_names_gp = gpar(fontsize = 10), heatmap_width = unit(6.5, "cm"), #6
      heatmap_height = unit(14, "cm"), show_heatmap_legend = FALSE,
      cell_fun = function(j, i, x, y, width, height, fill) {
      grid.text(sprintf("%.1f", serious[i, j]), x, y, gp = gpar(fontsize = 8))}, 
      top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = col[1]),
      labels = c("Serious"))),row_title = NULL,
      left_annotation = rowAnnotation(Incidence = anno_boxplot(serious1, height = unit(2, "cm"), gp = gpar(fill = col1[1]))))
ht

total1[is.na(total1)] <- 0
ht1 = Heatmap(total1, col = col_fun, name = "HLGT-total", 
      border_gp = gpar(col = "black"), rect_gp = gpar(col = "dimgray", lwd = 0.7), 
      show_row_names = TRUE, row_names_gp = gpar(fontsize = 10), 
      cluster_columns = F, cluster_rows = F, column_names_rot = 45, 
      column_names_gp = gpar(fontsize = 10), heatmap_width = unit(11, "cm"), #10.5
      heatmap_height = unit(14, "cm"), show_heatmap_legend = FALSE,
      cell_fun = function(j, i, x, y, width, height, fill) {
      grid.text(sprintf("%.1f", total[i, j]), x, y, gp = gpar(fontsize = 8))}, 
      top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = col[2]),
      labels = c("Total"))),row_title = NULL,
      left_annotation = rowAnnotation(Incidence = anno_boxplot(total1, height = unit(2, "cm"), gp = gpar(fill = col1[2]))))
ht1

ht3 = ht + ht1
ht3
pdf(file.path(main_output_dir, "primary-serious-total-heatmap.pdf"), width = 9)
plot(ht3)
dev.off()


data <- secondary
data <- as.data.frame(data)
rownames(data) <- data[,1]
data <- data[,-1]
data1=as.data.frame(lapply(data,as.numeric))
rownames(data1) <- rownames(data)
colnames(data1)

serious <- data1[,c(1:4)]
total <- data1[,c(5:8)]
colnames(serious) <- c("Arrhythmias","Heart Failure","Vascular Toxicity","Hypertension")
colnames(total) <- c("Arrhythmias","Heart Failure","Vascular Toxicity","Hypertension")

serious1 <- scale(serious)
total1 <- scale(total)
serious1[is.na(serious1)] <- 0
ht = Heatmap(serious1, col = col_fun, name = "HLGT-serious", 
      border_gp = gpar(col = "black"), rect_gp = gpar(col = "dimgray", lwd = 0.7), 
      show_row_names = FALSE, row_names_gp = gpar(fontsize = 10), 
      cluster_columns = F, cluster_rows = F, column_names_rot = 45, 
      column_names_gp = gpar(fontsize = 10), heatmap_width = unit(8.5, "cm"), #6
      heatmap_height = unit(14, "cm"), show_heatmap_legend = FALSE,
      cell_fun = function(j, i, x, y, width, height, fill) {
      grid.text(sprintf("%.1f", serious[i, j]), x, y, gp = gpar(fontsize = 8))}, 
      top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = col[1]),
      labels = c("Serious"))),row_title = NULL,
      left_annotation = rowAnnotation(Incidence = anno_boxplot(serious1, height = unit(2, "cm"), gp = gpar(fill = col1[1]))))
ht

total1[is.na(total1)] <- 0
ht1 = Heatmap(total1, col = col_fun, name = "HLGT-total", 
      border_gp = gpar(col = "black"), rect_gp = gpar(col = "dimgray", lwd = 0.7), 
      show_row_names = TRUE, row_names_gp = gpar(fontsize = 10), 
      cluster_columns = F, cluster_rows = F, column_names_rot = 45, 
      column_names_gp = gpar(fontsize = 10), heatmap_width = unit(13.5, "cm"), #10.5
      heatmap_height = unit(14, "cm"), show_heatmap_legend = FALSE,
      cell_fun = function(j, i, x, y, width, height, fill) {
      grid.text(sprintf("%.1f", total[i, j]), x, y, gp = gpar(fontsize = 8))}, 
      top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = col[2]),
      labels = c("Total"))),row_title = NULL,
      left_annotation = rowAnnotation(Incidence = anno_boxplot(total1, height = unit(2, "cm"), gp = gpar(fill = col1[2]))))
ht1

ht3 = ht + ht1
ht3
pdf(file.path(main_output_dir, "secondary-serious-total-heatmap.pdf"), width = 12)
plot(ht3)
dev.off()

