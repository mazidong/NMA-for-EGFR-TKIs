# file name: 4. Funnel_plot.r
# Function: Generating funnel plots for network meta-analyses of primary outcomes, secondary outcomes and retention of high quality studies.
# Input: input/network_data.xlsx
# output: outcomes/funnel
# Category: input_settings, network_meta_analysis, funnel_plot
# Date: 2025-04-24
# Author: Ma zidong



# Load necessary libraries
library(here)
library(netmeta)
library(readxl)
library(openxlsx)

#### input_settings ####

# Set the main output directory
main_output_dir <- here("outcomes", "funnel")
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)

# The path to the data folder
excel_path <- here("input", "network_data.xlsx")  # Original data containing the main figures 2, 3, and 4 and supplementary materials for network meta-analysis

cardiac_serious <- read_excel(excel_path, sheet = "Serious Cardiac")
cardiac_total <- read_excel(excel_path, sheet = "Total Cardiac")
vascular_serious <- read_excel(excel_path, sheet = "Serious Vascular")
vascular_total <- read_excel(excel_path, sheet = "Total Vascular")
vascular_total1 <- read_excel(excel_path, sheet = "Total Vascular (funnel)")# retention of high quality studies

ar_serious <- read_excel(excel_path, sheet = "Serious Arrhythmias")
hf_serious <- read_excel(excel_path, sheet = "Serious Heart Failure")
hy_serious <- read_excel(excel_path, sheet = "Serious Hypertension")
vt_serious <- read_excel(excel_path, sheet = "Serious Vascular Toxicity")
ar_total <- read_excel(excel_path, sheet = "Total Arrhythmias")
hf_total <- read_excel(excel_path, sheet = "Total Heart Failure")
hy_total <- read_excel(excel_path, sheet = "Total Hypertension")
vt_total <- read_excel(excel_path, sheet = "Total Vascular Toxicity")
hy_total1 <- read_excel(excel_path, sheet = "Total Hypertension (funnel)")# retention of high quality studies


#### network_meta_analysis ####

# Perform pairwise meta-analysis for primary outcome
pw1 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = cardiac_serious, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw2 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = cardiac_total, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw3 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = vascular_serious, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw4 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = vascular_total, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw5 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = vascular_total1, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)

# Perform network meta-analysis for primary outcome
net1 <- netmeta(pw1, ref = "Placebo", common = FALSE)
net2 <- netmeta(pw2, ref = "Placebo", common = FALSE)
net3 <- netmeta(pw3, ref = "Placebo", common = FALSE)
net4 <- netmeta(pw4, ref = "Placebo", common = FALSE)
net5 <- netmeta(pw5, ref = "Placebo", common = FALSE)

#### funnel_plot ####
pdf(file.path(main_output_dir,"cardiac-serious-funnel.pdf"), height = 8, width = 8)
par(mar = c(16, 2,0.5,0.5), xpd = TRUE)
colourCount = 39
colors <- ggsci::pal_igv()(colourCount)
funnel(net1, order=unique(net1$treat1), legend = F, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- meta::funnel(net1, order=unique(net1$treat1), legend = F, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,39), text.width=0.75,x.intersp=0.45,legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.5)
dev.off()

pdf(file.path(main_output_dir,"cardiac-total-funnel.pdf"), height = 8, width = 8)
par(mar = c(16, 2,0.5,0.5), xpd = TRUE)
colourCount = 46
colors <- ggsci::pal_igv()(colourCount)
funnel(net2, order=unique(net2$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net2, order=unique(net2$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,46), text.width=0.8,x.intersp=0.35, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.5)
dev.off()

pdf(file.path(main_output_dir,"vascular-serious-funnel.pdf"), height = 8, width = 8)
colourCount = 39
colors <- ggsci::pal_igv()(colourCount)
par(mar = c(16, 2,0.5,0.5), xpd = TRUE)
funnel(net3, order=unique(net3$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net3, order=unique(net3$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,39), text.width=0.8,x.intersp=0.3, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.5)
dev.off()

pdf(file.path(main_output_dir,"vascular-total-funnel.pdf"), height = 8, width = 8)
colourCount = 45
colors <- ggsci::pal_igv()(colourCount)
par(mar = c(16, 2,0,0), xpd = TRUE)
funnel(net4, order=unique(net4$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net4, order=unique(net4$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.15),pch = rep(16,45), text.width=0.8,x.intersp=0.2, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.5)
dev.off()


pdf(file.path(main_output_dir,"vascular-total-funnel(Funnel plot correction).pdf"), height = 8, width = 8)
colourCount = 45
colors <- ggsci::pal_igv()(colourCount)
par(mar = c(16, 2,0,0), xpd = TRUE)
funnel(net5, order=unique(net5$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net5, order=unique(net5$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.15),pch = rep(16,45),text.width=0.8,x.intersp=0.3, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.5)
dev.off()


#### network_meta_analysis ####

# Perform pairwise meta-analysis for secondary outcome
pw1 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = ar_serious, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw2 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = hf_serious, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw3 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = hy_serious, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw4 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = vt_serious, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)

pw5 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = ar_total, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw6 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = hf_total, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw7 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = hy_total, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw8 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = vt_total, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
pw9 <- pairwise(treat = alloc1, n = sampleSize, event = responders, studlab = study, data = hy_total1, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)

# Perform network meta-analysis for secondary outcome
net1 <- netmeta(pw1, ref = "Placebo", common = FALSE)
net2 <- netmeta(pw2, ref = "Placebo", common = FALSE)
net3 <- netmeta(pw3, ref = "Placebo", common = FALSE)
net4 <- netmeta(pw4, ref = "Placebo", common = FALSE)
net5 <- netmeta(pw5, ref = "Placebo", common = FALSE)
net6 <- netmeta(pw6, ref = "Placebo", common = FALSE)
net7 <- netmeta(pw7, ref = "Placebo", common = FALSE)
net8 <- netmeta(pw8, ref = "Placebo", common = FALSE)
net9 <- netmeta(pw9, ref = "Placebo", common = FALSE)

#### funnel_plot ####
pdf(file.path(main_output_dir,"Arrhythmias_serious_funnel.pdf"), height = 8, width = 8)
par(mar = c(14, 2,0.5,0.5), xpd = TRUE)
colourCount = 32
colors <- ggsci::pal_igv()(colourCount)
funnel(net1, order=unique(net1$treat1), legend = F, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- meta::funnel(net1, order=unique(net1$treat1), legend = F, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,32), text.width=0.75,x.intersp=0.3,legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.75)
dev.off()

pdf(file.path(main_output_dir,"Arrhythmias_total_funnel.pdf"), height = 8, width = 8)
par(mar = c(14, 2,0.5,0.5), xpd = TRUE)
colourCount = 38
colors <- ggsci::pal_igv()(colourCount)
funnel(net5, order=unique(net5$treat1), legend = F, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- meta::funnel(net5, order=unique(net5$treat1), legend = F, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,38), text.width=0.75,x.intersp=0.3,legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.75)
dev.off()

pdf(file.path(main_output_dir,"Heart failure_serious_funnel.pdf"), height = 8, width = 8)
par(mar = c(12, 2,0.5,0.5), xpd = TRUE)
colourCount = 28
colors <- ggsci::pal_igv()(colourCount)
funnel(net2, order=unique(net2$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net2, order=unique(net2$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,28), text.width=0.7,x.intersp=0.3, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.75)
dev.off()


pdf(file.path(main_output_dir,"Heart failure_total_funnel.pdf"), height = 8, width = 8)
par(mar = c(12, 2,0.5,0.5), xpd = TRUE)
colourCount = 29
colors <- ggsci::pal_igv()(colourCount)
funnel(net6, order=unique(net6$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net6, order=unique(net6$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,29), text.width=0.7,x.intersp=0.3, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.75)
dev.off()

pdf(file.path(main_output_dir,"Hypertension_serious_funnel.pdf"), height = 8, width = 8)
colourCount = 21
colors <- ggsci::pal_igv()(colourCount)
par(mar = c(12.5, 2,0.5,0.5), xpd = TRUE)
funnel(net3, order=unique(net3$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net3, order=unique(net3$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,21), text.width=0.75,x.intersp=0.3, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.75)
dev.off()

pdf(file.path(main_output_dir,"Hypertension_total_funnel.pdf"), height = 8, width = 8)
colourCount = 29
colors <- ggsci::pal_igv()(colourCount)
par(mar = c(12.5, 2,0.5,0.5), xpd = TRUE)
funnel(net7, order=unique(net7$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net7, order=unique(net7$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,29), text.width=0.75,x.intersp=0.3, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.75)
dev.off()

pdf(file.path(main_output_dir,"Vascular Toxicity_serious_funnel.pdf"), height = 8, width = 8)
colourCount = 37
colors <- ggsci::pal_igv()(colourCount)
#funnel(net4, order=unique(net4$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col ="black", cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
par(mar = c(14, 2,0,0), xpd = TRUE)
funnel(net4, order=unique(net4$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net4, order=unique(net4$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.15),pch = rep(16,37), text.width=0.75,x.intersp=0.5, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.45, pt.cex = 0.75)
dev.off()

pdf(file.path(main_output_dir,"Vascular Toxicity_total_funnel.pdf"), height = 8, width = 8)
colourCount = 42
colors <- ggsci::pal_igv()(colourCount)
par(mar = c(14, 2,0,0), xpd = TRUE)
funnel(net8, order=unique(net8$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net8, order=unique(net8$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.15),pch = rep(16,42), text.width=0.75,x.intersp=0.5, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.45, pt.cex = 0.75)
dev.off()

pdf(file.path(main_output_dir,"Hypertension_total_funnel(Funnel plot correction).pdf"), height = 8, width = 8)
colourCount = 29
colors <- ggsci::pal_igv()(colourCount)
par(mar = c(12.5, 2,0.5,0.5), xpd = TRUE)
funnel(net9, order=unique(net9$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
cc <- funnel(net9, order=unique(net9$treat1), legend = FALSE, method.bias = "Egger", digits.pval = 2, col = colors, cex.studlab = 0.7,studlab=F,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1.5, pch = 16)
legend("topright", inset = c(0, 1.2),pch = rep(16,29), text.width=0.75,x.intersp=0.3, legend = cc$comparison,col=colors,bty="n",ncol = 4, cex = 0.5, pt.cex = 0.75)
dev.off()
