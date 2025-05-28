# file name: 8. Network meta-analysis for efficacy analysis.r
# Function: Efficacy analysis of PFS (progression-Free-Survival) data from included RCTs using network meta-analysis.
# Input: input/PFS-netmeta.xlsx
# output: outcomes/PFS-netmeta
# Category: input_settings, network meta-analysis, plotting
# Date: 2025-04-24
# Author: Ma zidong




# Load necessary libraries
library(here)
library(netmeta)
library(readxl)
library(openxlsx)


#### input_settings ####

# Set the main output directory
main_output_dir <- here("outcomes", "PFS-netmeta")
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)

# The path to the data folder
excel_path <- here("input", "PFS-netmeta.xlsx")  

data <- read_excel(excel_path, sheet = "Sheet1")


head(data)
data <- as.data.frame(data)
data$TE <- log(data$HR)
data$seTE <- (log(data$CI_high) - log(data$CI_low))/3.92

#### network meta-analysis ####
m.netmeta <- netmeta(TE = TE,  # TE = log(HR)
                     seTE = seTE,  # seTE = (log(upperCI) - log(lowerCI))/3.92)
                     treat1 = treat1,
                     treat2 = treat2,
                     studlab = old,
                     data = data,
                     sm = "HR",
                     reference.group = "Placebo",
                     sep.trts = " vs ")



#### plotting ####
pdf(file.path(main_output_dir, "PFS-netmeta-graph.pdf"), width = 8.5)
netgraph(m.netmeta,  seq = "optimal",
         points=T,
         labels = trts,
         alpha.transparency = 0.8,
         cex.points = k.trts,
         plastic=F, 
         col = "black",
         col.points = "#91cbd9",
         bg.points = "#91cbd9",
         number.of.studies = F, 
         cex=0.7,scale = 1.15,
         multiarm = F,thickness = "number.of.studies")
dev.off()

pdf(file.path(main_output_dir, "PFS-netmeta-graph-1.pdf"), width = 8.5)
netgraph(m.netmeta, seq = "optimal",
       points=T,
       labels = trts,
       alpha.transparency = 0.8,
       cex.points = k.trts,
       plastic=F, 
       col = "black",
       col.points = "#91cbd9",
       bg.points = "#91cbd9",
       cex=0.7,scale = 1.15,
       multiarm = F,
       thickness = "number.of.studies",
       lwd.min = 1, lwd.max = 10,
       number.of.studies = TRUE,
       pos.number.of.studies = 0.5,
       cex.number.of.studies = 0.7
    )
dev.off()

pdf(file.path(main_output_dir, "PFS-netmeta-forest.pdf"), width = 10, height = 10)
forest(m.netmeta,
       reference.group = "Placebo",
       #smlab = "Comparison: other vs Placebo",
       drop.reference.group = TRUE,
       #label.left = "HR",
       col.square = "#91cbd9",
       drop = TRUE,
       sortvar = -TE)

dev.off()

pdf(file.path(main_output_dir, "PFS-netmeta-forest-1.pdf"), width = 10, height = 10)
forest(m.netmeta,
       reference.group = "Placebo",
       drop.reference.group = TRUE,
       col.square = "#91cbd9",
       drop = TRUE,
       rightcols = c("effect", "ci"), leftcols=c("studlab", "k", "Pscore"), 
       sortvar=Pscore)
dev.off()

league1 <- netleague(m.netmeta, digits = 2, seq = netrank(m.netmeta, small.values = "good"),bracket = "(", separator = " to ")
write.table(league1$random, file = file.path(main_output_dir, "league1-PFS-netmeta-netrank-good.csv"),row.names = FALSE, col.names = FALSE, sep = ",")
pdf(file.path(main_output_dir, "PFS-netmeta-netsplit.pdf"), height = 22, width = 10)
meta::forest(netsplit(m.netmeta), col.square = "#0000ff", col.square.lines = "#000000", col.inside = "#000000", col.diamond = "#000000", col.diamond.lines = "#000000")#,show = "indirect.only")
dev.off()

