# file name: 8. Baseline_distribution.r
# Function: Generate baseline (sex, age, proportion of Asian population, proportion of non-smokers, median follow-up duration) distribution of all therapies included in the randomised controlled trial.
# Input: input/various distributed data.xlsx
# output: outcomes/Distribution
# Category: input_settings, figure_generation
# Date: 2025-04-24
# Author: Ma zidong





# Load necessary libraries
library(ggplot2)
library(ggsci)
# ----------------------------------input_settings  --------------------------------- #

# Set the main output directory
main_output_dir <- "/your_path_to_project/NMA-for-EGFR-TKI/outcomes/Distribution/" # Change to your main output directory

if (!dir.exists(main_output_dir)) dir.create(main_output_dir)
setwd(main_output_dir)

# The path to the data folder
file_path <- "/your_path_to_project/NMA-for-EGFR-TKI/input/" # Change to your data folder path

data <- read_excel(paste0(file_path, "various distributed data.xlsx"))
colnames(data)

# ----------------------------------figure_generation  --------------------------------- #
data1 <- subset(data, Median_Age != 'NR')
data1$Median_Age <- as.numeric(data1$Median_Age)

p <- ggplot(data1,aes(x=Author,y=data1$"Median_Age"))+
  geom_point(size=6,position = position_dodge(width = 0.5), aes(colour = factor(Group1)))+  
  theme_bw() + scale_y_continuous(limits = c(40, 80),breaks = seq(40, 80, by = 10))+
  facet_grid(. ~ Group, scales = "free_x", space = "free_x")+
  geom_hline(yintercept = 65, color = "black", linetype = "dashed", size = 0.7) +
  theme(
      axis.title.y = element_text(size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1),  
    strip.text.x = element_text(angle = 90,size = 15, face = "bold"),  
    panel.spacing = unit(0.5, "lines"))   + labs(x = "", y = "Median age(years)",color = "Group")

p1 <- p + scale_color_npg(alpha = 0.8)

pdf("all-study-median-mean-age.pdf", width = 30, height = 10)
print(p1)
dev.off()

#----------------------------------figure_generation  --------------------------------- #
data1 <- subset(data, median_follow_up != 'NR')
data1$median_follow_up <- as.numeric(data1$median_follow_up)


p <- ggplot(data1,aes(x=Author,y=data1$median_follow_up))+
  geom_point(size=6,position = position_dodge(width = 0.5), aes(colour = factor(Group)))+  
  theme_bw() + 
  facet_grid(. ~ Group, scales = "free_x", space = "free_x")+
  geom_hline(yintercept = 24, color = "black", linetype = "dashed", size = 0.7) +
  theme(
       axis.title.y = element_text(size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1),  
    strip.text.x = element_text(angle = 90,size = 15, face = "bold"), 
    panel.spacing = unit(0.5, "lines"))   + labs(x = "", y = "Median follow-up(months)")+theme(legend.position = "none") #


p1 <- p + scale_color_npg(alpha = 0.8)

pdf("all-study-Median follow-up.pdf", width = 30, height = 10)
print(p1)
dev.off()

#----------------------------------figure_generation  --------------------------------- #
data1 <- subset(data, Female != 'NR')
data1$Female <- as.numeric(data1$Female)

p <- ggplot(data1,aes(x=Author,y=data1$Female))+
  geom_point(size=6,position = position_dodge(width = 0.5), aes(colour = factor(Group)))+  
  theme_bw() + 
  facet_grid(. ~ Group, scales = "free_x", space = "free_x")+
  geom_hline(yintercept = 50, color = "black", linetype = "dashed", size = 0.7) +
  theme(
       axis.title.y = element_text(size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1), 
    strip.text.x = element_text(angle = 90,size = 15, face = "bold"),  
    panel.spacing = unit(0.5, "lines"))   + labs(x = "", y = "Female(%)")+theme(legend.position = "none") #

p1 <- p + scale_color_npg(alpha = 0.8)

pdf("all-study-female.pdf", width = 30, height = 10)
print(p1)
dev.off()

#----------------------------------figure_generation  --------------------------------- #
data1 <- subset(data, Asian != 'NR')
data1$Asian <- as.numeric(data1$Asian)

p <- ggplot(data1,aes(x=Author,y=data1$Asian))+
  geom_point(size=6,position = position_dodge(width = 0.5), aes(colour = factor(Group)))+  
  theme_bw() +
  facet_grid(. ~ Group, scales = "free_x", space = "free_x")+
  geom_hline(yintercept = 50, color = "black", linetype = "dashed", size = 0.7) +
  theme(
       axis.title.y = element_text(size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1),  
    strip.text.x = element_text(angle = 90,size = 15, face = "bold"),  
    panel.spacing = unit(0.5, "lines"))   + labs(x = "", y = "Asian(%)")+theme(legend.position = "none") #

p1 <- p + scale_color_npg(alpha = 0.8)

pdf("all-study-Asian.pdf", width = 30, height = 10)
print(p1)
dev.off()

#----------------------------------figure_generation  --------------------------------- #

data1 <- subset(data, Never_Smoker != 'NR')
data1$Never_Smoker <- as.numeric(data1$Never_Smoker)

p <- ggplot(data1,aes(x=Author,y=data1$Never_Smoker))+
  geom_point(size=6,position = position_dodge(width = 0.5), aes(colour = factor(Group)))+  
  theme_bw() + 
  facet_grid(. ~ Group, scales = "free_x", space = "free_x")+
  geom_hline(yintercept = 50, color = "black", linetype = "dashed", size = 0.7) +
  theme(
       axis.title.y = element_text(size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1), 
    strip.text.x = element_text(angle = 90,size = 15, face = "bold"),  
    panel.spacing = unit(0.5, "lines"))   + labs(x = "", y = "Never Smoker(%)")+theme(legend.position = "none") #

p1 <- p + scale_color_npg(alpha = 0.8)

pdf("all-study-Never Smoker.pdf", width = 30, height = 10)
print(p1)
dev.off()

