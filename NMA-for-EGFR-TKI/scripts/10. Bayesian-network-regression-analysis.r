# file name: 10. Bayesian-network-regression-analysis.r
# Function: Bayesian network meta-regression analyses (regression dependent on continuous and binary variables) were performed on the primary and secondary outcomes, effect values from regression analyses were extracted to construct tables, SUCRA values from binary regression analyses were extracted, heat maps were plotted, and multivariate correlation analyses were performed to compare differences between groups.
# Input: input/Bayesian regression analysis-continuity data.xlsx, input/Bayesian regression analysis-primary.xlsx
# output: outcomes/Regression
# Category: input_settings, Bayesian network meta-regression analyses, table_generation, plotting
# Date: 2025-04-24
# Author: Ma zidong


# Load necessary libraries
library(here)
library(readxl)
library(openxlsx)
library(dplyr)
library(gemtc)
library(igraph)
library(netmeta)
library(ComplexHeatmap)
library(circlize)
library(GetoptLong)
library(GGally)
library(tidyverse)

#### input_settings ####

# Set the main output directory
main_output_dir <- here("outcomes", "Regression")
if (!dir.exists(main_output_dir)) dir.create(main_output_dir)

# The path to the data folder

data_path = here("input",'Bayesian regression analysis-primary.xlsx')
file_path = here("input", 'Bayesian regression analysis-continuity data.xlsx')

sheet_name = excel_sheets(data_path)
sheet_name

data <- read_excel(file_path, sheet = 'Sheet1')
colnames(data)
data <- data %>%
  mutate(
    median_follow_up = as.numeric(ifelse(median_follow_up == "NR", NA, median_follow_up)),
    Asian = as.numeric(ifelse(Asian == "NR", NA, Asian)),
    Never_Smoker = as.numeric(ifelse(Never_Smoker == "NR", NA, Never_Smoker)),
    Female = as.numeric(ifelse(Female == "NR", NA, Female)),
    Median_Age = as.numeric(ifelse(Median_Age == "NR", NA, Median_Age)),
    Line = as.numeric(ifelse(Line == "NR", NA, Line))
  )


data1 <- data %>%
  mutate(
    follow_up_group = case_when(
      is.na(median_follow_up) ~ NA_real_,
      median_follow_up >= 24 ~ 1,
      median_follow_up < 24 ~ 0
    ),
    Region_group = case_when(
      is.na(Asian) ~ NA_real_,
      Asian == 0 ~ 1,#no Asian
      Asian >= 99 ~ 2,#Asian
      Asian > 0 & Asian < 99 ~ 0#Mixed
    ),
    Smoker_group = case_when(
      is.na(Never_Smoker) ~ NA_real_,
      Never_Smoker > 50 ~ 1,#Never-smoker dominant
      Never_Smoker <= 50 ~ 0#Smoker dominant
    )
  )


wb <- createWorkbook()

for (name in sheet_name) {
  df <- read_excel(data_path, sheet = name)
  merged_df <- merge(data1, df, by = c("study", "treatment"))
  addWorksheet(wb, name)
  writeData(wb, name, merged_df)
}

saveWorkbook(wb, file.path(main_output_dir, "Merged_bayesian_network_regression-data.xlsx"), overwrite = TRUE)

colnames(merged_df)
options(mc.cores = 1)  # Prevent multi-threaded conflicts


#### Bayesian network meta-regression analyses ####

Binary_list <- c("follow_up_group","Region_group","Smoker_group","Line")
Continuous_list <- c("Asian", "Never_Smoker", "Median_Age", "Female", "median_follow_up")# If there is no median age, use the mean year age instead

outcomes <- c("Serious Cardiac",  "Total Cardiac",    "Serious Vascular", "Total Vascular")
data_path = file.path(main_output_dir,'Merged_bayesian_network_regression-data.xlsx')

# Binary Bayesian network meta-regression analyses
binary_output_dir <- file.path(main_output_dir, 'binary')
if (!dir.exists(binary_output_dir)) dir.create(binary_output_dir)

output_file = file.path(binary_output_dir, 'Bayesian network meta-regression analyses-Binary.xlsx')
for (group in Binary_list){
      for (j in outcomes){
            timestamp <- Sys.time()
            data <- read_excel(data_path, sheet = j)
            results_path <- file.path(main_output_dir, paste0(group, "_", j, "_regression.txt"))
            if (file.exists(results_path)) {
                cat("[", j, "] Completed, skip \n")
                next  # Enter the next loop and skip the current one
              }
            data_1 <- as.data.frame(data)
            data_2 <- subset(data_1, !is.na(data_1[[group]]))
            print(paste0(group, ": ", j,"[", timestamp, "]"))
            data1 <- data_2[, c("study","treatment", "sampleSize", "responders")]
            study <- data_2[, c("study", group)]
            pw = pairwise(treat = treatment, n = sampleSize, event = responders, studlab = study, data = data1, sm = "OR")
            edges <- pw %>% select(treat1, treat2) %>% distinct()
            g <- graph_from_data_frame(edges, directed = FALSE)
            membership <- components(g)$membership
            main_subnet <- which.max(components(g)$csize)
            main_treats <- names(membership[membership == main_subnet])
            main_study <- pw %>%
              filter(treat1 %in% main_treats & treat2 %in% main_treats) %>%
              pull(studlab) %>% unique()
            data_main <- data1 %>% filter(study %in% main_study)
            study_main <- study %>% filter(study %in% main_study)
            networkreg <- mtc.network(data=data_main, studies=study_main)
            model <- mtc.model(networkreg, type="regression",  
                              regressor=list(coefficient='unrelated',
                                          variable = group,
                                          control ='Placebo'))
            results <- mtc.run(model, n.adapt = 100000, n.iter = 200000, thin = 10)
            
            results_path <- file.path(binary_output_dir, paste0(group, "_", j, "_regression.txt"))
            sink(results_path)
            print(summary(results))
            print(gelman.diag(results))
            sink()

            test <- summary(results)
            test1 <- test$summaries
            test2 <- as.data.frame(test1$statistics)
            test2_1=as.data.frame(lapply(test2,as.numeric))
            test3 <- as.data.frame(test1$quantiles)
            test4 <- as.data.frame(test$DIC)
            test2_1$id <- rownames(test2)
            test3$id <- rownames(test3)
            test4$id <- rownames(test4)
            Z <- 1.96
            test2_1$OR <- exp(test2_1$Mean)
            test2_1$CI_Lower <- exp(test2_1$Mean - Z * test2_1$Time.series.SE)
            test2_1$CI_Upper <- exp(test2_1$Mean + Z * test2_1$Time.series.SE)
            
            ranks <- rank.probability(results)
            cumrank.prob <- apply(t(ranks), 2, cumsum)
            sucra <- round(colMeans(cumrank.prob[-nrow(cumrank.prob),]),4)
            sucra1 <- as.data.frame(sucra)
            sucra1$id <- rownames(sucra1)

            gtest <- gelman.diag(results)
            gtest_psrf <- as.data.frame(gtest$psrf)
            gtest_psrf$mpsrf <- gtest$mpsrf
            gtest_psrf$id <- rownames(gtest_psrf)
            for(i in unique(networkreg$studies[,group])){
                  print(i)
                  ranks <- rank.probability(results, covariate = i, preferredDirection = 1)
                  cumrank.prob <- apply(t(ranks), 2, cumsum)
                  sucra <- round(colMeans(cumrank.prob[-nrow(cumrank.prob),]),4)
                  write.csv(sucra,file.path(binary_output_dir,paste0(group, "_", j, "_", i, ".csv")))
                  print(sucra)
                  print(paste0(group, "_", j, "_", i, ".csv"))
                  }
            data_list <- list("statistics" = test2_1, "quantiles" = test3, "DIC" = test4, "sucra_all" = sucra1, "psrf" = gtest_psrf)
            
            if (file.exists(output_file)) {
                  wb <- loadWorkbook(output_file)  
            } else {
                  wb <- createWorkbook()  
            }

            sheet_name <- paste0(group,j)
            sheet_name <- gsub(" ", "", sheet_name)  
            sheet_name <- gsub("group", "", sheet_name)

            # If the sheet already exists, the last line of data is retrieved and the writing is continued; if it does not exist, a new sheet is created
            if (!sheet_name %in% names(wb)) {
                  addWorksheet(wb, sheet_name)  
                  row_start <- 1  # A new sheet starts with the first line
            } else {
                  row_start <- nrow(read.xlsx(wb, sheet = sheet_name)) + 2  # Find the last line of the sheet and two lines are freed
            }

            for (name in names(data_list)) {
                  print(name)
                  df <- data_list[[name]]
                  # Write title
                  #
                  writeData(wb, sheet_name, paste("###", name, "###"), startRow = row_start, colNames = FALSE)
                  row_start <- row_start + 1    
                  # write DataFrame
                  writeData(wb, sheet_name, df, startRow = row_start)
                  row_start <- row_start + nrow(df) + 2  # The two empty lines are separated
            }
            saveWorkbook(wb, output_file, overwrite = TRUE)

            cat("Data has been saved to: ", output_file, "\n")
      }
}


# Continous Bayesian network meta-regression analyses

continous_output_dir <- file.path(main_output_dir, 'continous')
if (!dir.exists(continous_output_dir)) dir.create(continous_output_dir)

output_file = file.path(continous_output_dir, 'Bayesian network meta-regression analyses-Continous.xlsx')


for (group in Continuous_list){
      for (j in outcomes){
            timestamp <- Sys.time()
            data <- read_excel(data_path, sheet = j)
            results_path <- file.path(main_output_dir, paste0(group, "_", j, "_regression.txt"))
            if (file.exists(results_path)) {
                cat("[", j, "] Completed, skip \n")
                next  # Enter the next loop and skip the current one
              }
            data_1 <- as.data.frame(data)
            data_2 <- subset(data_1, !is.na(data_1[[group]]))
            print(paste0(group, ": ", j,"[", timestamp, "]"))
            data1 <- data_2[, c("study","treatment", "sampleSize", "responders")]
            study <- data_2[, c("study", group)]            
            pw = pairwise(treat = treatment, n = sampleSize, event = responders, studlab = study, data = data1, sm = "OR")
            edges <- pw %>% select(treat1, treat2) %>% distinct()
            g <- graph_from_data_frame(edges, directed = FALSE)
            membership <- components(g)$membership
            main_subnet <- which.max(components(g)$csize)
            main_treats <- names(membership[membership == main_subnet])
            main_study <- pw %>%
              filter(treat1 %in% main_treats & treat2 %in% main_treats) %>%
              pull(studlab) %>% unique()
            data_main <- data1 %>% filter(study %in% main_study)
            study_main <- study %>% filter(study %in% main_study)
            networkreg <- mtc.network(data=data_main, studies=study_main)
            model <- mtc.model(networkreg, type="regression", 
                              regressor=list(coefficient='unrelated',
                                          variable = group,
                                          control='Placebo'))
            results <- mtc.run(model, n.adapt = 100000, n.iter = 200000, thin = 10)
            
            results_path <- file.path(continous_output_dir, paste0(group, "_", j, "_regression.txt"))
            sink(results_path)
            print(summary(results))
            print(gelman.diag(results))
            sink()

            test <- summary(results)
            test1 <- test$summaries
            test2 <- as.data.frame(test1$statistics)
            test2_1=as.data.frame(lapply(test2,as.numeric))
            test3 <- as.data.frame(test1$quantiles)
            test4 <- as.data.frame(test$DIC)
            test2_1$id <- rownames(test2)
            test3$id <- rownames(test3)
            test4$id <- rownames(test4)
            Z <- 1.96
            test2_1$OR <- exp(test2_1$Mean)
            test2_1$CI_Lower <- exp(test2_1$Mean - Z * test2_1$Time.series.SE)
            test2_1$CI_Upper <- exp(test2_1$Mean + Z * test2_1$Time.series.SE)
            
            ranks <- rank.probability(results)
            cumrank.prob <- apply(t(ranks), 2, cumsum)
            sucra <- round(colMeans(cumrank.prob[-nrow(cumrank.prob),]),4)
            sucra1 <- as.data.frame(sucra)
            sucra1$id <- rownames(sucra1)

            gtest <- gelman.diag(results)
            gtest_psrf <- as.data.frame(gtest$psrf)
            gtest_psrf$mpsrf <- gtest$mpsrf
            gtest_psrf$id <- rownames(gtest_psrf)

            data_list <- list("statistics" = test2_1, "quantiles" = test3, "DIC" = test4, "sucra_all" = sucra1, "psrf" = gtest_psrf)
            if (file.exists(output_file)) {
                  wb <- loadWorkbook(output_file)  
            } else {
                  wb <- createWorkbook()  
            }

            sheet_name <- paste0(group,j)
            sheet_name <- gsub(" ", "", sheet_name)  
            sheet_name <- gsub("group", "", sheet_name)

            if (!sheet_name %in% names(wb)) {
                  addWorksheet(wb, sheet_name)  
                  row_start <- 1  
            } else {
                  row_start <- nrow(read.xlsx(wb, sheet = sheet_name)) + 2  
            }

            for (name in names(data_list)) {
                  print(name)
                  df <- data_list[[name]]
                  
                  writeData(wb, sheet_name, paste("###", name, "###"), startRow = row_start, colNames = FALSE)
                  row_start <- row_start + 1    
                  writeData(wb, sheet_name, df, startRow = row_start)
                  row_start <- row_start + nrow(df) + 2  
            }
            saveWorkbook(wb, output_file, overwrite = TRUE)
            cat("Data has been saved to: ", output_file, "\n")

      }
}


#### table_generation ####

list.files(path = binary_output_dir, pattern = '*\\_.*\\.csv')

line_labels <- c("non-priority therapy", "priority therapy")#0 non-priority therapy, 1 priority therapy
smoker_labels <- c("Smoker", "Non-smoker")
region_labels <- c("Mixed", "Non-Asian", "Asian")
follow_up_labels <- c("<2 years", ">=2 years")


# Summary list
label_dict <- list(
  Line = line_labels,
  Smoker_group = smoker_labels,
  Region_group = region_labels,
  follow_up_group = follow_up_labels
)



# List all relevant csv files
files <- list.files(path = binary_output_dir, pattern = "*_.*\\.csv")


# Define the outcome regular pattern
outcome_pattern <- "(Serious|Total) (Cardiac|Vascular)"

file_info <- tibble(filename = files) %>%
  mutate(
    outcome = str_extract(filename, outcome_pattern),
    subgroup = str_remove(filename, paste0("_", outcome, ".*")),
    index = str_extract(filename, "\\d+(?=\\.csv$)")
  )

print(head(file_info))
print(table(file_info$subgroup))

file_info <- file_info %>%
  mutate(index = as.integer(index))

# Add a tag column
file_info <- file_info %>%
  rowwise() %>%
  mutate(
    label = ifelse(subgroup %in% names(label_dict),
                   label_dict[[subgroup]][index + 1],
                   NA_character_)
  ) %>%
  ungroup()
#Use index + 1 because the file name is _0.csv, _1.csv, and the vector index of R starts from 1.

print(file_info, n = nrow(file_info))


outcomes <- unique(file_info$outcome)

wb <- createWorkbook()

for (outcome_val in outcomes) {
  cat("Processing:", outcome_val, "\n")
  files_to_read <- file_info %>% filter(outcome == outcome_val)
  print(files_to_read$filename)
  test <- data.frame(group = character())
  for (i in seq_len(nrow(files_to_read))) {
    row <- files_to_read[i, ]
    data <- read.csv(file.path(binary_output_dir, row$filename))
    # Name the data frame, for example, comment label
    cat("Subgroup: ", row$subgroup, "| Label:", row$label, "\n")
    colnames(data) <- c('group', row$label)
    test <- full_join(test, data, by = "group")
  }
  addWorksheet(wb, outcome_val)
  writeData(wb, outcome_val, test)
  message("Done:", outcome_val, "\n")
}  

saveWorkbook(wb, file.path(binary_output_dir,"primary outcomes-sucra-regression-binary.xlsx"), overwrite = TRUE)




#### plotting ####

file <- here("input", "primary outcomes-sucra-regression-binary.xlsx")# Use a version standardized for the therapy name for drawing
sheet_name <- excel_sheets(file)
col_fun = colorRamp2(c(-3, 0, 3), c("#2A6EBBFF",  "white", "#CD202CFF"))#BMJ
my_ggally_cor <- function(data,
                          mapping,
                          ...) {
  args <- list(...)
  method <- args$method %||% "pearson"
  use <- args$use %||% "pairwise.complete.obs"
  digits <- args$digits %||% 2
  ggally_statistic(
    data,
    mapping,
    title = NULL,
    sep = NULL,
    text_fn = function(x, y) {
      corObj <- stats::cor.test(x, y,
        method = method,
        use = use
      )
      cor_est <- as.numeric(corObj$estimate)
      p_est <- as.numeric(corObj$p.value)

      cor_txt <- formatC(cor_est, digits = digits, format = "f")
      p_txt <- formatC(p_est, digits = digits, format = "f")
      paste0("r = ", cor_txt, "\n", "p = ", p_txt)
    }
  )
}


for (sheet in sheet_name){
  data <- read_excel(file, sheet = sheet)
  data <- as.data.frame(data)
  rownames(data) <- data[,1]
  data <- data[,-1]
  data1 <- scale(data)
  data1[is.na(data1)] <- 0
  df_clean <- data1[, colSums(is.na(data1)) < nrow(data1)]  
  df_clean <- df_clean[complete.cases(df_clean), ] 
  
  p = Heatmap(data1, col = col_fun, name = "SUCRA", 
             border_gp = gpar(col = "black"), rect_gp = gpar(col = "dimgray", lwd = 0.7), 
             show_row_names = TRUE, row_names_gp = gpar(fontsize = 10), 
             cluster_columns = F, cluster_rows = T, column_names_rot = 45, 
             column_names_gp = gpar(fontsize = 10), heatmap_width = unit(16, "cm"), 
             heatmap_height = unit(16, "cm"), show_heatmap_legend = FALSE,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%.2f", data[i, j]), x, y, gp = gpar(fontsize = 8))}, row_title = NULL)
  file_name = file.path(main_output_dir, paste0('primary outcomes-sucra-regression-binary-heatmap_', sheet, ".pdf"))
  pdf(file_name, width = 7.5)
  plot(p)
  dev.off()
  file_name1 = file.path(main_output_dir, paste0('primary outcomes-sucra-regression-binary-ggpairs_', sheet, ".pdf"))
  p1 <- ggpairs(
  data = df_clean,
  upper = list(continuous = wrap(
    my_ggally_cor,
    method = "pearson", digits = 2,
    size = 6, color = "black"
  )),
  lower = list(continuous = wrap("smooth", color = "firebrick")),
  diag = list(continuous = wrap("densityDiag", fill = "slategray3"))
)
  pdf(file_name1, width = 12, height = 10)
  print(p1)
  dev.off()
}


