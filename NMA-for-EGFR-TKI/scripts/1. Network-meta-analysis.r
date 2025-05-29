# file name: 1. Network-meta-analysis.r
# Function: Perform network meta-analysis on each sheet and output graphs and tables
# Input: input/network_data.xlsx, input/network_data_rare_events.xlsx
# output: outcomes/network-meta/main/ and outcomes/network-meta/supp/
# Category: input_settings, network_meta_analysis, table_generation, CINeMA rating
# Date: 2025-04-24
# Author: Ma zidong



# Load necessary libraries
library(here)
library(openxlsx)
library(netmeta)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)


#### input_settings ####

# Set the main output directory
main_output_dir <- here("outcomes", "network-meta")

if (!dir.exists(main_output_dir)) dir.create(main_output_dir)

# Custom functions: Ensure the path exists
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# Custom functions: Breakpoint continuous transmission
skip_if_exists <- function(file, code_block) {
  if (!file.exists(file)) {
    code_block()
  } else {
    message("File already exists, skipping:", file)
  }
}

# The path to the data folder
excel_path_default <- here("input", "network_data.xlsx")# Original data containing the main figures 2, 3, and 4 and supplementary materials for network meta-analysis
excel_path_rare <- here("input", "network_data_rare_events.xlsx")# Original data for the sensitivity analysis of rare event in supplementary materials


excel_path_default[is.na(excel_path_default)] <- 0
excel_path_rare[is.na(excel_path_rare)] <- 0

# Get all sheet names
sheet_names_default <- excel_sheets(excel_path_default)
sheet_names_rare <- excel_sheets(excel_path_rare)

# sheet for main figures
main_sheets <- c(
  "Serious Cardiac", "Serious Cardiac (Gen)", "Total Cardiac", "Total Cardiac (Gen)",
  "Serious Vascular", "Serious Vascular (Gen)", "Total Vascular", "Total Vascular (Gen)",#The above are the data used in main figures 2 and 3
  "Serious Arrhythmias", "Serious Arrhythmias (Gen)",  "Total Arrhythmias", "Total Arrhythmias (Gen)", 
  "Serious Heart Failure", "Serious Heart Failure (Gen)", "Total Heart Failure", "Total Heart Failure (Gen)", 
  "Serious Vascular Toxicity", "Serious Vascular Toxicity (Gen)", "Total Vascular Toxicity", "Total Vascular Toxicity (Gen)", 
  "Serious Hypertension", "Serious Hypertension (Gen)", "Total Hypertension", "Total Hypertension (Gen)"#The above is the data used in the main figure 4
  )

supp_sheets <- c(
  "cardiac serious (dose)-gen", "cardiac total (dose)-gen", "vascular serious (dose)-gen", "vascular total (dose)-gen",
  "cardiac serious (dose)-1", "cardiac total (dose)-1", "vascular serious (dose)-1", "vascular total (dose)-1", #Data used for multi-dose group (low dose)
  "cardiac serious (dose)-1gen", "cardiac total (dose)-1gen", "vascular serious (dose)-1gen", "vascular total (dose)-1gen", #Data used for multi-dose group (low dose)
  "cardiac serious (dose)-2", "cardiac total (dose)-2", "vascular serious (dose)-2", "vascular total (dose)-2", #Data used for multi-dose group (multiple doses)
  "cardiac serious (dose)-2gen", "cardiac total (dose)-2gen", "vascular serious (dose)-2gen", "vascular total (dose)-2gen", #Data used for multi-dose group (multiple doses)
  "cardiac serious (dose)", "cardiac total (dose)", "vascular serious (dose)","vascular total (dose)",#Data used for multi-dose group (high dose)
  "Total Hypertension (funnel)", "Total Vascular (funnel)",#The above are the data used for retention of high quality studies
  "vascular serious (antiangio)", "vascular serious (MET)", "vascular serious (IGF-1R)", 
  "vascular total (antiangio)", "vascular total (MET)", "vascular total (IGF-1R)",
  "cardiac serious (antiangio)", "cardiac serious (MET)", "cardiac serious (IGF-1R)",       
  "cardiac total (antiangio)", "cardiac total (MET)",  "cardiac total (IGF-1R)",#The above are the data used for control subgroup         
  "cardiac serious (0)-gen",  "cardiac total (0)-gen", "vascular serious (0)-gen", "vascular total (0)-gen",
  "cardiac serious (0)", "cardiac total (0)", "vascular serious (0)", "vascular total (0)",#The above are the data used for rare events 
  "Serious Cardiac (G-E)", "Serious Cardiac (G-E)-Gen", "Total Cardiac (G-E)", "Total Cardiac (G-E)-Gen",
  "Serious Vascular (G-E)", "Serious Vascular (G-E)-Gen", "Total Vascular (G-E)", "Total Vascular (G-E)-Gen"#The above are the data used for sensitivity analysis of FLAURA and EVEREST studies
)


input_settings_default <- data.frame(
  sheet_name = sheet_names_default,
  label = sheet_names_default,  
  type = ifelse(sheet_names_default %in% main_sheets, "main", "supp")
)

input_settings_rare <- data.frame(
  sheet_name = sheet_names_rare,
  label = sheet_names_rare,  
  type = ifelse(sheet_names_rare %in% main_sheets, "main", "supp")
)
# Set the color scheme (according to the sheet name)

color_list <- c("cardiac serious (0)" = "steelblue", 
                "cardiac total (0)" = "steelblue",
                "vascular serious (0)" = "steelblue",
                "vascular total (0)" = "steelblue",
                "cardiac serious (0)-gen"= "steelblue", 
                "cardiac total (0)-gen"= "steelblue", 
                "vascular serious (0)-gen"= "steelblue", 
                "vascular total (0)-gen"= "steelblue",
                "cardiac serious (dose)" = "#8B0000",
                "cardiac serious (dose)-gen"= "#8B0000", 
                "cardiac total (dose)-gen"= "#8B0000", 
                "vascular serious (dose)-gen"= "#8B0000", 
                "vascular total (dose)-gen"= "#8B0000",
                "cardiac total (dose)" = "#8B0000",
                "vascular serious (dose)" = "#8B0000",
                "vascular total (dose)" = "#8B0000",
                "cardiac serious (dose)-1"= "#8B0000", 
                "cardiac total (dose)-1"= "#8B0000", 
                "vascular serious (dose)-1"= "#8B0000", 
                "vascular total (dose)-1"= "#8B0000",
                "cardiac serious (dose)-2"= "#8B0000", 
                "cardiac total (dose)-2"= "#8B0000", 
                "vascular serious (dose)-2"= "#8B0000", 
                "vascular total (dose)-2"= "#8B0000",
                "cardiac serious (dose)-1gen"= "#8B0000", 
                "cardiac total (dose)-1gen"= "#8B0000", 
                "vascular serious (dose)-1gen"= "#8B0000", 
                "vascular total (dose)-1gen"= "#8B0000",
                "cardiac serious (dose)-2gen"= "#8B0000", 
                "cardiac total (dose)-2gen"= "#8B0000",
                "vascular serious (dose)-2gen"= "#8B0000",
                "vascular total (dose)-2gen"= "#8B0000",
                "cardiac serious (antiangio)" = "darkgreen",
                "cardiac serious (MET)" = "darkgreen",
                "cardiac serious (IGF-1R)" = "darkgreen",
                "cardiac total (antiangio)" = "darkgreen",
                "cardiac total (MET)" = "darkgreen",
                "cardiac total (IGF-1R)" = "darkgreen",
                "vascular serious (antiangio)" = "darkgreen",
                "vascular serious (MET)" = "darkgreen",
                "vascular serious (IGF-1R)" = "darkgreen",
                "vascular total (antiangio)" = "darkgreen",
                "vascular total (MET)" = "darkgreen",
                "vascular total (IGF-1R)" = "darkgreen",
                "Serious Cardiac" = "#7D5CC6FF",
                "Serious Cardiac (Gen)" = "#2A6EBBFF",
                "Total Cardiac" = "#7D5CC6FF",
                "Total Cardiac (Gen)" = "#2A6EBBFF",
                "Serious Vascular" = "#7D5CC6FF",
                "Serious Vascular (Gen)" = "#2A6EBBFF",
                "Total Vascular" = "#7D5CC6FF",
                "Total Vascular (Gen)" = "#2A6EBBFF",
                "Serious Arrhythmias" = "#00B2A9FF",
                "Serious Arrhythmias (Gen)" = "#5C8286",
                "Total Arrhythmias" = "#00B2A9FF",
                "Total Arrhythmias (Gen)" = "#5C8286",
                "Serious Heart Failure" = "#00B2A9FF",
                "Serious Heart Failure (Gen)" = "#5C8286",
                "Total Heart Failure" = "#00B2A9FF",
                "Total Heart Failure (Gen)" = "#5C8286",
                "Serious Vascular Toxicity" = "#00B2A9FF",
                "Serious Vascular Toxicity (Gen)" = "#5C8286",
                "Total Vascular Toxicity" = "#00B2A9FF",
                "Total Vascular Toxicity (Gen)" = "#5C8286",
                "Serious Hypertension" = "#00B2A9FF",
                "Serious Hypertension (Gen)" = "#5C8286",
                "Total Hypertension" = "#00B2A9FF",
                "Total Hypertension (Gen)" = "#5C8286",
                "Total Hypertension (funnel)" = "#00B2A9FF",
                "Total Vascular (funnel)" = "#7D5CC6FF",
                "Serious Cardiac (G-E)" = "steelblue",
                "Total Cardiac (G-E)" = "steelblue",
                "Serious Vascular (G-E)" = "steelblue",
                "Total Vascular (G-E)" = "steelblue",
                "Serious Cardiac (G-E)-Gen" = "steelblue",
                "Total Cardiac (G-E)-Gen" = "steelblue",
                "Serious Vascular (G-E)-Gen" = "steelblue",
                "Total Vascular (G-E)-Gen" = "steelblue"
                ) 


#### network_meta_analysis ####
run_network_analysis <- function(excel_path, sheet_names, input_settings, main_output_dir, method = "default") {
  for (sheet in sheet_names) {
    message("Sheet: ", sheet)

    info <- input_settings[input_settings$sheet_name == sheet, ]
    #print(info)
    label <- info$label
    type <- info$type
    out_dir <- file.path(main_output_dir, type, label)
    ensure_dir(out_dir)

    # Read data
    df <- read_excel(excel_path, sheet = sheet)

    # network meta-analysis
    if (method == "default") {
      pw <- pairwise(treat = alloc1, n = sampleSize, event = responders,
                     studlab = study, data = df, sm = "OR", incr = 0.5, allincr = TRUE, addincr = TRUE)
      net <- netmeta(pw, ref = "Placebo", common = FALSE)
    } else if (method == "inverse") {
      pw <- pairwise(treat = alloc1, n = sampleSize, event = responders, 
                      incr = 0.5, allincr = TRUE, addincr = TRUE, allstudies = TRUE, studlab = study, data = df, sm = "OR")
      net <- netmetabin(pw, ref = "Placebo", method = "Inverse", details.chkmultiarm = FALSE)
    }


  net_path <- file.path(out_dir, paste0(sheet, "_netmeta.txt"))
  sink(net_path)
  print(net)
  sink()
  
  
  # 1. netgraph
  netgraph_file <- file.path(out_dir, paste0(sheet, "_netgraph.pdf"))
  theme_color <- color_list[[sheet]]
  
  
  skip_if_exists(netgraph_file, function() {
    pdf(netgraph_file, width = 9)
    # Determine whether there is a multi-arm trial
    multiarm_flag <- any(net$multiarm)
    col_multiarm <- if (multiarm_flag) theme_color else NULL
    
    netgraph(net, seq = "optimal",
             plastic = FALSE, 
             cex.points = net$n.trts,
             offset = ifelse(n.trts < 1500, 0.025, 0.05),
             labels = trts,
             cex = 0.7,
             scale = 1.15,
             multiarm = multiarm_flag,
             col.multiarm = col_multiarm,
             col.points = theme_color,
             alpha.transparency = 0.8,
             col = "black",
             thickness = "number.of.studies",
             lwd.min = 1, lwd.max = 10
    )
    dev.off()
  })

  # 1-1. netgraph
  netgraph_file1 <- file.path(out_dir, paste0(sheet, "_netgraph-1.pdf"))
  theme_color <- color_list[[sheet]]
  
  
  skip_if_exists(netgraph_file1, function() {
    pdf(netgraph_file1, width = 9)
    # Determine whether there is a multi-arm trial
    multiarm_flag <- any(net$multiarm)
    col_multiarm <- if (multiarm_flag) theme_color else NULL
    
    netgraph(net, seq = "optimal",
             plastic = FALSE, 
             cex.points = net$n.trts,
             offset = ifelse(n.trts < 1500, 0.025, 0.05),
             labels = trts,
             cex = 0.7,
             scale = 1.15,
             multiarm = multiarm_flag,
             col.multiarm = col_multiarm,
             col.points = theme_color,
             alpha.transparency = 0.8,
             col = "black",
             thickness = "number.of.studies",
             lwd.min = 1, lwd.max = 10,
             number = TRUE,
             #col.number.of.studies = "black",
             #bg.number.of.studies = "black",
             pos.number.of.studies = 0.5,
             cex.number.of.studies = 0.7
    )
    dev.off()
  })

  # 1-2. netgraph
  netgraph_file2 <- file.path(out_dir, paste0(sheet, "_netgraph-2.pdf"))
  theme_color <- color_list[[sheet]]
  
  
  skip_if_exists(netgraph_file2, function() {
    pdf(netgraph_file2, width = 9)
    # Determine whether there is a multi-arm trial
    multiarm_flag <- any(net$multiarm)
    col_multiarm <- if (multiarm_flag) theme_color else NULL
    
    netgraph(net, seq = "optimal",
             plastic = FALSE, 
             cex.points = net$n.trts,
             offset = ifelse(n.trts < 1500, 0.025, 0.05),
             labels = paste0(trts,"\n(n=", round(n.trts), ")"),
             cex = 0.7,
             scale = 1.15,
             multiarm = multiarm_flag,
             col.multiarm = col_multiarm,
             col.points = theme_color,
             alpha.transparency = 0.8,
             col = "black",
             thickness = "number.of.studies",
             lwd.min = 1, lwd.max = 10,
             number = TRUE,
             #col.number.of.studies = "black",
             #bg.number.of.studies = "black",
             pos.number.of.studies = 0.5,
             cex.number.of.studies = 0.7
    )
    dev.off()
  })

  # 2. Forest plot
  forest_file <- file.path(out_dir, paste0(sheet, "_forest.pdf"))
  skip_if_exists(forest_file, function() {
    pdf(forest_file, width = 9, height = 10)
    forest(net,
           reference.group = "Placebo",
           smlab = paste0(sheet, " Adverse Drug Reactions\n(Comparison: other vs Placebo)"),
           drop.reference.group = TRUE,
           label.left = "Favours Control",
           col.square = color_list[[sheet]],
           drop = TRUE,
           sortvar = -TE,
           label.right = "Favours Experimental")
    dev.off()
  })

  # 2-1. Forest plot
  forest_file1 <- file.path(out_dir, paste0(sheet, "_forest-1.pdf"))
  skip_if_exists(forest_file1, function() {
    pdf(forest_file1, width = 12, height = 10)
    forest(net,
           reference.group = "Placebo",
           smlab = paste0(sheet, " Adverse Drug Reactions\n(Comparison: other vs Placebo)"),
           drop.reference.group = TRUE,
           label.left = "Favours Control",
           col.square = color_list[[sheet]],
           drop = TRUE,
           label.right = "Favours Experimental",
           rightcols = c("effect", "ci"), leftcols=c("studlab", "k", "Pscore"), sortvar=Pscore)
    dev.off()
  })


  # 3. League table
  league_file <- file.path(out_dir, paste0(sheet, "_league_table_bad_sorted.csv"))
  skip_if_exists(league_file, function() {
    league_bad_sorted <- netleague(net, digits = 2, seq = netrank(net, small.values = "bad"),bracket = "(", separator = " to ")
    write.table(league_bad_sorted$random, file = league_file,row.names = FALSE, col.names = FALSE, sep = ",")
  })
  
  league_file1 <- file.path(out_dir, paste0(sheet, "_league_table_good_sorted.csv"))
  skip_if_exists(league_file1, function() {
    league_good_sorted <- netleague(net, digits = 2, seq = netrank(net, small.values = "good"),bracket = "(", separator = " to ")
    write.table(league_good_sorted$random, file = league_file1,row.names = FALSE, col.names = FALSE, sep = ",")
  })
  
  # 4.Funnel plot
  
  funnel_file <- file.path(out_dir, paste0(sheet, "_funnel.pdf"))
  skip_if_exists(funnel_file, function() {
    pdf(funnel_file)
    funnel(net, order=unique(net$treat1), pos.studlab = 4, legend = FALSE, method.bias = "Egger", col = "black", digits.pval = 2, cex.studlab = 0.7,studlab=TRUE,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1, pch = 16)
    dev.off()
  })
  f = funnel(net, order=unique(net$treat1), pos.studlab = 4, legend = FALSE, method.bias = "Egger", col = "black", digits.pval = 2, cex.studlab = 0.7,studlab=TRUE,level = 0.95, contour = c(0.9, 0.95, 0.99),col.contour = c("darkgrey", "grey", "lightgrey"),lwd = 2, cex = 1, pch = 16)
  
  out_path <- file.path(out_dir, paste0(sheet, "_funnel.txt"))
  sink(out_path)
  print(metabias(metagen(TE.adj, seTE, data = f), method = "linreg"))
  sink()
  
  # 5. node split
  netsplit_file <- file.path(out_dir, paste0(sheet, "_netsplit.pdf"))
  skip_if_exists(netsplit_file, function() {
    pdf(netsplit_file, width = 8, height = 35)
    meta::forest(netsplit(net), col.square = "#0000ff", col.square.lines = "#000000", smlab = paste0(sheet," \nAdverse Drug Reactions"),
                 col.inside = "#000000", col.diamond = "#000000", col.diamond.lines = "#000000")
    dev.off()
  })
  message("Done:", sheet, "\n")
  }
}

run_network_analysis(excel_path_rare, sheet_names_rare, input_settings_rare, main_output_dir, method = "inverse")

run_network_analysis(excel_path_default, sheet_names_default, input_settings_default, main_output_dir, method = "default")



#### table_generation ####

# Extract the intersection of each therapy and placebo, and build the outcome table (Figure3, Figure4)

folder_list <- c("cardiac serious (0)", "cardiac total (0)", 
                 "vascular serious (0)", "vascular total (0)",
                 "cardiac serious (dose)", "cardiac total (dose)", 
                 "vascular serious (dose)", "vascular total (dose)",
                 "cardiac serious (dose)-1", "cardiac total (dose)-1", 
                 "vascular serious (dose)-1", "vascular total (dose)-1",
                 "cardiac serious (dose)-2", "cardiac total (dose)-2", 
                 "vascular serious (dose)-2", "vascular total (dose)-2",
                 "cardiac serious (dose)-2gen", "cardiac total (dose)-2gen",
                 "vascular serious (dose)-2gen", "vascular total (dose)-2gen",
                 "cardiac serious (dose)-1gen", "cardiac total (dose)-1gen",
                 "vascular serious (dose)-1gen", "vascular total (dose)-1gen",
                 "cardiac serious (antiangio)", "cardiac serious (MET)", "cardiac serious (IGF-1R)", 
                 "cardiac total (antiangio)","cardiac total (MET)", "cardiac total (IGF-1R)", 
                 "vascular serious (antiangio)", "vascular serious (MET)", "vascular serious (IGF-1R)", 
                 "vascular total (antiangio)", "vascular total (MET)", "vascular total (IGF-1R)",
                 "Serious Cardiac", "Serious Cardiac (Gen)",  "Total Cardiac", "Total Cardiac (Gen)",
                 "Serious Vascular", "Serious Vascular (Gen)", "Total Vascular", "Total Vascular (Gen)", 
                 "Serious Arrhythmias", "Serious Arrhythmias (Gen)", "Total Arrhythmias", "Total Arrhythmias (Gen)", 
                 "Serious Heart Failure", "Serious Heart Failure (Gen)", "Total Heart Failure", "Total Heart Failure (Gen)",
                 "Serious Vascular Toxicity","Serious Vascular Toxicity (Gen)",
                 "Total Vascular Toxicity","Total Vascular Toxicity (Gen)",
                 "Serious Hypertension","Serious Hypertension (Gen)",
                 "Total Hypertension","Total Hypertension (Gen)",
                 "cardiac serious (0)-gen", "cardiac total (0)-gen", "vascular serious (0)-gen", "vascular total (0)-gen",
                 "cardiac serious (dose)-gen","cardiac total (dose)-gen", "vascular serious (dose)-gen",
                 "vascular total (dose)-gen","Total Hypertension (funnel)","Total Vascular (funnel)",
                 "Serious Cardiac (G-E)", "Serious Cardiac (G-E)-Gen", "Total Cardiac (G-E)", "Total Cardiac (G-E)-Gen",
                "Serious Vascular (G-E)", "Serious Vascular (G-E)-Gen", "Total Vascular (G-E)", "Total Vascular (G-E)-Gen"
)  


base_dir <- main_output_dir  

type_dirs <- c("main", "supp")

all_therapies <- c() # Initialize empty vector

for (type in type_dirs) {
  subfolders <- list.dirs(file.path(base_dir, type), full.names = TRUE, recursive = FALSE)
  
  for (subfolder in subfolders) {
    message("Processing: ", subfolder)

    files <- list.files(subfolder, pattern = "bad|good", full.names = TRUE)

    for (file in files) {
      mat <- tryCatch({
        read.csv(file, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = ".")
      }, error = function(e) return(NULL))

      if (!is.null(mat)) {
        diag_names <- diag(as.matrix(mat))
        all_therapies <- union(all_therapies, diag_names)
      }
    }
  }
}
# Remove Placebo and reference drugs (such as Gefitinib/Erlotinib)
all_therapies <- setdiff(all_therapies, c("Placebo", "Gefitinib/Erlotinib"))
#print(all_therapies)

# Store results
results_list <- list()
for (type in type_dirs) {
  subfolders <- list.dirs(file.path(base_dir, type), full.names = TRUE, recursive = FALSE)
  
  for (folder in subfolders) {
    message("Processing: ", folder)
    #print(folder)
    files <- list.files(folder, pattern = "bad|good", full.names = TRUE)
    # Initialize a temporary list to store the current folder results
    temp_result <- data.frame(
      Therapy = all_therapies,
      value = NA,
      stringsAsFactors = FALSE
    )
    # Define bad and good path variables
    bad_file <- grep("bad", files, value = TRUE)
    good_file <- grep("good", files, value = TRUE)  

    bad_mat <- tryCatch({
      read.csv(bad_file, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = ".")
    }, error = function(e) NULL)  

    good_mat <- tryCatch({
      read.csv(good_file, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = ".")
    }, error = function(e) NULL)

    if (!is.null(bad_mat) && !is.null(good_mat)) {
      rownames(bad_mat) <- colnames(bad_mat) <- diag(as.matrix(bad_mat))
      rownames(good_mat) <- colnames(good_mat) <- diag(as.matrix(good_mat))
      # Extract data
      for (therapy in temp_result$Therapy) {
        # Get the rank index of therapy and placebo
        #print(therapy)
        if (therapy %in% rownames(bad_mat)){
          therapy_row <- which(rownames(bad_mat) == therapy)
          placebo_row <- which(rownames(bad_mat) == "Placebo")
          if (length(placebo_row) == 0 || length(therapy_row) == 0) next
          # Extract the value
          if (therapy_row < placebo_row) {
            # therapy above placebo, extracted from the lower triangle of the serious_bad table
            temp_result[temp_result$Therapy == therapy, "value"] <- bad_mat[placebo_row, therapy_row]
          } else {
            # therapy under placebo, extracted from the lower triangle of the serious_good table
            therapy_row1 <- which(rownames(good_mat) == therapy)
            placebo_row1 <- which(rownames(good_mat) == "Placebo") 
            temp_result[temp_result$Therapy == therapy, "value"] <- good_mat[placebo_row1, therapy_row1]
          }
        }
      }
    }
    # Add the current folder results to the total list
    folder_name <- basename(folder)
    colnames(temp_result) <- c("Therapy", folder_name)
    results_list[[folder_name]] <- temp_result
  }
}
# Initialize the final result table
final_result <- data.frame(Therapy = all_therapies, stringsAsFactors = FALSE)

# Append columns one by one folder
for (folder in folder_list) {
  temp_result <- results_list[[folder]]
  # Extract only the value column (make sure it is consistent with Therapy)
  final_result[[folder]] <- temp_result[match(final_result$Therapy, temp_result$Therapy), folder]
}

# View the final result
head(final_result)
write.csv(final_result, file.path(main_output_dir, "final_results_network_meta_outcomes.csv"))



# Extract and build serious, total league tables

key_list <- c("cardiac serious (0)", "cardiac total (0)", 
              "vascular serious (0)", "vascular total (0)",
              "cardiac serious (dose)", "cardiac total (dose)", 
              "vascular serious (dose)", "vascular total (dose)",
              "cardiac serious (dose)-1", "cardiac total (dose)-1", 
              "vascular serious (dose)-1", "vascular total (dose)-1",
              "cardiac serious (dose)-2", "cardiac total (dose)-2", 
              "vascular serious (dose)-2", "vascular total (dose)-2",
              "cardiac serious (dose)-2gen", "cardiac total (dose)-2gen",
              "vascular serious (dose)-2gen", "vascular total (dose)-2gen",
              "cardiac serious (dose)-1gen", "cardiac total (dose)-1gen",
              "vascular serious (dose)-1gen", "vascular total (dose)-1gen",
              "cardiac serious (antiangio)", "cardiac total (antiangio)",
              "cardiac serious (MET)", "cardiac total (MET)",
              "cardiac serious (IGF-1R)",  "cardiac total (IGF-1R)", 
              "vascular serious (antiangio)", "vascular total (antiangio)", 
              "vascular serious (MET)", "vascular total (MET)",
              "vascular serious (IGF-1R)",  "vascular total (IGF-1R)",
              "Serious Cardiac", "Total Cardiac", 
              "Serious Cardiac (Gen)",  "Total Cardiac (Gen)",
              "Serious Vascular", "Total Vascular",
              "Serious Vascular (Gen)",  "Total Vascular (Gen)", 
              "Serious Arrhythmias", "Total Arrhythmias",
              "Serious Arrhythmias (Gen)",  "Total Arrhythmias (Gen)", 
              "Serious Heart Failure", "Total Heart Failure",
              "Serious Heart Failure (Gen)",  "Total Heart Failure (Gen)",
              "Serious Vascular Toxicity","Total Vascular Toxicity",
              "Serious Vascular Toxicity (Gen)", "Total Vascular Toxicity (Gen)",
              "Serious Hypertension", "Total Hypertension",
              "Serious Hypertension (Gen)", "Total Hypertension (Gen)",
              "Serious Vascular","Total Vascular (funnel)",
              "Serious Hypertension" , "Total Hypertension (funnel)",
              "cardiac serious (0)-gen", "cardiac total (0)-gen", 
              "vascular serious (0)-gen", "vascular total (0)-gen",
              "cardiac serious (dose)-gen", "cardiac total (dose)-gen", 
              "vascular serious (dose)-gen", "vascular total (dose)-gen",
              "Serious Cardiac (G-E)", "Total Cardiac (G-E)",
              "Serious Vascular (G-E)", "Total Vascular (G-E)",
              "Serious Cardiac (G-E)-Gen",  "Total Cardiac (G-E)-Gen",
              "Serious Vascular (G-E)-Gen",  "Total Vascular (G-E)-Gen"
              
)  


keys <- c("cardiac (0)",
          "vascular (0)",
          "cardiac (dose)",
          "vascular (dose)",
          "cardiac (dose)-1",
          "vascular (dose)-1",
          "cardiac (dose)-2",
          "vascular (dose)-2",
          "cardiac (dose)-2gen",
          "vascular (dose)-2gen",
          "cardiac (dose)-1gen",
          "vascular (dose)-1gen",
          "cardiac (antiangio)",
          "cardiac (MET)",
          "cardiac (IGF-1R)",
          "vascular (antiangio)",
          "vascular (MET)",
          "vascular (IGF-1R)",
          "Cardiac",
          "Cardiac (Gen)",
          "Vascular",
          "Vascular (Gen)",
          "Arrhythmias",
          "Arrhythmias (Gen)",
          "Heart Failure",
          "Heart Failure (Gen)",
          "Vascular Toxicity",
          "Vascular Toxicity (Gen)",
          "Hypertension",
          "Hypertension (Gen)",
          "Vascular (funnel)",
          "Hypertension (funnel)",
          "cardiac (0)-gen",
          "vascular (0)-gen",
          "cardiac (dose)-gen",
          "vascular (dose)-gen",
          "Cardiac (G-E)",
          "Vascular (G-E)",
          "Cardiac (G-E)-Gen",
          "Vascular (G-E)-Gen"
          )



# Divide key_list into groups of two elements
grouped_list <- split(key_list, rep(1:(length(key_list)/2), each = 2))
                      
# Assign keys to each group
names(grouped_list) <- keys

# View the results
#print(grouped_list)
                      
                      
base_dir <- main_output_dir 

# Define base folders
main_dir <- file.path(base_dir, "main")
supp_dir <- file.path(base_dir, "supp")

# Function to determine the correct folder path
find_folder_path <- function(folder_name) {
  main_path <- file.path(main_dir, folder_name)
  supp_path <- file.path(supp_dir, folder_name)
  if (dir.exists(main_path)) {
    return(main_path)
  } else if (dir.exists(supp_path)) {
    return(supp_path)
  } else {
    warning(paste("Folder does not exist in main or supp:", folder_name))
    return(NA)
  }
}

wb <- createWorkbook()

for (key in names(grouped_list)) {
  serious_folder <- grouped_list[[key]][1]
  total_folder   <- grouped_list[[key]][2]
  cat("Processing:",key, "\n")
  cat("Serious: ", serious_folder, "\n")
  cat("Total:   ", total_folder, "\n")
  serious_path <- find_folder_path(serious_folder)
  total_path   <- find_folder_path(total_folder)
  if (!dir.exists(serious_path)) {
    warning(paste("Folder does not exist, skipping:", serious_path))
    next 
  }
  total <- read.csv(paste0(total_path,"/", total_folder, "_league_table_bad_sorted.csv"), header = F, check.names = F, stringsAsFactors = FALSE, na.strings = ".")
  bad <- read.csv(paste0(serious_path, "/", serious_folder, "_league_table_bad_sorted.csv"), header = F, check.names = F, stringsAsFactors = FALSE, na.strings = ".")
  good <- read.csv(paste0(serious_path, "/", serious_folder, "_league_table_good_sorted.csv"), header = F, check.names = F, stringsAsFactors = FALSE, na.strings = ".")
  
  matrix <- as.matrix(total)
  # Extract diagonal elements
  matrix <- apply(matrix, 2, trimws)
  diag_elements <- diag(matrix)
  
  # Diagonal results
  print(diag_elements)
  
  total_diag <- diag(as.matrix(total))
  bad_diag <- diag(as.matrix(bad))
  good_diag <- diag(as.matrix(good))
  
  # Fill in the upper triangle part of the total matrix
  for (i in 1:(nrow(total) - 1)) {
    for (j in (i + 1):ncol(total)) {
      # Get the name of the intersecting therapy
      therapy1 <- total_diag[i]
      therapy2 <- total_diag[j]
      
      # Find the corresponding row and column in bad_diag
      bad_row <- which(bad_diag == therapy1)
      bad_col <- which(bad_diag == therapy2)
      
      if (length(bad_row) > 0 && length(bad_col) > 0) {  # Make sure both exist
        # Compare whether the order of bad_diag is consistent with total_diag
        if ((which(total_diag == therapy1) < which(total_diag == therapy2)) == 
            (which(bad_diag == therapy1) < which(bad_diag == therapy2))) {
          # The order is consistent, fill in the value of the bad table
          total[i, j] <- bad[max(bad_row, bad_col), min(bad_row, bad_col)]
        } else {
          # In the opposite order, fill in the value of the good table
          # Re-tune rows and columns according to total_diag order to extract the correct values ​​from the good table
          good_row <- which(good_diag == therapy1)
          good_col <- which(good_diag == therapy2)
          
          # Use the reciprocal relationship to extract the correct value in the good table
          total[i, j] <- good[max(good_row, good_col), min(good_row, good_col)]
        }
      }
    }
  }
  
  sheet_name <- key
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, total)
  
}
file_path1 <- file.path(main_output_dir,"network-meta-analysis-total_matrix.xlsx")
saveWorkbook(wb, file = file_path1, overwrite = TRUE)



#### CINeMA rating ####
get_confidence <- function(row) {
  counts <- table(factor(unlist(row), levels = c("Low risk", "No concerns", "Some concerns", "Major concerns")))
  some <- counts["Some concerns"]
  major <- counts["Major concerns"]
  low_or_none <- counts["Low risk"] + counts["No concerns"]
  
  # Rule-based logic
  if (some == 0 && major == 0) return("High")
  if (some == 1 && major == 0) return("Moderate")
  if ((some == 1 && major == 1) || (some == 2 && major == 0) || (some == 0 && major == 1)) return("Low")
  if ((some == 3) || (some == 4) || (some == 2 && major == 1) || (some == 0 && major == 2) || (some == 1 && major == 2) || (some == 2 && major == 2)) return("Very low")
  
  return("Undefined")
}

# The path to the data folder
cinema_output_dir <- here("input", "CINeMA_report")
cinema_excel_path <- file.path(cinema_output_dir, "network-meta-analysis-CINEMA.xlsx")

wb <- createWorkbook()

list.files(cinema_output_dir)
for (file in list.files(cinema_output_dir, pattern = "\\.csv$", full.names = FALSE)) {
  #print(str_split(file,'_')[[1]][1])
  data <- read.csv(normalizePath(file.path(cinema_output_dir,file)), , fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
  data1 <- data[,c(0:8)]# Up to the column before ‘Confidence.rating’
  data1$'Confidence rating' <- apply(data1[, 3:8], 1, get_confidence)
  addWorksheet(wb, str_split(file,'_')[[1]][1])
  writeData(wb, str_split(file,'_')[[1]][1], data1)
} 

saveWorkbook(wb, cinema_excel_path, overwrite = TRUE)



# Add evidence to the league table

cinema_path = cinema_excel_path
matrix_path = file.path(main_output_dir, "network-meta-analysis-total_matrix.xlsx")
#print(excel_sheets(cinema_path))
#print(excel_sheets(matrix_path))

key_list <- c("cardiac serious (0)", "cardiac total (0)", 
              "vascular serious (0)", "vascular total (0)",
              "cardiac serious (dose)", "cardiac total (dose)", 
              "vascular serious (dose)", "vascular total (dose)",
              "cardiac serious (dose)-1", "cardiac total (dose)-1", 
              "vascular serious (dose)-1", "vascular total (dose)-1",
              "cardiac serious (dose)-2", "cardiac total (dose)-2", 
              "vascular serious (dose)-2", "vascular total (dose)-2",
              "cardiac serious (dose)-2gen", "cardiac total (dose)-2gen",
              "vascular serious (dose)-2gen", "vascular total (dose)-2gen",
              "cardiac serious (dose)-1gen", "cardiac total (dose)-1gen",
              "vascular serious (dose)-1gen", "vascular total (dose)-1gen",
              "cardiac serious (antiangio)", "cardiac total (antiangio)",
              "cardiac serious (MET)", "cardiac total (MET)",
              "cardiac serious (IGF-1R)",  "cardiac total (IGF-1R)", 
              "vascular serious (antiangio)", "vascular total (antiangio)", 
              "vascular serious (MET)", "vascular total (MET)",
              "vascular serious (IGF-1R)",  "vascular total (IGF-1R)",
              "Serious Cardiac", "Total Cardiac", 
              "Serious Cardiac (Gen)",  "Total Cardiac (Gen)",
              "Serious Vascular", "Total Vascular",
              "Serious Vascular (Gen)",  "Total Vascular (Gen)", 
              "Serious Arrhythmias", "Total Arrhythmias",
              "Serious Arrhythmias (Gen)",  "Total Arrhythmias (Gen)", 
              "Serious Heart Failure", "Total Heart Failure",
              "Serious Heart Failure (Gen)",  "Total Heart Failure (Gen)",
              "Serious Vascular Toxicity","Total Vascular Toxicity",
              "Serious Vascular Toxicity (Gen)", "Total Vascular Toxicity (Gen)",
              "Serious Hypertension", "Total Hypertension",
              "Serious Hypertension (Gen)", "Total Hypertension (Gen)",
              "Serious Vascular","Total Vascular (funnel)",
              "Serious Hypertension","Total Hypertension (funnel)",
              "cardiac serious (0)-gen", "cardiac total (0)-gen", 
              "vascular serious (0)-gen", "vascular total (0)-gen",
              "cardiac serious (dose)-gen", "cardiac total (dose)-gen", 
              "vascular serious (dose)-gen", "vascular total (dose)-gen",
              "Serious Cardiac (G-E)", "Total Cardiac (G-E)",
              "Serious Vascular (G-E)", "Total Vascular (G-E)",
              "Serious Cardiac (G-E)-Gen",  "Total Cardiac (G-E)-Gen",
              "Serious Vascular (G-E)-Gen",  "Total Vascular (G-E)-Gen")


keys <- c("cardiac (0)",
          "vascular (0)",
          "cardiac (dose)",
          "vascular (dose)",
          "cardiac (dose)-1",
          "vascular (dose)-1",
          "cardiac (dose)-2",
          "vascular (dose)-2",
          "cardiac (dose)-2gen",
          "vascular (dose)-2gen",
          "cardiac (dose)-1gen",
          "vascular (dose)-1gen",
          "cardiac (antiangio)",
          "cardiac (MET)",
          "cardiac (IGF-1R)",
          "vascular (antiangio)",
          "vascular (MET)",
          "vascular (IGF-1R)",
          "Cardiac",
          "Cardiac (Gen)",
          "Vascular",
          "Vascular (Gen)",
          "Arrhythmias",
          "Arrhythmias (Gen)",
          "Heart Failure",
          "Heart Failure (Gen)",
          "Vascular Toxicity",
          "Vascular Toxicity (Gen)",
          "Hypertension",
          "Hypertension (Gen)",
          "Vascular (funnel)",
          "Hypertension (funnel)",
          "cardiac (0)-gen",
          "vascular (0)-gen",
          "cardiac (dose)-gen",
          "vascular (dose)-gen",
          "Cardiac (G-E)",
          "Vascular (G-E)",
          "Cardiac (G-E)-Gen",
          "Vascular (G-E)-Gen")

length(keys)
length(key_list)


# Divide key_list into groups of two elements
grouped_list <- split(key_list, rep(1:(length(key_list)/2), each = 2))

# Assign keys to each group
names(grouped_list) <- keys

#print(grouped_list)

matrix_cinema_path <- file.path(main_output_dir, "network-meta-analysis-total_matrix-cinema.xlsx")

wb <- createWorkbook()

for (key in names(grouped_list)) {
  serious_sheet <- grouped_list[[key]][1]
  total_sheet   <- grouped_list[[key]][2]
  cat("Processing:",key, "\n")
  cat("Serious: ", serious_sheet, "\n")
  cat("Total:   ", total_sheet, "\n")
  
  total <- read_excel(matrix_path, sheet = key)
  matrix_data <- as.data.frame(total)
  
  serious_evidence <- read_excel(cinema_path, sheet = serious_sheet)
  total_evidence <- read_excel(cinema_path, sheet = total_sheet)
  
  # Convert evidence to dictionary
  serious_evidence_dict <- setNames(serious_evidence$`Confidence rating`, serious_evidence$Comparison)
  total_evidence_dict <- setNames(total_evidence$`Confidence rating`, total_evidence$Comparison)
  
  # Fill the upper and lower triangle parts of the matrix
  for (i in 1:nrow(matrix_data)) {
    for (j in 1:ncol(matrix_data)) {
      if (i < j) {
        # Upper triangle part (serious)
        # Get the therapy name on the diagonal
        therapy1 <- diag(as.matrix(matrix_data))[i]  # Therapy name in line i
        therapy2 <- diag(as.matrix(matrix_data))[j]  # Therapy name in column j
        # Build pair
        pair <- paste(therapy1, therapy2, sep = ":") 
        pair1 <- paste(therapy2, therapy1, sep = ":")     
        #print(pair)
        if (pair %in% names(serious_evidence_dict)) {
          if (!is.na(matrix_data[i, j])) {
            matrix_data[i, j] <- paste0(matrix_data[i, j], " (", serious_evidence_dict[[pair]], ")")
          }
        }
        else if (pair1 %in% names(serious_evidence_dict)) {
          if (!is.na(matrix_data[i, j])) {
            matrix_data[i, j] <- paste0(matrix_data[i, j], " (", serious_evidence_dict[[pair1]], ")")
          }
        }
      } else if (i > j) {
        # Lower triangle part (total)
        therapy1 <- diag(as.matrix(matrix_data))[j]  # Therapy name in line j
        therapy2 <- diag(as.matrix(matrix_data))[i]  # Therapy name in column i
        # Build pair
        pair <- paste(therapy1, therapy2, sep = ":")    
        pair1 <- paste(therapy2, therapy1, sep = ":")    
        if (pair %in% names(total_evidence_dict)) {
          if (!is.na(matrix_data[i, j])) {
            matrix_data[i, j] <- paste0(matrix_data[i, j], " (", total_evidence_dict[[pair]], ")")
          }
        }
        else if (pair1 %in% names(total_evidence_dict)){
          if (!is.na(matrix_data[i, j])) {
            matrix_data[i, j] <- paste0(matrix_data[i, j], " (", total_evidence_dict[[pair1]], ")")
          }
        }
      }
    }
  }
  addWorksheet(wb, key)
  writeData(wb, key, matrix_data)
  message("Done:", key, "\n")
}
saveWorkbook(wb, matrix_cinema_path, overwrite = TRUE)




# Add evidence to Figure 3 and Figure 4
base_dir <- main_output_dir  

cinema_path = file.path(cinema_output_dir, 'network-meta-analysis-CINEMA.xlsx')
outcome_path = file.path(base_dir, 'final_results_network_meta_outcomes.csv')

outcome_table <- read.csv(outcome_path,header = TRUE, check.names = FALSE)  

colnames(outcome_table)
outcome_table <- outcome_table[,-1]
# Extract all ending column names (i.e. sheet names)
outcome_cols <- setdiff(colnames(outcome_table), "Therapy")
available_sheets <- excel_sheets(cinema_path)
# Initialization result
outcome_final <- outcome_table

# Iterate through each ending (each column = one sheet)
for (outcome in outcome_cols) {
  
  if (!(outcome %in% available_sheets)) {
      message("Skip missing sheets: ", outcome)
      next
    }
    # Read the corresponding sheet (a confidence table for the ending)
  conf_df <- read_excel(cinema_path, sheet = outcome)
  # Split the comparisons to find Therapy vs Placebo
  conf_filtered <- conf_df %>%
    separate(Comparison, into = c("drug1", "drug2"), sep = ":", remove = FALSE) %>%
    filter(drug1 == "Placebo" | drug2 == "Placebo") %>%
    mutate(Therapy = ifelse(drug1 == "Placebo", drug2, drug1)) %>%
    select(Therapy, Confidence = `Confidence rating`)
  
  # Merge the column and confidence in the OR table
  outcome_final <- outcome_final %>%
    left_join(conf_filtered, by = "Therapy") %>%
    mutate(
      !!outcome := ifelse(!is.na(Confidence) & .data[[outcome]] != "NA",
                          paste0(.data[[outcome]], "(", Confidence, ")"),
                          .data[[outcome]])
    ) %>%
    select(-Confidence)  # Clear the Confidence columns merged in the middle
}

outcome_final_path <- file.path(main_output_dir, "final_results_network_meta_outcomes_with_all_confidence.csv")
write.csv(outcome_final, outcome_final_path, row.names = FALSE)


