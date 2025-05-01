# Treatment-related cardiac and vascular adverse events associated with epidermal growth factor receptor tyrosine kinase inhibitor-based therapies in EGFR-mutated non-small-cell lung cancer: systematic review and network meta-analysis

## Overview

This study was based on the framework of network meta-analysis to investigate treatment-related cardiac and vascular adverse events in patients with non-small cell lung cancer treated with related therapies such as EGFR inhibitors, in addition to a detailed assessment using pairwise meta-analysis, Bayesian framework of network meta-analysis, single-group meta-analysis, subgroup analyses, and regression analyses.

All input data are provided, and every step from data loading to final figure/table output is fully documented and executable. This structure is designed to meet reproducibility requirements for journal submission.

---

## Folder Structure

- `input/`: Contains all raw input files (e.g., Excel sheets).
- `outcomes/`: Automatically generated output files (e.g., figures, tables).
- `scripts/`: All R scripts, modularized by function:
  - `1. Network-meta-analysis.r`: Perform network meta-analysis on each sheet and output graphs and tables.
  - `2. Forest-plot.r`: Forest plots for comparison of cardiac and vascular adverse drug reactions between EGFR TKIs and placebo.
  - `3. Incidence heat map.r`: The incidence heat map of the first to third generation EGFR TKIs based on primary and secondary outcomes.
  - `4. Funnel_plot.r`: Generating funnel plots for network meta-analyses of primary outcomes, secondary outcomes and retention of high quality studies.
  - `5. Subgroup forest plot.r`: Extraction of summary outcome estimates for head-to-head comparisons in paired meta-analyses, and visualisation for graphs.
  - `6. Single group meta-analysis.r`: Extraction of primary outcome and secondary outcome safety data for the EGFR-TKI arm of the RCTs, single arm trials, observational studies, single group meta-analysis, forest plotting.
  - `7. Bayesian-network-meta-analysis.r`: Bayesian network meta-analysis was performed based on primary and secondary outcomes, and SUCRA values were collected for each therapy to draw heat maps.
  - `8. Baseline_distribution.r`: Generate baseline (sex, age, proportion of Asian population, proportion of non-smokers, median follow-up duration) distribution of all therapies included in the randomised controlled trial.
  - `9. Bayesian-network-regression-analysis.r`: Bayesian network meta-regression analyses (regression dependent on continuous and binary variables) were performed on the primary and secondary outcomes, effect values from regression analyses were extracted to construct tables, SUCRA values from binary regression analyses were extracted, heat maps were plotted, and multivariate correlation analyses were performed to compare differences between groups.
  - `10. Network meta-analysis for efficacy analysis.r`: Efficacy analysis of PFS (progression-Free-Survival) data from included RCTs using network meta-analysis.
- `README.md`: This document.

---

## R Version: 4.2.3

---

## How to Reproduce the Analysis

1. **Install required R packages**:

   ```r
   install.packages(c("openxlsx", "stringr", "dplyr", "tidyr", "readr", "netmeta", "readxl",
      "meta", "forestploter", "grid", "ggsci", "ggplot2", "igraph", "GGally", "tidyverse"))

   if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
   BiocManager::install(c("ComplexHeatmap", "circlize", "GetoptLong"))
   ```

2. **JAGS Library (Required for `rjags`)**:

   The rjags package requires that you have the JAGS library installed on your system. Install it from:

   - **Windows**: Download installer from [JAGS official site](https://sourceforge.net/projects/mcmc-jags/files/)
   - **macOS**: `brew install jags`
   - **Linux**: `sudo apt-get install jags`
   
   Once JAGS is installed, install the R packages:

   ```r
   install.packages(c("rjags", "gemtc"))
   ```

3. **Set working directory to the project root**:

   It is necessary to replace the main output path in each code file with your actual local path to the project directory (your_path_to_project). 

   For example, if your local path is:  

   ```bash
   /Users/yourname/projects/NMA-for-EGFR-TKIs
   ```  

   Then the following line:

   ```r
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/1. Network-meta-analysis.r")
   ```

   should be updated to:

   ```r
   source("/Users/yourname/projects/NMA-for-EGFR-TKIs/NMA-for-EGFR-TKI/scripts/1. Network-meta-analysis.r")
   ```

   In each script, you will also find similar path definitions that need to be updated, such as:

   ```r
   main_output_dir <- "/your_path_to_project/NMA-for-EGFR-TKI/outcomes/" # Change to your main output directory
   if (!dir.exists(main_output_dir)) dir.create(main_output_dir)
   setwd(main_output_dir)

   file_path <- "/your_path_to_project/NMA-for-EGFR-TKI/input/" # Change to your data folder path
   ```
   Make sure to **replace all occurrences** of `/your_path_to_project/` with your actual path before running the scripts.

4. **Run the main script**:

   Note: None of the scripts will run correctly unless you first replace all instances of /your_path_to_project/ with your actual local path.
   
   ```r
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/1. Network-meta-analysis.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/2. Forest-plot.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/3. Incidence heat map.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/4. Funnel_plot.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/5. Subgroup forest plot.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/6. Single group meta-analysis.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/7. Bayesian-network-meta-analysis.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/8. Baseline_distribution.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/9. Bayesian-network-regression-analysis.r")
   source("/your_path_to_project/NMA-for-EGFR-TKI/scripts/10. Network meta-analysis for efficacy analysis.r")
   ```
---

## Notes
For further details on the full dataset of the network meta-analysis please contact the corresponding author Professor Kang Xiaohong (1fy2014036@xxmu.edu.cn) and Professor Ai Sizhi (ai_sz@xxmu.edu.cn). 