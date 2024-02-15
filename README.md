# Milk Quality and Environmental Impact Analysis

## Description
This repository contains the R source code and data necessary to reproduce the analysis conducted in the publication titled "Relationship between milk intrinsic quality scores and the environmental impact scores of dairy farms." The analysis aims to investigate the correlation between milk intrinsic quality scores and the environmental impact scores of dairy farms.

## Features
- Implementation of statistical analysis methods in R to assess the relationship between milk quality and environmental impact.
- Data preprocessing scripts to clean and prepare the datasets for analysis.
- Statistical modeling scripts to perform the correlation analysis, including:
  - Automatic Principal Component Analysis (PCA)
  - Analysis of Variance (ANOVA)
  - Correlation Analysis
- Customization flexibility: The scripts are structured to enable easy customization. By modifying the information in the `data/dictionary.xlsx` file, users can perform similar analyses on different datasets without needing to change the scripts themselves. This feature enhances efficiency and encourages exploration of various datasets.
- Automatic result generation: Each script generates a `results` folder containing all necessary information and graphics as a result of the analysis, enhancing organization and accessibility of the output.


  
## Installation
1. Clone this repository to your local machine.
2. Ensure you have R (version 4.1.2 or higher) installed on your system.
3. The following R packages are required to run the scripts:
   - FactoMineR
   - factoextra
   - nFactors
   - readxl
   - tidyverse
   - missMDA
   - ggforce
   - rstatix
   - ggpubr
   - GGally

   You can install them by executing `install.packages("package_name")` in the R console.



## Usage
### Running the Scripts via Terminal/Command Prompt
- Open your terminal/command prompt.
- Navigate to the directory containing the R scripts.
- Run the appropriate R scripts for the analysis you wish to perform using the `Rscript` command, for example:
  - For PCA analysis: `Rscript PCA.R`
  - For ANOVA analysis: `Rscript ANOVA.R var_name` (e.g., `Rscript ANOVA.R GEN_AB`)
  - For correlation analysis: `Rscript correlation_analysis.R`

### Using RStudio
- Open RStudio.
- Open the desired R script (`PCA.R`, `ANOVA.R`, or `correlation_analysis.R`).
- Execute the script line by line or by selecting the entire script and running it.

## Data
Please note that the dataset included in this repository is a reduced and anonymized version of the actual data used for the article. 

## Authors
- [Grollemund Paul-Marie](https://github.com/pmgrollemund/)
- Lepoivre Mathieu

## License
This project is licensed under a [Creative Commons License](https://creativecommons.org/) allowing free reuse of the research work. 
