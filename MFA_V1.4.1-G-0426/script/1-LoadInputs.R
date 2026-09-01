##################################################################################
##
## Script name:   1-LoadInputs.R
##
## Version:       1.4.1-G-0426
## 
## Author:        Dr Josh Cottom (J.Cottom@imperial.ac.uk)
##
## Organisation:  Imperial College London - Dr Costas Velis (C.Velis@imperial.ac.uk) Research Group.
##
## Last updated:  13/14/2026
##
## Parent script  0-MasterScript.R
##
## Description:   Loads inputs for the probabilistic MFA
##
##################################################################################

##################################################################################
##  Load the excel based input files                                                               
##################################################################################

##  Set the working directory as the inputs location
setwd("../inputs")

##  Load the excel PDF sheet
PDFs <- read_excel(workbook,sheet="PDFs")

##  Load the rural correction data
rural_corrections <- read_excel(workbook,
                                sheet = "Rural_correction_factors")

##  Load the China incineration data
China_incin <- read_excel(path=workbook,
                          sheet = "China_incineration",
                          skip = 1)

##  Load the excel inputs sheet specifying the first 7 columns (names) as text and
##  the remaining columns (population & parameters) to be numeric
inputs <- read_excel(workbook,
                     sheet="MFA_inputs",
                     skip=2,
                     col_types = c(rep("text",8),
                                   rep("numeric",32),
                                   rep("numeric",sum(PDFs$No_Parameters))))

##  Load the processes sheet
processes <- read_excel(workbook,sheet = "Processes")

##  Load the excel outputs sheet
coeffs <- read_excel(workbook,sheet = "Coefficients")

##  Load the excel outputs sheet
outputs <- read_excel(workbook,sheet="Outputs")

##  Load the excel groupings sheet
groups <- read_excel(workbook,sheet="Groupings")

## Load the income threshold projections
Income_thresholds <- read_excel(workbook,sheet="Income_thresholds")

##  Load the excel groupings sheet
Street_sweep <- read_excel(workbook,sheet="Street_sweep")

##  Load incineration countries ISO3 as vector
Incin_countries <- read_excel(workbook,sheet="Incineration_countries")$ISO3

##  Load the list of municipalities for which the full raw results should be saved
munic.raw <- read_excel(workbook,sheet="Retain_full_munic")$Unique_ID

## Load special cases for IRIS model
IRIS_special <- read_excel(workbook,sheet="IRIS Model",skip=42)

##  Extract the testing data sheet from the input sheet
testing_data <- inputs[,21:40]

##  Extract the unique IDs of all the municipalities
uniqueID <- inputs$Unique_ID

##  Create an empty warning list
warnings <- list()

##  Check for empty inputs and stop if any are empty
empties <- if(sum(is.na(inputs[,9:ncol(inputs)])==TRUE)>0){
  warnings <- c(warnings,paste0("There are some blank input parameters. Model halted"))
  stop()
}

##################################################################################
##  Load RF models                                                              
##################################################################################

RF_models <- list()

load("Waste_gen_rate_RF")
RF_models$Waste_gen_rate <- RF_model.all_data

load("Col_cov_RF")
RF_models$Col_cov <- RF_model.all_data

load("Plastic_MSW_RF")
RF_models$Plastic_MSW <- RF_model.all_data

load("Form_dry_recy_RF")
RF_models$Form_dry_recy <- RF_model.all_data

load("Other_recv_RF")
RF_models$Other_recv <- RF_model.all_data

load("Incin_RF")
RF_models$Incin <- RF_model.all_data

load("Cont_disp_RF")
RF_models$Cont_disp <- RF_model.all_data

rm(RF_model.all_data)
