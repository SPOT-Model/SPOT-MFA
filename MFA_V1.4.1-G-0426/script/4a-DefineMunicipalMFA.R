##################################################################################
##
## Script name:   4a-DefineMunicipalMFA.R
##
## Version:       1.4.1-G-0426
## 
## Author:        Dr Josh Cottom (J.Cottom@imperial.ac.uk)
##
## Organisation:  Imperial College London - Dr Costas Velis (C.Velis@imperial.ac.uk) Research Group.
##
## Last updated:  30/03/2026
##
## Parent script  0-MasterScript.R
##
## Description:   Create a function to run the MFA for each municipality
##
##################################################################################

##################################################################################
##  Define a function to calculate income category as a fractional input                                                
##################################################################################

frac_income <- function(GNI){
  m1 <- Income_thresholds[[which(Income_thresholds$Income_category=="LIC"),"Midpoint"]]
  m2 <- Income_thresholds[[which(Income_thresholds$Income_category=="LMC"),"Midpoint"]]
  m3 <- Income_thresholds[[which(Income_thresholds$Income_category=="UMC"),"Midpoint"]]
  m4 <- Income_thresholds[[which(Income_thresholds$Income_category=="HIC"),"Midpoint"]]
  
  
  if (GNI < m1) {
    return(1)
  } else if (GNI < m2) {
    return(1 + (GNI - m1) / (m2 - m1))
  } else if (GNI < m3) {
    return(2 + (GNI - m2) / (m3 - m2))
  } else if (GNI < m4) {
    return(3 + (GNI - m3) / (m4 - m3))
  } else if(GNI >m4) {
    return(4)
  } else {
    return(NA)
  }
}

#################################################################################
##  Create a function to run the MFA for each municipality (m)                                              
#################################################################################
MFA.municipal <- function(m){

  ##################################################################################
  ##  Sample the inputs for the municipality                                                    
  ##################################################################################
  
  ## Get the RF inputs for the municipality
  tP1pc <- RF_predictions$Waste_gen_rate[m,]
  C0 <- RF_predictions$Plastic_MSW[m,]
  tC1 <- RF_predictions$Col_cov[m,]
  tC2i <- RF_predictions$Form_dry_recy[m,]
  tC2ii <- RF_predictions$Other_recv[m,]
  tC2iii <- RF_predictions$Incin[m,]
  tC3 <- RF_predictions$Cont_disp[m,]
  
  ##################################################################################
  ##  Calculate income category as a fractional input                                                
  ##################################################################################
  
  frac_income_cat <- frac_income(inputs$GNI_per_capita[m])

  ##################################################################################
  ##  Correct for rurality                                                   
  ##################################################################################
  
rural_correction <- function(process_id){
  
  
  if((process_id=="tC2i" | process_id=="tC3")){
    
    ##  Get the rurality correction multiplier for the municipality and process being sampled
    cor_factors <- rural_corrections[which(rural_corrections$Income_level_ID==floor(frac_income_cat) &
                                             rural_corrections$Variable==process_id),c(5,11:14)]
    
    
    cor_factors.high <- rural_corrections[which(rural_corrections$Income_level_ID==ceiling(frac_income_cat) &
                                                  rural_corrections$Variable==process_id),c(5,11:14)]
    
    Param1 <- cor_factors$Parameter_1+(cor_factors.high$Parameter_1-cor_factors$Parameter_1)*(frac_income_cat-floor(frac_income_cat))
    Param2 <- cor_factors$Parameter_2+(cor_factors.high$Parameter_2-cor_factors$Parameter_2)*(frac_income_cat-floor(frac_income_cat))
    if(cor_factors$PDF=="Beta-PERT" | cor_factors$PDF == "Triangular"){
      Param3 <- cor_factors$Parameter_3+(cor_factors.high$Parameter_3-cor_factors$Parameter_3)*(frac_income_cat-floor(frac_income_cat))
    }
    if(cor_factors$PDF=="Beta-PERT"){
      Param4 <- cor_factors$Parameter_4+(cor_factors.high$Parameter_4-cor_factors$Parameter_4)*(frac_income_cat-floor(frac_income_cat))
    }
    
    ##  Sample the rural correction multiplier PDF
    cor_factors <- sample.pdf(iterations,
                              distribution = as.character(cor_factors[1,1]),
                              Param1 = as.numeric(Param1),
                              Param2 = as.numeric(Param2),
                              Param3 = as.numeric(Param3),
                              Param4 = as.numeric(Param4))
  } else {
    
    ##  Get the rurality correction multiplier for the municipality and process being sampled
    cor_factors <- rural_corrections[which(rural_corrections$Income_level_ID==inputs$Income_level[m] &
                                             rural_corrections$Variable==process_id),c(5,11:14)]
    
    ##  Sample the rural correction multiplier PDF
    cor_factors <- sample.pdf(iterations,
                              distribution = as.character(cor_factors[1,1]),
                              Param1 = as.numeric(cor_factors[1,2]),
                              Param2 = as.numeric(cor_factors[1,3]),
                              Param3 = as.numeric(cor_factors[1,4]),
                              Param4 = as.numeric(cor_factors[1,2]))
  }
 
  return(cor_factors)
}

RC_tP1pc <- rural_correction("tP1")
RC_C0 <- rural_correction("C0")
RC_tC1 <- rural_correction("tC1")
RC_tC2i <- rural_correction("tC2i")
RC_tC2ii <- rural_correction("tC2ii")
RC_tC2iii <- rural_correction("tC2iii")
RC_tC3 <- rural_correction("tC3")
  


## Sample from the other input PDFs for the municipality
if(inputs$ISO3[m] %in% IRIS_special$ISO3){ 
  WP <- sample.pdf(iterations,
                   IRIS_special[which(inputs$ISO3[m]==IRIS_special$ISO3 & inputs$DEGURBA_L2_2020[m] == IRIS_special$DEGURBA_L2),"Distribution"],
                   inputs$WP_1[m],inputs$WP_2[m],inputs$WP_3[m],inputs$WP_4[m])
} else {
  WP <- sample.pdf.cat(frac_income_cat,iterations,PDFs[1,5],"WP_1","WP_2","WP_3","WP_4",TRUE)
}

WPp <- sample.pdf.cat(frac_income_cat,iterations,PDFs[2,5],"WPp_1","WPp_2","WPp_3","WPp_4",FALSE)
WPdw <- sample.pdf(iterations,PDFs[3,5],inputs$WPdw_1[m],inputs$WPdw_2[m],inputs$WPdw_3[m],inputs$WPdw_4[m])
WPmw <- sample.pdf(iterations,PDFs[4,5],inputs$WPmw_1[m],inputs$WPmw_2[m],inputs$WPmw_3[m],inputs$WPmw_4[m])
C0a <- sample.pdf.cat(frac_income_cat,iterations,PDFs[5,5],"C0a_1","C0a_2","C0a_3","C0a_4",FALSE)
C3i <- sample.pdf(iterations,PDFs[6,5],inputs$C3i_1[m],inputs$C3i_2[m],inputs$C3i_3[m],inputs$C3i_4[m])
C8 <- sample.pdf(iterations,PDFs[7,5],inputs$C8_1[m],inputs$C8_2[m],inputs$C8_3[m],inputs$C8_4[m])
C9 <- sample.pdf(iterations,PDFs[8,5],inputs$C9_1[m],inputs$C9_2[m],inputs$C9_3[m],inputs$C9_4[m])
C10 <- sample.pdf.cat(frac_income_cat,iterations,PDFs[9,5],"C10_1","C10_2","C10_3","C10_4",FALSE)
C11 <- sample.pdf(iterations,PDFs[10,5],inputs$C11_1[m],inputs$C11_2[m],inputs$C11_3[m],inputs$C11_4[m])
C14 <- sample.pdf(iterations,PDFs[11,5],inputs$C14_1[m],inputs$C14_2[m],inputs$C14_3[m],inputs$C14_4[m])
C15 <- sample.pdf.cat(frac_income_cat,iterations,PDFs[12,5],"C15_1","C15_2","C15_3","C15_4",FALSE)
C16 <- sample.pdf(iterations,PDFs[13,5],inputs$C16_1[m],inputs$C16_2[m],inputs$C16_3[m],inputs$C16_4[m])
C11a <- sample.pdf(iterations,PDFs[14,5],inputs$C11a_1[m],inputs$C11a_2[m],inputs$C11a_3[m],inputs$C11a_4[m])
C14a <- sample.pdf(iterations,PDFs[15,5],inputs$C14a_1[m],inputs$C14a_2[m],inputs$C14a_3[m],inputs$C14a_4[m])
C21a <- sample.pdf(iterations,PDFs[16,5],inputs$C21a_1[m],inputs$C21a_2[m],inputs$C21a_3[m],inputs$C21a_4[m])
C23aa <- sample.pdf(iterations,PDFs[17,5],inputs$C23aa_1[m],inputs$C23aa_2[m],inputs$C23aa_3[m],inputs$C23aa_4[m])
C23ab <- sample.pdf(iterations,PDFs[18,5],inputs$C23ab_1[m],inputs$C23ab_2[m],inputs$C23ab_3[m],inputs$C23ab_4[m])
C24aa <- sample.pdf(iterations,PDFs[19,5],inputs$C24aa_1[m],inputs$C24aa_2[m],inputs$C24aa_3[m],inputs$C24aa_4[m])
C24ab <- sample.pdf(iterations,PDFs[20,5],inputs$C24ab_1[m],inputs$C24ab_2[m],inputs$C24ab_3[m],inputs$C24ab_4[m])

CO2_EF <- sample.pdf(iterations,PDFs[21,5],inputs$CO2_EF_1[m],inputs$CO2_EF_2[m],inputs$CO2_EF_3[m],inputs$CO2_EF_4[m])
CO_EF <- sample.pdf(iterations,PDFs[22,5],inputs$CO_EF_1[m],inputs$CO_EF_2[m],inputs$CO_EF_3[m],inputs$CO_EF_4[m])
SO2_EF <- sample.pdf(iterations,PDFs[23,5],inputs$SO2_EF_1[m],inputs$SO2_EF_2[m],inputs$SO2_EF_3[m],inputs$SO2_EF_4[m])
NOx_EF <- sample.pdf(iterations,PDFs[24,5],inputs$NOx_EF_1[m],inputs$NOx_EF_2[m],inputs$NOx_EF_3[m],inputs$NOx_EF_4[m])
PM2.5_EF <- sample.pdf(iterations,PDFs[25,5],inputs$PM2.5_EF_1[m],inputs$PM2.5_EF_2[m],inputs$PM2.5_EF_3[m],inputs$PM2.5_EF_4[m])
PM10_EF <- sample.pdf(iterations,PDFs[26,5],inputs$PM10_EF_1[m],inputs$PM10_EF_2[m],inputs$PM10_EF_3[m],inputs$PM10_EF_4[m])
EC_EF <- sample.pdf(iterations,PDFs[27,5],inputs$EC_EF_1[m],inputs$EC_EF_2[m],inputs$EC_EF_3[m],inputs$EC_EF_4[m])
PCDD.Fs_TEQ_EF <- sample.pdf(iterations,PDFs[28,5],inputs$PCDD.Fs_TEQ_EF_1[m],inputs$PCDD.Fs_TEQ_EF_2[m],inputs$PCDD.Fs_TEQ_EF_3[m],inputs$PCDD.Fs_TEQ_EF_4[m])
dl.PCB_TEQ_EF <- sample.pdf(iterations,PDFs[29,5],inputs$dl.PCB_TEQ_EF_1[m],inputs$dl.PCB_TEQ_EF_2[m],inputs$dl.PCB_TEQ_EF_3[m],inputs$dl.PCB_TEQ_EF_4[m])

  ## Calculate street sweeping efficiency (S) for the municipality by the different degrees of urbanisation
  S.tmp <- Street_sweep[which(Street_sweep$Income_level_ID==floor(frac_income_cat)),c(2,5,11:14)]
  
  S <- inputs$UCentre_share_2020[m] * sample.pdf(iterations, as.character(S.tmp[1,2]),as.numeric(S.tmp[1,3]),
                                                 as.numeric(S.tmp[1,4]),as.numeric(S.tmp[1,5]),as.numeric(S.tmp[1,6]))+
    inputs$DUC_share_2020[m] * sample.pdf(iterations, as.character(S.tmp[2,2]),as.numeric(S.tmp[2,3]),
                                          as.numeric(S.tmp[2,4]),as.numeric(S.tmp[2,5]),as.numeric(S.tmp[2,6]))+
    inputs$SDUC_share_2020[m] * sample.pdf(iterations, as.character(S.tmp[3,2]),as.numeric(S.tmp[3,3]),
                                           as.numeric(S.tmp[3,4]),as.numeric(S.tmp[3,5]),as.numeric(S.tmp[3,6]))+
    inputs$SUrb_share_2020[m]* sample.pdf(iterations, as.character(S.tmp[4,2]),as.numeric(S.tmp[4,3]),
                                          as.numeric(S.tmp[4,4]),as.numeric(S.tmp[4,5]),as.numeric(S.tmp[4,6]))+
    inputs$Rural_share_2020[m] * sample.pdf(iterations, as.character(S.tmp[5,2]),as.numeric(S.tmp[5,3]),
                                            as.numeric(S.tmp[5,4]),as.numeric(S.tmp[5,5]),as.numeric(S.tmp[5,6]))
  
  S[S>100] <- 100
  
    S.tmp2 <- Street_sweep[which(Street_sweep$Income_level_ID==ceiling(frac_income_cat)),c(2,5,11:14)]
    
    S2 <- inputs$UCentre_share_2020[m] * sample.pdf(iterations, as.character(S.tmp2[1,2]),as.numeric(S.tmp2[1,3]),
                                               as.numeric(S.tmp2[1,4]),as.numeric(S.tmp2[1,5]),as.numeric(S.tmp2[1,6]))+
      inputs$DUC_share_2020[m] * sample.pdf(iterations, as.character(S.tmp2[2,2]),as.numeric(S.tmp2[2,3]),
                                       as.numeric(S.tmp2[2,4]),as.numeric(S.tmp2[2,5]),as.numeric(S.tmp2[2,6]))+
      inputs$SDUC_share_2020[m] * sample.pdf(iterations, as.character(S.tmp2[3,2]),as.numeric(S.tmp2[3,3]),
                                        as.numeric(S.tmp2[3,4]),as.numeric(S.tmp2[3,5]),as.numeric(S.tmp2[3,6]))+
      inputs$SUrb_share_2020[m]* sample.pdf(iterations, as.character(S.tmp2[4,2]),as.numeric(S.tmp2[4,3]),
                                       as.numeric(S.tmp2[4,4]),as.numeric(S.tmp2[4,5]),as.numeric(S.tmp2[4,6]))+
      inputs$Rural_share_2020[m] * sample.pdf(iterations, as.character(S.tmp2[5,2]),as.numeric(S.tmp2[5,3]),
                                         as.numeric(S.tmp2[5,4]),as.numeric(S.tmp2[5,5]),as.numeric(S.tmp2[5,6]))
    
    S2[S2>100] <- 100
    
    S<- S+(S2-S)*(frac_income_cat-floor(frac_income_cat))

  ## Add all inputs to a named list
  MFA.inputs <- list("Pop" = rep(inputs$Population_2020[m],iterations),
                     "tP1pc" = tP1pc,
                     "C0" = C0,
                     "C0a" = C0a,
                     "tC1" = tC1,
                     "tC2i" = tC2i,
                     "tC2ii" = tC2ii,
                     "tC2iii" = tC2iii,
                     "tC3" = tC3,
                     "RC_tP1pc" = RC_tP1pc,
                     "RC_C0" = RC_C0,
                     "RC_tC1" = RC_tC1,
                     "RC_tC2i" = RC_tC2i,
                     "RC_tC2ii" = RC_tC2ii,
                     "RC_tC2iii" = RC_tC2iii,
                     "RC_tC3" = RC_tC3,
                     "WP" = WP,
                     "WPp" = WPp,
                     "WPdw" = WPdw,
                     "WPmw" = WPmw,
                     "Rural_share" = rep(inputs$Rural_share_2020[m],iterations),
                     "C3i" = C3i,
                     "C8" = C8,
                     "C9" = C9,
                     "C10" = C10,
                     "C11" = C11,
                     "C14" = C14,
                     "C15" = C15,
                     "C16" = C16,
                     "C11a" = C11a,
                     "C14a" = C14a,
                     "C21a" = C21a,
                     "C23aa" = C23aa,
                     "C23ab" = C23ab ,
                     "C24aa" = C24aa,
                     "C24ab" = C24ab,
                     "LT" = LT,
                     "S" = S,
                     "CO2_EF"= CO2_EF,
                     "CO_EF" = CO_EF,
                     "SO2_EF" = SO2_EF,
                     "NOx_EF" = NOx_EF,
                     "PM2.5_EF" = PM2.5_EF,
                     "PM10_EF" = PM10_EF,
                     "EC_EF" = EC_EF,
                     "PCDD.Fs_TEQ_EF" = PCDD.Fs_TEQ_EF,
                     "dl.PCB_TEQ_EF" = dl.PCB_TEQ_EF
                      )

  ##################################################################################
  ##  Calculate the processes (Solve the MFA)                                                     
  ##################################################################################
  
  process.results <- MFA(MFA.inputs)
  
  ##################################################################################
  ##  Calculate the coefficients and outputs                                                  
  ##################################################################################
  
  coeff.results <- calc_coefficients(process.results)

  output.results <- calc_outputs(process.results,matrix(c(0,inputs$Population_2020[m]),nrow=1),WP)
  
  ## remove WP from process results and all results to list

    res <- list("Processes" = process.results,
              "Coefficients" = coeff.results,
              "Outputs" = output.results)
    
    res2 <- list(res,MFA.inputs)
    
    return(res2)
}
