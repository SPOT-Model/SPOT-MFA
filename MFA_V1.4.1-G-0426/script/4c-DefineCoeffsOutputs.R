##################################################################################
##
## Script name:   4c-DefineCoeffsOutputs.R
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
## Description:   Create functions to back calculate coefficients and outputs at 
##                an aggregated level
##
##################################################################################

#################################################################################
##  Create function to calculate coefficients from process results
#################################################################################

calc_coefficients <- function(df){
  
  C1 <- (df[,"P2"]/df[,"P1"])*100
  C2 <- (df[,"P5"]/df[,"P3"])*100
  C3 <- (df[,"P6"]/df[,"P5"])*100
  C4 <- (df[,"P9"]/df[,"P7"])*100
  C5 <- (df[,"P11"]/df[,"P8"])*100
  C6i <- (df[,"P13"]/df[,"P9"])*100
  C6ii <- (df[,"P12ii"]/df[,"P9"])*100
  C6iii <- (df[,"P12i"]/df[,"P9"])*100
  C7 <- (df[,"P15"]/df[,"P13"])*100
  C8 <- (df[,"P17"]/df[,"P10"])*100
  C9 <- (df[,"P19"]/df[,"P16"])*100
  C10 <- (df[,"P21"]/df[,"P4"])*100 ## Corrected. Was previously P20/P4
  C11 <- (df[,"P2a"]/df[,"P2"])*100
  C12 <- (df[,"P21a"]/df[,"P21"])*100
  C13 <- (df[,"P6a"]/df[,"P6"])*100
  C14 <- (df[,"P19a"]/df[,"P19"])*100
  C15 <- (df[,"P14a"]/df[,"P14"])*100
  C16 <- (df[,"P15a"]/df[,"P15"])*100
  C17 <- (df[,"P17a"]/df[,"P17"])*100
  C18 <- (df[,"P20a"]/df[,"P20"])*100
  C11a <- (df[,"P2aa"]/df[,"P2a"])*100
  C12a <- (df[,"P21aa"]/df[,"P21a"])*100
  C13a <- (df[,"P6aa"]/df[,"P6a"])*100
  C14a <- (df[,"P19aa"]/df[,"P19a"])*100
  C17a <- (df[,"P17aa"]/df[,"P17a"])*100
  C18a <- (df[,"P20aa"]/df[,"P20a"])*100
  C21a <- (df[,"P14aa"]/df[,"P14a"])*100
  C22a <- (df[,"P15aa"]/df[,"P15a"])*100
  C23aa <- (df[,"P22aa"]/df[,"P14aa"])*100
  C23ab <- (df[,"P22ab"]/df[,"P14ab"])*100
  C24aa <- (df[,"P24aa"]/df[,"P15aa"])*100
  C24ab <- (df[,"P24ab"]/df[,"P15ab"])*100
  C25aa <- (df[,"P26aa"]/df[,"P22aa"])*100
  C25ab <- (df[,"P26ab"]/df[,"P22ab"])*100
  C26aa <- (df[,"P28aa"]/df[,"P24aa"])*100
  C26ab <- (df[,"P28ab"]/df[,"P24ab"])*100
  C27aa <- (df[,"P30aa"]/df[,"P26aa"])*100
  C27ab <- (df[,"P30ab"]/df[,"P26ab"])*100
  C28aa <- (df[,"P32aa"]/df[,"P28aa"])*100
  C28ab <- (df[,"P32ab"]/df[,"P28ab"])*100
  C0 <- C13
  C0a <- C13a
  
  coeff.results <- cbind(C0,C0a,C1,C2,C3,C4,C5,C6i,C6ii,C6iii,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,
                         C11a,C12a,C13a,C14a,C17a,C18a,C21a,C22a,C23aa,C23ab,C24aa,C24ab,C25aa,C25ab,
                         C26aa,C26ab,C27aa,C27ab,C28aa,C28ab)
  
  ## Set any NaN to NA.
  coeff.results[which(is.nan(coeff.results))] <- NA
  
  
  return(coeff.results)
}

#################################################################################
##  Create function to calculate outputs from process results
#################################################################################

calc_outputs <- function(df,pop,WP){

  ## Extract the population
  pop <- pop[,2]
  
  wg <- df[,"P1"]
  wg_per_cap <- ((df[,"P1"]/pop)*1000)/365
  pwg <- df[,"P1a"]
  pwg_per_cap <- ((df[,"P1a"]/pop)*1000)/365
  rigid_pwg <- df[,"P1aa"]
  flex_pwg <-  df[,"P1ab"]
  
  col_cov_pct_gen_exc_lit <- (df[,"P5"]/df[,"P3"])*100
  col_del_pct_gen <- (df[,"P7"]/df[,"P1"])*100
  
  litter_em_pct_gen <- (df[,"P2"]/df[,"P1"])*100
  uncol_em_pct_gen <- (df[,"P4"]/df[,"P1"])*100
  collection_em_pct_gen <- (df[,"P6"]/df[,"P1"])*100
  cont_disp_pct_gen <- (df[,"P11"]/df[,"P1"])*100
  uncont_disp_pct_gen <- df[,"P10"]/df[,"P1"]*100
  compost_pct_gen <- (df[,"P12i"]/df[,"P1"])*100
  incin_pct_gen <- (df[,"P12ii"]/df[,"P1"])*100
  recy_pct_gen <- (df[,"P13"]/df[,"P1"])*100

  recy_form_pct_recy <- (df[,"P15"]/df[,"P13"])*100
  recy_inform_pct_recy <- (df[,"P14"]/df[,"P13"])*100

  cont_disp_pct_disp <- (df[,"P11"]/df[,"P8"])*100
  uncont_disp_pct_disp <- (df[,"P10"]/df[,"P8"])*100
  
  man_cont_pct_gen <- ((df[,"P11"]+df[,"P9"]-df[,"P26aa"]-df[,"P26ab"]-df[,"P28aa"]-df[,"P28ab"])/df[,"P1"])*100
  
  open_burn_MSW <- df[,"P21"]+df[,"P17"]+df[,"P30aa"]+df[,"P30ab"]+df[,"P32aa"]+df[,"P32ab"]
  open_burn_MSW_pct_gen <- (open_burn_MSW/df[,"P1"])*100
  
  plas_debris_em <- df[,"P2a"]+df[,"P6a"]+df[,"P19a"]+df[,"P20a"]+df[,"P31aa"]+df[,"P31ab"]+df[,"P33aa"]+df[,"P33ab"]
  plas_burn_em <- df[,"P17a"]+df[,"P21a"]+df[,"P30aa"]+df[,"P30ab"]+df[,"P32aa"]+df[,"P32ab"]
  plas_em <- plas_debris_em + plas_burn_em
  
  plas_debris_em_pct_em <- (plas_debris_em/plas_em)*100
  plas_burn_em_pct_em <- (plas_burn_em/plas_em)*100
  
  plas_debris_em_per_cap <- (plas_debris_em/pop)*1000
  plas_burn_em_per_cap <- (plas_burn_em/pop)*1000
  plas_em_per_cap <- (plas_em/pop)*1000
  
  rigid_plas_deb_em <- df[,"P2aa"]+ df[,"P6aa"] + df[,"P20aa"] + df[,"P19aa"] + df[,"P31aa"] + df[,"P33aa"]
  flex_plas_deb_em <- df[,"P2ab"]+ df[,"P6ab"] + df[,"P20ab"] + df[,"P19ab"] + df[,"P31ab"] + df[,"P33ab"]
  rigid_plas_ob_em <- df[,"P21aa"] + df[,"P17aa"] + df[,"P30aa"] + df[,"P32aa"]
  flex_plas_ob_em <- df[,"P21ab"] + df[,"P17ab"] + df[,"P30ab"] + df[,"P32ab"]
  
  plas_em_pct_pwg <- (plas_em/pwg)*100
  rigid_plas_em_pct_rigid_pwg <- ((rigid_plas_deb_em + rigid_plas_ob_em)/rigid_pwg)*100
  flex_plas_em_pct_flex_pwg <- ((flex_plas_deb_em+flex_plas_ob_em)/flex_pwg)*100
  
  rigid_plas_deb_em_pct_em <- (rigid_plas_deb_em/plas_em)*100
  flex_plas_deb_em_pct_em <- (flex_plas_deb_em/plas_em)*100
  rigid_plas_ob_em_pct_em <- (rigid_plas_ob_em/plas_em)*100
  flex_plas_ob_em_pct_em <- (flex_plas_ob_em/plas_em)*100
  
  plas_litter_em <- df[,"P2a"]
  plas_uncol_em <- df[,"P20a"]+df[,"P21a"]
  plas_collection_em <- df[,"P6a"]
  plas_disp_em <- df[,"P19a"]+df[,"P17a"]
  plas_recy_em <- df[,"P31aa"]+df[,"P31ab"]+df[,"P33aa"]+df[,"P33ab"]+df[,"P30aa"]+df[,"P30ab"]+df[,"P32aa"]+df[,"P32ab"]
  
  plas_litter_em_pct_em <- (plas_litter_em/plas_em)*100
  plas_uncol_em_pct_em <- (plas_uncol_em/plas_em)*100
  plas_collection_em_pct_em <- (plas_collection_em/plas_em)*100
  plas_disp_em_pct_em <- (plas_disp_em/plas_em)*100
  plas_recy_em_pct_em <- (plas_recy_em/plas_em)*100
  
  plas_litter_deb_em_pct_em <- plas_litter_em_pct_em
  plas_uncol_deb_em_pct_em <- (df[,"P20a"]/plas_em)*100
  plas_uncol_ob_em_pct_em <- (df[,"P21a"]/plas_em)*100
  plas_collection_deb_em_pct_em <- (df[,"P6a"]/plas_em)*100
  plas_disp_deb_em_pct_em <- (df[,"P19a"]/plas_em)*100
  plas_disp_ob_em_pct_em <- (df[,"P17a"]/plas_em)*100
  plas_inf_deb_em_pct_em <- ((df[,"P31aa"]+df[,"P31ab"])/plas_em)*100
  plas_inf_ob_em_pct_em <- ((df[,"P30aa"]+df[,"P30ab"])/plas_em)*100
  plas_form_deb_em_pct_em <- ((df[,"P33aa"]+df[,"P33ab"])/plas_em)*100
  plas_form_ob_em_pct_em <- ((df[,"P32aa"]+df[,"P32ab"])/plas_em)*100

  WP_no <- WP/100 * pop
  people_no_col <- df[,"P4"]/(df[,"P3"]/pop)
  exp_uncol_plas_ob <- df[,"P21a"]/((df[,"P1a"]-df[,"P2a"])/pop)
  exp_IRS_disp_plas_ob <- as.numeric(plas_disp_ob_em_pct_em > 0)*WP_no*0.28
  exp_form_disp_plas_ob <- as.numeric(plas_disp_ob_em_pct_em > 0)*(0.1182*(df[,"P10"]/1000)+3.3726) #Linear regression of personnel to '000 tonnes landfill capacity as per http://documents.worldbank.org/curated/en/09903222417508404
  exp_ob <- exp_uncol_plas_ob + exp_IRS_disp_plas_ob + exp_form_disp_plas_ob

  # Combine the output results
  output.results <- cbind(wg,wg_per_cap,pwg,pwg_per_cap,rigid_pwg,flex_pwg,
                          col_cov_pct_gen_exc_lit,col_del_pct_gen,
                          litter_em_pct_gen,uncol_em_pct_gen,collection_em_pct_gen,cont_disp_pct_gen,uncont_disp_pct_gen,compost_pct_gen,incin_pct_gen,recy_pct_gen,
                          recy_form_pct_recy,recy_inform_pct_recy,
                          cont_disp_pct_disp,uncont_disp_pct_disp,
                          man_cont_pct_gen,
                          open_burn_MSW,open_burn_MSW_pct_gen,
                          plas_debris_em,plas_burn_em,plas_em,
                          plas_debris_em_pct_em,plas_burn_em_pct_em,
                          plas_debris_em_per_cap,plas_burn_em_per_cap,plas_em_per_cap,
                          plas_em_pct_pwg,rigid_plas_em_pct_rigid_pwg,flex_plas_em_pct_flex_pwg,
                          rigid_plas_deb_em,flex_plas_deb_em,rigid_plas_ob_em,flex_plas_ob_em,
                          rigid_plas_deb_em_pct_em,flex_plas_deb_em_pct_em,rigid_plas_ob_em_pct_em,flex_plas_ob_em_pct_em,
                          plas_litter_em,plas_uncol_em,plas_collection_em,plas_disp_em,plas_recy_em,
                          plas_litter_em_pct_em,plas_uncol_em_pct_em,plas_collection_em_pct_em,plas_disp_em_pct_em,plas_recy_em_pct_em,
                          plas_litter_deb_em_pct_em,plas_uncol_deb_em_pct_em,plas_uncol_ob_em_pct_em,plas_collection_deb_em_pct_em,plas_disp_deb_em_pct_em,plas_disp_ob_em_pct_em,plas_inf_deb_em_pct_em,plas_inf_ob_em_pct_em,plas_form_deb_em_pct_em,plas_form_ob_em_pct_em,
                          WP_no,people_no_col,exp_uncol_plas_ob,exp_IRS_disp_plas_ob,exp_form_disp_plas_ob,exp_ob
                          )
  
  ## Set any NaN to NA.
  output.results[which(is.nan(output.results))] <- NA
  
  return(output.results)
  
}