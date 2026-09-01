##################################################################################
##
## Script name:   6-AggregateMFA.R
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
## Description:   Aggregates the probabilistic MFA to other levels
##
##################################################################################

##################################################################################
##  Aggregate results to other levels and summarise
##################################################################################

## Back calculate coefficients and outputs for each iteration at a national level
for(i in 1:iterations){
  raw.coeff.nat[,,i] <- calc_coefficients(raw.process.nat[,,i])
  raw.output.nat[,,i] <- calc_outputs(raw.process.nat[,,i],
                                      pop.agg.nat[match(rownames(raw.process.nat),pop.agg.nat$Group.1),],0) #0 is for WP, replace this later with stored value.
  raw.output.nat[,c("WP_no","people_no_col","exp_uncol_plas_ob","exp_IRS_disp_plas_ob","exp_form_disp_plas_ob","exp_ob"),i] <- raw.misc.nat[,,i]
}

## Summarise National results and add to results table
Results[[2]][[1]][,(length(D2)+1):ncol(Results[[2]][[1]])] <- summarise(raw.process.nat)
Results[[2]][[2]][,(length(D2)+1):ncol(Results[[2]][[2]])] <- summarise(raw.coeff.nat)
Results[[2]][[3]][,(length(D2)+1):ncol(Results[[2]][[3]])] <- summarise(raw.output.nat)

if(run_other_agg==T){
  
## Extract the aggregations in the correct order for the results table
aggregations <- Results[[3]][[1]][seq(1,nrow(Results[[3]][[1]]),length(stat.names)),2:3]

##  Convert groups to long format
groups.long <- pivot_longer(groups[,1:8],
                            cols = c("World","Region","Subregion","Intermediate_region","OECD_region","Income_level"),
                            names_to = "Aggregation_lv1",
                            values_to = "Aggregation_lv2")

##   Standardise Lv1 ames between aggregations table and groups.long
groups.long$Aggregation_lv1[which(groups.long$Aggregation_lv1=="Region")] <- "UN Region"
groups.long$Aggregation_lv1[which(groups.long$Aggregation_lv1=="Subregion")] <- "UN Subregion"
groups.long$Aggregation_lv1[which(groups.long$Aggregation_lv1=="Intermediate_region")] <- "UN Intermediate region"
groups.long$Aggregation_lv1[which(groups.long$Aggregation_lv1=="Income_level")] <- "Income category"
groups.long$Aggregation_lv1[which(groups.long$Aggregation_lv1=="OECD_region")] <- "OECD region"

## Sum national process results by each grouping
for(n in 1:nrow(aggregations)){
  
  ##  Extract the ISO3 codes for countries in current grouping
  x <- groups.long$ISO3[which(groups.long$Aggregation_lv1==aggregations[n,1] & 
                                groups.long$Aggregation_lv2==aggregations[n,2])]
  
  if(length(x)>1){
    raw.process.agg[n,,] <- apply(raw.process.nat[match(x,unique(inputs$ISO3)),,],MARGIN=c(2,3),FUN=function(x)sum(x,na.rm = T))
    raw.misc.agg[n,,] <- apply(raw.misc.nat[match(x,unique(inputs$ISO3)),,],MARGIN=c(2,3),FUN=function(x)sum(x,na.rm = T))
  }
  
  if(length(x)==1){
    raw.process.agg[n,,] <- raw.process.nat[match(x,unique(inputs$ISO3)),,]
    raw.misc.agg[n,,] <- raw.misc.nat[match(x,unique(inputs$ISO3)),,]
  }
  
}

## Back calculate coefficients and outputs for each iteration at a other aggreation level
for(i in 1:iterations){
  raw.coeff.agg[,,i] <- calc_coefficients(raw.process.agg[,,i])
  raw.output.agg[,,i] <- calc_outputs(raw.process.agg[,,i],pop.agg.other,0)
  raw.output.agg[,c("WP_no","people_no_col","exp_uncol_plas_ob","exp_IRS_disp_plas_ob","exp_form_disp_plas_ob","exp_ob"),i] <- raw.misc.agg[,,i]
  
}

## Summarise Other aggregation level results
Results[[3]][[1]][,(length(D3)+1):ncol(Results[[3]][[1]])] <- summarise(raw.process.agg)
Results[[3]][[2]][,(length(D3)+1):ncol(Results[[3]][[2]])] <- summarise(raw.coeff.agg)
Results[[3]][[3]][,(length(D3)+1):ncol(Results[[3]][[3]])] <- summarise(raw.output.agg)


A <- list("Municipalities" = raw.save.m)
B <- list("Countries" = list("Processes" = raw.process.nat,
                             "Coefficients" = raw.coeff.nat,
                             "Outputs" = raw.output.nat))

C <- list("Other_agg" = list("Processes" = raw.process.agg,
                             "Coefficients" = raw.coeff.agg,
                             "Outputs" = raw.output.agg))

}

## Save the raw MFA results for aggregations if required
if(length(raw.save.m)>0 & retain_full_munic==T){
  raw.save <- append(raw.save,A)
}

if(retain_full_countries==T){
  raw.save <- append(raw.save,B)
}

if(retain_full_other==T){
  raw.save <- append(raw.save,C)
}

