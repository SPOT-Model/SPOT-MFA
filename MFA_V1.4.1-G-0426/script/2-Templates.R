##################################################################################
##
## Script name:   2-Templates.R
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
## Description:   Creates templates to store sampled inputs and results
##
##################################################################################

##################################################################################
##  Define a function that creates a raw MFA result template for municipal level
##  results for a given country                                                        
##################################################################################

raw.mc_template <- function(m.country){
  
  raw.mc <- array(NA,dim=c(iterations,
                           length(processes$Process_ID),
                           length(m.country)))
  
  return(raw.mc)
}


##################################################################################
##  Create raw MFA result template for aggregated level results                                                        
##################################################################################

##  Create arrays for holding raw national results
raw.process.nat <- array(NA,dim=c(length(unique(inputs$ISO3)),
                                  length(processes$Process_ID),
                                  iterations))

raw.coeff.nat <- array(NA,dim=c(length(unique(inputs$ISO3)),
                                  length(coeffs$Coeff_ID),
                                  iterations))

raw.output.nat <- array(NA,dim=c(length(unique(inputs$ISO3)),
                                  length(outputs$Output_ID),
                                  iterations))

raw.misc.nat<- array(NA,dim=c(length(unique(inputs$ISO3)),
                                   6, 
                                   iterations))

rownames(raw.process.nat) <- unique(inputs$ISO3)
rownames(raw.coeff.nat) <- unique(inputs$ISO3)
rownames(raw.output.nat) <- unique(inputs$ISO3)
rownames(raw.misc.nat) <- unique(inputs$ISO3)
colnames(raw.process.nat) <- processes$Process_ID
colnames(raw.coeff.nat) <- coeffs$Coeff_ID
colnames(raw.output.nat) <- outputs$Output_ID
colnames(raw.misc.nat) <- c("WP_no","people_no_col","exp_uncol_plas_ob","exp_IRS_disp_plas_ob","exp_form_disp_plas_ob","exp_ob")


##  Create templates for holding raw other aggregation results
if(run_other_agg == T){
  
  ##  Combine the other aggregations
  Other_agg <- c(sort(unique(groups$World)),
                 sort(unique(groups$Income_level)),
                 sort(unique(groups$Region)),
                 sort(unique(groups$Subregion)),
                 sort(unique(groups$Intermediate_region)),
                 sort(unique(groups$OECD_region)))
  
  ##  Create arrays for holding raw other aggregation results
  raw.process.agg <- array(NA,dim=c(length(Other_agg),
                                    length(processes$Process_ID),
                                    iterations))
  
  raw.coeff.agg <- array(NA,dim=c(length(Other_agg),
                                  length(coeffs$Coeff_ID),
                                  iterations))
  
  raw.output.agg <- array(NA,dim=c(length(Other_agg),
                                   length(outputs$Output_ID),
                                   iterations))
  
  raw.misc.agg<- array(NA,dim=c(length(Other_agg),
                                6, 
                                iterations))
  
  rownames(raw.process.agg) <- Other_agg
  rownames(raw.coeff.agg) <- Other_agg
  rownames(raw.output.agg) <- Other_agg
  rownames(raw.misc.agg) <- Other_agg
  colnames(raw.process.agg) <- processes$Process_ID
  colnames(raw.coeff.agg) <- coeffs$Coeff_ID
  colnames(raw.output.agg) <- outputs$Output_ID
  colnames(raw.misc.agg) <- c("WP_no","people_no_col","exp_uncol_plas_ob","exp_IRS_disp_plas_ob","exp_form_disp_plas_ob","exp_ob")
}


##################################################################################
##  Create summarized results template                                                        
##################################################################################

##  Extract the reporting statistics names and replace with cleaned names for quantiles.
stat.names <- gsub("Quantiles.","",names(unlist(summary.stats)))

##  Create a function to generate dataframes to hold the summarized results of processes, 
##  coefficient and other outputs from the Monte Carlo MFA results
summarised_results <- function(rows,description_cols){
  x <- data.frame(matrix(NA,
                         ncol = length(processes$Process_ID)+length(description_cols),
                         nrow = length(rows)*(length(stat.names))))
  
  y <-  data.frame(matrix(NA,
                          ncol = length(coeffs$Coeff_ID)+length(description_cols),
                          nrow = length(rows)*(length(stat.names))))
  
  z <- data.frame(matrix(NA,
                         ncol = length(outputs$Output_ID)+length(description_cols),
                         nrow = length(rows)*(length(stat.names))))

  ##  Add column names
    colnames(x) <- c(description_cols,processes$Process_ID)
    colnames(y) <- c(description_cols,coeffs$Coeff_ID)
    colnames(z) <- c(description_cols,outputs$Output_ID)
    
  ## Add to a list and return
    return(list("Processes" = x,
                "Coefficients" =y,
                "Outputs" = z))
    
}


##  Create the summarized result templates description column names
D1 <- c("Statistic","Country","ISO3","Income_cat","Region","Subregion","Intermediate_region",
        "OECD_region","Municipality","Unique_ID","Population_2020","Rural_share","Deg_Urban_Lv1","Deg_Urban_Lv2")

D2 <- D1[c(1:8,11:12)]

D3 <- c(D1[1],"Aggregation_lv1","Aggregation_lv2",D1[11:12])


if(run_other_agg==T){
  Results <- list("Municipal.results" = Municipal.results <- summarised_results(rows = uniqueID,
                                                                                description_cols = D1),
                  "National.results" = National.results <- summarised_results(rows = unique(inputs$ISO3),
                                                                              description_cols = D2),
                  "Other_aggregated.results" = Other_aggregations.results <- summarised_results(rows = Other_agg,
                                                                                                description_cols = D3)
  )
} else {
  Results <- list("Municipal.results" = Municipal.results <- summarised_results(rows = uniqueID,
                                                                                description_cols = D1),
                  "National.results" = National.results <- summarised_results(rows = unique(inputs$ISO3),
                                                                              description_cols = D2)
  )
  
}

##################################################################################
##  Populate the result templates with basic information                                                  
##################################################################################

##  Populate Municipal results data frames
for(i in 1:3){
Results[[1]][[i]]$Statistic <- rep(stat.names,length(uniqueID))
Results[[1]][[i]]$Country <- rep(inputs$Country,each=length(stat.names))
Results[[1]][[i]]$ISO3 <- rep(inputs$ISO3,each=length(stat.names))
Results[[1]][[i]]$Income_cat <- groups[match(Results[[1]][[i]]$ISO3,groups$ISO3),"Income_level"]$Income_level
Results[[1]][[i]]$Region <- groups[match(Results[[1]][[i]]$ISO3,groups$ISO3),"Region"]$Region
Results[[1]][[i]]$Subregion <- groups[match(Results[[1]][[i]]$ISO3,groups$ISO3),"Subregion"]$Subregion
Results[[1]][[i]]$Intermediate_region <- groups[match(Results[[1]][[i]]$ISO3,groups$ISO3),"Intermediate_region"]$Intermediate_region
Results[[1]][[i]]$OECD_region <- groups[match(Results[[1]][[i]]$ISO3,groups$ISO3),"OECD_region"]$OECD_region
Results[[1]][[i]]$Municipality <- rep(inputs$Municipality,each=length(stat.names))
Results[[1]][[i]]$Unique_ID <- rep(inputs$Unique_ID,each=length(stat.names))
Results[[1]][[i]]$Population_2020<- rep(inputs$Population_2020,each=length(stat.names))
Results[[1]][[i]]$Rural_share<- rep(inputs$Rural_share_2020,each=length(stat.names))
Results[[1]][[i]]$Deg_Urban_Lv1 <- rep(inputs$DEGURBA_L1_2020,each=length(stat.names))
Results[[1]][[i]]$Deg_Urban_Lv2 <- rep(inputs$DEGURBA_L2_2020,each=length(stat.names))
}

##  Calcualte the rural population for each municipality
inputs$rural_pop_2020 <- inputs$Rural_share_2020 * inputs$Population_2020

## Aggregate population and rural population to the National level
pop.agg.nat <- aggregate(inputs$Population_2020,by=list(inputs$ISO3),FUN=sum)
rural_pop.agg.nat <- aggregate(inputs$rural_pop_2020,by=list(inputs$ISO3),FUN=sum)

##  Calculate the aggregated rural population share
rural.pop_pct.agg.nat <- rural_pop.agg.nat$x/pop.agg.nat$x

##  Populate National results dataframes
for(i in 1:3){
  Results[[2]][[i]]$Statistic <- rep(stat.names,length(unique(inputs$ISO3)))
  Results[[2]][[i]]$Country <- rep(unique(inputs$Country),each=length(stat.names))
  Results[[2]][[i]]$ISO3 <- rep(unique(inputs$ISO3),each=length(stat.names))
  Results[[2]][[i]]$Income_cat <- groups[match(Results[[2]][[i]]$ISO3,groups$ISO3),"Income_level"]$Income_level
  Results[[2]][[i]]$Region <- groups[match(Results[[2]][[i]]$ISO3,groups$ISO3),"Region"]$Region
  Results[[2]][[i]]$Subregion <- groups[match(Results[[2]][[i]]$ISO3,groups$ISO3),"Subregion"]$Subregion
  Results[[2]][[i]]$Intermediate_region <- groups[match(Results[[2]][[i]]$ISO3,groups$ISO3),"Intermediate_region"]$Intermediate_region
  Results[[2]][[i]]$OECD_region <- groups[match(Results[[2]][[i]]$ISO3,groups$ISO3),"OECD_region"]$OECD_region
  Results[[2]][[i]]$Population_2020<- rep(pop.agg.nat$x,each=length(stat.names))
  Results[[2]][[i]]$Rural_share<- rep(rural.pop_pct.agg.nat,each=length(stat.names))
}

if(run_other_agg==T){
  groups$Population_2020 <- pop.agg.nat[match(groups$ISO3,pop.agg.nat$Group.1),2]
  
  
  ## Aggregate population and rural population to the Other aggregations level
  pop.agg.world <- aggregate(groups$Population_2020,by=list(groups$World),FUN=sum)
  pop.agg.income <- aggregate(groups$Population_2020,by=list(groups$Income_level),FUN=sum)
  pop.agg.region <- aggregate(groups$Population_2020,by=list(groups$Region),FUN=sum)
  pop.agg.subregion <- aggregate(groups$Population_2020,by=list(groups$Subregion),FUN=sum)
  pop.agg.intregion <- aggregate(groups$Population_2020,by=list(groups$Intermediate_region),FUN=sum)
  pop.agg.OECD <- aggregate(groups$Population_2020,by=list(groups$OECD_region),FUN=sum)
  
  pop.agg.other <- rbind(pop.agg.world,
                   pop.agg.income,
                   pop.agg.region,
                   pop.agg.subregion,
                   pop.agg.intregion,
                   pop.agg.OECD)
  
  groups$Rural_pop <- rural_pop.agg.nat[match(groups$ISO3,rural_pop.agg.nat$Group.1),2]
  
  rural.pop.agg.world <- aggregate(groups$Rural_pop,by=list(groups$World),FUN=sum)
  rural.pop.agg.income <- aggregate(groups$Rural_pop,by=list(groups$Income_level),FUN=sum)
  rural.pop.agg.region <- aggregate(groups$Rural_pop,by=list(groups$Region),FUN=sum)
  rural.pop.agg.subregion <- aggregate(groups$Rural_pop,by=list(groups$Subregion),FUN=sum)
  rural.pop.agg.intregion <- aggregate(groups$Rural_pop,by=list(groups$Intermediate_region),FUN=sum)
  rural.pop.agg.OECD <- aggregate(groups$Rural_pop,by=list(groups$OECD_region),FUN=sum)
  
  rural.pop.agg.other <- rbind(rural.pop.agg.world,
                               rural.pop.agg.income,
                               rural.pop.agg.region,
                               rural.pop.agg.subregion,
                               rural.pop.agg.intregion,
                               rural.pop.agg.OECD)
  
  ##  Calculate the aggregated rural population share
  rural.pop_pct.agg.other <- rural.pop.agg.other$x/pop.agg.other$x
  
  
  ##  Populate Other aggregations results dataframes
  for(i in 1:3){
    Results[[3]][[i]]$Statistic <- rep(stat.names,length(Other_agg))
    Results[[3]][[i]]$Aggregation_lv1 <- rep(c("World",
                                                 rep("Income category",each=length(unique(groups$Income_level))),
                                                 rep("UN Region",each=length(unique(groups$Region))),
                                                 rep("UN Subregion",each=length(unique(groups$Subregion))),
                                                 rep("UN Intermediate region",each=length(unique(groups$Intermediate_region))),
                                                 rep("OECD region",each=length(unique(groups$OECD_region))))
                                              ,each=length(stat.names))
    Results[[3]][[i]]$Aggregation_lv2 <- rep(Other_agg,each=length(stat.names))
    Results[[3]][[i]]$Population_2020<- rep(pop.agg.other$x,each=length(stat.names))
    Results[[3]][[i]]$Rural_share<- rep(rural.pop_pct.agg.other,each=length(stat.names))
  }
}

##################################################################################
##  Create lists to hold the raw outputs for any municipalities or groupings being
##  saved as well as any sensitivity results                                                      
##################################################################################

##  Create a list to hold the raw MFA result for selected municipalities
raw.save.m <- replicate(length(munic.raw),list())
names(raw.save.m) <- munic.raw
raw.save <- list()
