
##Needed Libraries

library(tidyverse)


###########Utility functions to create and merge in county level hospitalization rates based on county age strata ########

#Create multinomial "dictionary" for age strata
dict_merge <- function(dict = "UT_geo_id_dictionary.csv", dta = "cc-est2018-alldata.csv", state = "Utah"){
  require(dplyr)
  
  #read files in
  df <- read.csv(dta)
  df_dict <- read.csv(dict)
  
  
  #Filter to state
  df <- droplevels(df[df$STNAME==state,])
  
  #merge dict info in
  df <- merge(df, df_dict, by ="CTYNAME", all = T)
  
  #Select Total Rows to calculate 
  df_total <- df %>% 
    filter(AGEGRP==0) %>% 
    select( STNAME,CTYNAME,geoid,YEAR,TOT_POP)
  
  names(df_total) <- c("STNAME","CTYNAME","geoid", "YEAR", "TOTAL_POPULATION")
  
  #remove total rows from original Dataframe
  df <- df %>%
    filter(AGEGRP!=0)
  
  #add total row in as a column
  df <- merge(df, df_total, by = c("STNAME","CTYNAME", "YEAR", "geoid"), all = T)
  
  #Calculate Age group Proportions
  df$AGEGRP_PROP <- df$TOT_POP/df$TOTAL_POPULATION
  
  #Keep only most recent year (perhaps later we will change this but most recent seems reasonable for now)
  df_agg <- df %>%
    group_by(STNAME, CTYNAME) %>%
    filter(YEAR == max(YEAR)) %>%
    select(STNAME, CTYNAME,geoid, AGEGRP, AGEGRP_PROP, TOT_POP, TOTAL_POPULATION ) %>% 
    pivot_wider(id_cols = AGEGRP,names_from = geoid ,values_from = AGEGRP_PROP)
  
  
  
  df_agg
  
}


#calculate single hospitalization rate based on strata
strat_rates <- function( dta = df_agg,  num_obs= 10 , s_size = 1000,
                         age_rates = c(0,0,.0004,.0004, .0104, .0104, .0343, .0343, .0425, .0425, .0816, .0816, .118, .118, .166, .166,.184,.184)){
  
  #initialize dataframe to hold overall proportions
  df_prop <- data.frame(geoid = names(dta[-1]),p_hosp=NA)
  
  #FOr each county, loop to get age dist. multinomial draws
  for(i in 1:(ncol(dta)-1)){
    age_dist = unclass(unlist(dta[,i]))
    
    #initialize count of success var. with 0 for each county
    suc <- 0
    
    #generate age buckets
    draws <- rmultinom(n = num_obs, size = s_size,prob = age_dist)
    
    #loop to get binomial draws for each age group within the county
    for(j in 1:dim(draws)[1]){
      
      suc <- sum(suc, rbinom(n = draws[j,],size = draws[j,],prob = age_rates[j]))
      
    }
    
    #Get overall county hospitalization rate 
    df_prop$p_hosp[i] <-  suc/sum(draws)
    
  }
  
  df_prop
}



#version of hosp_create_delay_frame to allow for county specific hosp rate, hinges on cty=TRUE to use county specific hosp rate, and include p_X as "p_hosp" 
hosp_create_delay_frame_strat <- function(X, p_X, data_, X_pars, varname, cty = T) {
  
  if(cty==F ){
    
    print("Dictionary, county level data or state left undefined, calculating hospitalizations using flat hospitalization rate")
    
    X_ <- rbinom(length(data_[[X]]),data_[[X]],p_X)
    rc <- data.table::data.table(
      time = data_$time + round(exp(X_pars[1] + X_pars[2]^2 / 2)),
      uid = data_$uid,
      count = X_
    )
    names(rc)[3] <- paste0("incid",varname)
    return(rc)
    
  }else{
    print("Calculating with county specific, age stratified hospitalization rate")
    
    sz <- enquo(X)
    pb <- enquo(p_X)
    
    X_ <- as.vector(unlist(data_ %>%
                             group_by(geoid, time) %>%
                             summarise(X_ = rbinom(n = 1,size = !! sz,prob = !! pb)) %>%
                             ungroup() %>% 
                             select(X_)))
    
    
    rc <- data.table::data.table(
      time = data_$time + round(exp(X_pars[1] + X_pars[2]^2 / 2)),
      uid = data_$uid,
      count = X_
    )
    names(rc)[3] <- paste0("incid",varname)
    return(rc) 
    
  }
  
}


#######Prerp Data #############
#This section will need to be placed somewhere to run once for each report run

#Prep  County Level Age Group Data

#Build "dictionary" for age groups within each county (geoid)
df_agg <- dict_merge()

#get single overall hosp rate based on age strata at county level
df_p_hosp <- strat_rates()

#merge hospitalization rates based on geoid into main dataset
dat_2 <- merge(df_p_hosp, dat_, by ="geoid", all = T)


########End Prep data #############

#swap hosp_create_delay_frame out for hosp_create_delay_frame_strat (at least in hospitalizations calculation part, though could use this function all around using cty = F)
hosp_create_delay_frame_strat(X = incidI, p_X =  p_hosp,data_ = dat_2,X_pars = c(1,1),varname = "H", cty = T)


