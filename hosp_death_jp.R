
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
strat_rates <- function( dta = df_agg,  num_obs= 100 , s_size = 1000,
                         age_rates = c(0,0,.0004,.0004, .0104, .0104, .0343, .0343, .0425, .0425, .0816, .0816, .118, .118, .166, .166,.184,.184)){
  
  
  #initialize dataframe to hold overall proportions
  df_prop <- data.frame(geoid = names(df_agg[-1]),p_hosp=NA)
  
  for(i in 1:(ncol(dta)-1)){
    age_dist = unclass(unlist(dta[,i]))
    
    #generate age buckets
    draws <- rmultinom(n = num_obs, size = s_size,prob = age_dist)
    
    
    df_success <- data.frame(g_1 = rbinom(n = draws[1,],size = draws[1,],prob = age_rates[1]),
                             g_2 = rbinom(n = draws[2,],size = draws[2,],prob = age_rates[2]),
                             g_3 = rbinom(n = draws[3,],size = draws[3,],prob = age_rates[3]),
                             g_4 = rbinom(n = draws[4,],size = draws[4,],prob = age_rates[4]),
                             g_5 = rbinom(n = draws[5,],size = draws[5,],prob = age_rates[5]),
                             g_6 = rbinom(n = draws[6,],size = draws[6,],prob = age_rates[6]),
                             g_7 = rbinom(n = draws[7,],size = draws[7,],prob = age_rates[7]),
                             g_8 = rbinom(n = draws[8,],size = draws[8,],prob = age_rates[8]),
                             g_9 = rbinom(n = draws[9,],size = draws[9,],prob = age_rates[9]),
                             g_10 = rbinom(n = draws[10,],size = draws[10,],prob = age_rates[10]),
                             g_11 = rbinom(n = draws[11,],size = draws[11,],prob = age_rates[11]),
                             g_12 = rbinom(n = draws[12,],size = draws[12,],prob = age_rates[12]),
                             g_13 = rbinom(n = draws[13,],size = draws[13,],prob = age_rates[13]),
                             g_14 = rbinom(n = draws[14,],size = draws[14,],prob = age_rates[14]),
                             g_15 = rbinom(n = draws[15,],size = draws[15,],prob = age_rates[15]),
                             g_16 = rbinom(n = draws[16,],size = draws[16,],prob = age_rates[16]),
                             g_17 = rbinom(n = draws[17,],size = draws[17,],prob = age_rates[17]),
                             g_18 = rbinom(n = draws[18,],size = draws[18,],prob = age_rates[18]))
    
    #Get overall hospitalization rate
    #print(sum(df_success)/sum(draws))
    df_prop$p_hosp[i] <-  sum(df_success)/sum(draws)
    
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


