#### Cleaned up functions for Review ####
######## Jacob Pettit 2020/4/20 #########



#Function to Map age dist data to geoids
#dict = file path to geoid dictionary
#dta = file path to strata information at the county level

dict_merge <- function(dict = "UT_geo_id_dictionary.csv", dta = "cc-est2018-alldata.csv", state = "Utah"){
  require(dplyr)
  
  #read files in
  df <- read.csv(dta)
  df_dict <- read.csv(dict)
  
  #FIlter to state
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

#function to replace hosp_create_delay_frame when calculating hospitalizations wit hstrata specific rates
strata_sampler <- function(X = "incidI", p_vect = p_hosp, dta_i = dat_,
                           X_pars = time_hosp_pars, varname = "H", dta_map = df_map){
  
  
  #check that there is  a mapping for each geoid
  #if( override_warn == F){ #sum(names(dta_map) %in% unique(dta_i$geoid)!= length(unique(dta_i$geoid)),na.rm = T) 
  # print("Mapping data missing for 1 or more geoids")
  
  #}else{
  
  #initialize list to hold 1 df for each geoid (1 row/day, 1 column/strata)
  geo_dfs <- vector(mode = "list",length = length(unique(dta_i$geoid)))
  names(geo_dfs) <- unique(dta_i$geoid)
  
  
  #grab multinomial probabilities for each geoid, utilize them for sampling each day's SEIR output 
  for(i in unique(dta_i$geoid)){
    
    #grab respective probs for each strata
    age_dist = dta_map[[i]] 
    
    #grab only days relative to particular geoid
    dta_strata = dta_i[dta_i$geoid == i,]
    
    #Initialize list with length = to number of days calculated for each geoid
    l_strata <- vector(mode = "list",length = nrow(dta_strata))
    
    #for each day, sample from multinomial distribution of size incident
    for(day in 1:nrow(dta_strata)){
      #print(unlist(dta_strata[day,"incidI"]))
      l_strata[[day]] <- matrix(data = rmultinom(n = 1,size = dta_strata$incidI[day],prob = age_dist),nrow = 1,byrow = T)
      
    }
    
    #save each dataframe to a list, ending up with alist of datafames 
    # w/ one row for each day, one column for each strata and one df for each geoid
    df_col <- data.frame(do.call(rbind, l_strata))
    
    
    df_col <- cbind(df_col, dta_strata %>% select(time,geoid, sim_num, uid))
    
    #store that geoid inlist of geoid specific datafarames
    geo_dfs[[i]] <- df_col
    
  }
  
  #combine all dfs in one, ending with one row/day/geoid and one column for each strata plus one ID column
  #numbers indicate # of infected within each strata on each day
  df_final <- bind_rows(geo_dfs)
  
  
  #initialize a hospitalization dataframe,
  
  df_hosp <- as.data.frame(matrix(nrow = nrow(df_final), ncol = ncol(df_final)))
  names(df_hosp) <- paste(names(df_final),"hosp",sep = "_")
  
  #check to make sure vector of probabilities matches number of strata for each county
  if((ncol(df_final)-4) == length(p_vect)){
    
    for(i in 1:(ncol(df_final)-4)){ #minus 4 keeps process from doing this for non group incid columns
      
      df_hosp[[i]] <- rbinom(n = df_final[[i]],size = df_final[[i]],prob = p_vect[i]) 
      
    }
    
    #write days and geoid columns in again
    df_hosp$time_hosp  <- as.Date(df_final$time)
    df_hosp$geoid_hosp <- df_final$geoid
    df_hosp$sim_num_hosp  <- df_final$sim_num
    df_hosp$uid_hosp  <- df_final$uid
    
    #enter in a daily total column
    df_hosp$total_hosp <- rowSums(df_hosp[1:(ncol(df_hosp)-4)]) #sum strata specific hosp columns
    
  }else{
    print("Vector of strata specific probabilities is not the same length as the number of strata")
  }
  
  #return a list of length 2, one with the total hospitalized, geoid
  rc <- data.table::data.table(
    time = df_hosp$time + round(exp(X_pars[1] + X_pars[2]^2 / 2)),
    uid = df_hosp$uid,
    count = df_hosp$total_hosp
  )
  
  names(rc)[3] <- paste0("incid",varname)
  
  # }
  
  #return a list with 2 objects, this way we can get the overall rates for use in current pipeline
  #but can also expand easier to include other strata specific elements down the road
  return(list(df_agg = rc,df_strat =data.table::as.data.table(df_hosp)))
  
}


#same as original
hosp_create_delay_frame <- function(X, p_X, data_, X_pars, varname) {
  X_ <- rbinom(length(data_[[X]]),data_[[X]],p_X)
  rc <- data.table::data.table(
    time = data_$time + round(exp(X_pars[1] + X_pars[2]^2 / 2)),
    uid = data_$uid,
    count = X_
  )
  names(rc)[3] <- paste0("incid",varname)
  return(rc)
}


#Same as Original
hosp_load_scenario_sim <- function(scenario_dir, sim_id, keep_compartments=NULL,
                                   time_filter_low = -Inf, time_filter_high = Inf) {
  files <- dir(scenario_dir,full.names = TRUE)
  rc <- list()
  i <- sim_id
  file <- files[i]
  if (is.null(keep_compartments)) {
    suppressMessages(tmp <- read_csv(file))
  } else {
    suppressMessages(tmp <- read_csv(file) %>% filter(comp %in% keep_compartments))
  }
  
  tmp <- #tmp[-1,] %>%
    tmp %>%
    filter(time <= time_filter_high & time >= time_filter_low) %>%
    pivot_longer(cols=c(-time, -comp), names_to = "geoid", values_to="N") %>%
    mutate(sim_num = i)
  return(tmp)
}



##' Build a set of sampled hospitalizations, deaths, and recoveries
##' from the incident infection data from the simulation model.
##'
##' @param p_hosp probability of hospitalization, among infections (if length p_hosp>1 then stratified sampling for hosp is used)
##' @param p_death probability of death, among infections (hospitalization is required for death)
##' @param p_ICU probability of needing the ICU among hospitalized patients
##' @param p_vent probability of needing a ventilator among ICU patients
##' @param data_filename Path to the directory that contains the CSV output of the simulation model
##' @param scenario_name The name of the scenario we are analyzing here (e.g., "highdeath", "meddeath", etc.)
##' @param time_hosp_pars parameters for time from onset to hospitalization distribution
##' @param time_ICU_pars parameters for time from hospitalization to ICU
##' @param time_vent_pars parameters for time from ICU to time on ventilator
##' @param time_death_pars parameters for time from hospitalization to death distribution
##' @param time_disch_pars parameters for time from hospitalization to discharge parameters
##' @param time_ICUdur_pars parameetrs for time of ICU duration
##' @param cores The number of CPU cores to run this model on in parallel
##' @param root_out_dir Path to the directory to write the outputs of this analysis
##' @param df_map data frame output from dict_merge; function that links multinomial probs for strata within county
build_hospdeath_par <- function(p_hosp, p_death, p_ICU, p_vent, data_filename, scenario_name,
                                df_map,
                                time_hosp_pars = c(1.23, 0.79), 
                                time_ICU_pars = c(log(10.5), log((10.5-7)/1.35)),
                                time_vent_pars = c(log(10.5), log((10.5-8)/1.35)),
                                time_death_pars = c(log(11.25), log(1.15)), 
                                time_disch_pars = c(log(11.5), log(1.22)),
                                time_ICUdur_pars = c(log(17.46), log(4.044)),
                                cores=8,
                                root_out_dir='hospitalization') {
  
  n_sim <- length(list.files(data_filename))
  print(paste("Creating cluster with",cores,"cores"))
  doParallel::registerDoParallel(cores)
  
  if(n_sim == 0){
    stop("No simulations selected to run")
  }
  
  print(paste("Running over",n_sim,"simulations"))
  
  pkgs <- c("dplyr", "readr", "data.table", "tidyr", "hospitalization")
  foreach::foreach(s=seq_len(n_sim), .packages=pkgs) %dopar% {
    dat_ <- hosp_load_scenario_sim(data_filename,s,keep_compartments = c("diffI","cumI"))
    dat_ <- dat_ %>% dplyr::filter(comp == "diffI") 
    dat_ <- dat_ %>% mutate(hosp_curr = 0, icu_curr = 0, vent_curr = 0, uid = paste0(geoid, "-",sim_num))
    dat_ <- dat_ %>% rename(incidI = N)
    dates_ <- as.Date(dat_$time)
    
    
    if(length(p_hosp)>1 & length(p_hosp) == (nrow(df_map)-1)){
      print("Using strata specific hosp rates")
      
      dat_H_strata <- strata_sampler( X = "incidI", p_vect = p_hosp, dta_i = dat_,
                                      X_pars = time_hosp_pars, varname = "H", dta_map = df_map)
      
      #keep only aggregated rate based on strata sp. rates for now
      dat_H <- dat_H_strata$df_agg
      
    }
    else{
      print("Using single hosp rate")
      dat_H <- hosp_create_delay_frame('incidI',p_hosp,dat_,time_hosp_pars,"H")
      
    }
    
    
    data_ICU <- hosp_create_delay_frame('incidH',p_ICU,dat_H,time_ICU_pars,"ICU")
    data_Vent <- hosp_create_delay_frame('incidICU',p_vent,data_ICU,time_vent_pars,"Vent")
    data_D <- hosp_create_delay_frame('incidH',p_death,dat_H,time_death_pars,"D")
    R_delay_ <- round(exp(time_disch_pars[1]))
    ICU_dur_ <- round(exp(time_ICUdur_pars[1]))
    
    # Using `merge` instead of full_join for performance reasons    
    res <- merge(dat_H %>% mutate(uid = as.character(uid)), 
                 data_ICU %>% mutate(uid = as.character(uid)), all=TRUE)
    res <- merge(res, data_Vent %>% mutate(uid = as.character(uid)), all=TRUE)
    res <- merge(res, data_D %>% mutate(uid = as.character(uid)), all=TRUE)
    res <- merge(dat_ %>% mutate(uid = as.character(uid)), 
                 res %>% mutate(uid = as.character(uid)), all=TRUE)
    
    
    res <- res %>% 
      replace_na(
        list(incidI = 0,
             incidH = 0,
             incidICU = 0,
             incidVent = 0,
             incidD = 0,
             vent_curr = 0,
             hosp_curr = 0))
    
    # get sim nums
    res <- res %>% select(-geoid, -sim_num) %>%
      separate(uid, c("geoid", "sim_num"), sep="-", remove=FALSE)
    
    res <- res %>% mutate(date_inds = as.integer(time - min(time) + 1))
    n_sim <- length(unique(res$sim_num))
    res$sim_num_good <- as.numeric(res$sim_num) 
    res$sim_num_good <- res$sim_num_good - min(res$sim_num_good) +1
    
    res$geo_ind <- as.numeric(as.factor(res$geoid))
    
    res <- res %>%
      arrange(geo_ind, date_inds) %>%
      group_by(geo_ind) %>%
      group_map(function(.x,.y){
        .x$hosp_curr <- cumsum(.x$incidH) - lag(cumsum(.x$incidH),n=R_delay_,default=0)
        .x$icu_curr <- cumsum(.x$incidICU) - lag(cumsum(.x$incidICU),n=ICU_dur_,default=0)
        .x$geo_ind <- .y$geo_ind
        return(.x)
      }) %>%
      do.call(what=rbind) %>%
      arrange(date_inds, geo_ind)
    
    outfile <- paste0(root_out_dir,'/', data_filename,'/',scenario_name,'-',s,'.csv')
    outdir <- gsub('/[^/]*$','',outfile)
    if(!dir.exists(outdir)){
      dir.create(outdir,recursive=TRUE)
    }
    write.csv(res,outfile)
  }
  doParallel::stopImplicitCluster()
}
