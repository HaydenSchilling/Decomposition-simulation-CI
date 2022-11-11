# functions by Hayden Schilling to calculate life expectancy by simulation

# Function to simulate a number of deaths around a specified mean with Poisson Distribution
death_simulation <- function(data){
  sim_array <- matrix(NA, nrow=nrow(data), ncol = 1000) # make empty table to hold values
  for (i in 1:nrow(data)){
    sim_array[i,] <- rpois(1000,data$Deaths[i])} 
  return(sim_array)
}


# Function to calculate life expectancies
# Taken from Jonny's previous work
life_expect <- function(data, death){
life_table <- data %>%
  mutate(ax #average fraction of interval lived
         = if_else(ageband %in% c("0"), 0.1, 0.5),
         Mx #age-specific death rate
         = death/Population, 
         n #number of years between intervals
         = if_else(ageband <90, dplyr::lead(ageband)-ageband, 2/Mx), 
         qx #proportion dying in interval
         = if_else(ageband <90, (n*Mx)/(1+n*(1-ax)*Mx), 1),
         px #proportion surviving 
         = 1-qx,
         Ix #number alive at age x
         = if_else(ageband %in% c("0"), 100000, 1),
         px_lag = dplyr::lag(px, default = 1),
         Ix = if_else(ageband >0, cumprod(Ix*px_lag), Ix),
         dx #number dying in interval
         = Ix - dplyr::lead(Ix, default = 0),
         Lx #total years lived in interval
         = if_else(ageband <90, n*(dplyr::lead(Ix)+(ax*dx)), Ix/Mx),
         Tx #total years lived beyond age x
         = rev(cumsum(rev(Lx))),
         eX #estimated life expectancy
         = Tx/Ix)
return(life_table)
}

# Function to generate 95% CI around LE estimates using the simulated deaths
life_expect_95CI <- function(data){
  hold_raw_death <- data[[1]]$death # store raw deaths for later
  sim_array2 <- matrix(NA, nrow=nrow(data[[1]]), ncol = 1000) #empty tables for simulation results
  sim_array3 <- matrix(NA, nrow=nrow(data[[1]]), ncol = 1000)
  sim_array4 <- matrix(NA, nrow=nrow(data[[1]]), ncol = 1000)
    for(i in 1:1000){ # loop through each of 1000 simulations
    data[[1]]$death <- data[[2]][,i]
    data[[3]] <- life_expect(data= data[[1]]) # store results of LE calcuations
    #head(data)
    sim_array2[,i] <- data[[3]]$eX # store results from reach simulation
    sim_array3[,i] <- data[[3]]$Ix
    sim_array4[,i] <- data[[3]]$Lx
  }
  # make final results table/variables
  data[[1]]$death <- hold_raw_death
  data[[1]]$Ex_median <- as.numeric(NA)
  data[[1]]$Ex_lower_95_CI <- as.numeric(NA)
  data[[1]]$Ex_upper_95_CI <- as.numeric(NA)
  data[[1]]$Lx_median <- as.numeric(NA)
  #data[[1]]$Lx_lower_95_CI <- as.numeric(NA)
  #data[[1]]$Lx_upper_95_CI <- as.numeric(NA)
  data[[1]]$Ix_median <- as.numeric(NA)
  #data[[1]]$Lx_lower_95_CI <- as.numeric(NA)
  #data[[1]]$Lx_upper_95_CI <- as.numeric(NA)
  #data[[3]] <- NULL
  
  for(i in 1:nrow(data[[1]])){ # for each ageband (row) store the results
    x <- quantile(sim_array2[i,],probs=c(.025,.5,.975))
    y <- median(sim_array3[i,])
    z <- median(sim_array4[i,])
    data[[1]]$Ex_median[i] <- x[2]
    data[[1]]$Ex_lower_95_CI[i] <- x[1]
    data[[1]]$Ex_upper_95_CI[i] <- x[3]
    data[[1]]$Ix_median[i] <- y
    data[[1]]$Lx_median[i] <- z
  }
  data[[4]] <- sim_array2 #Ex
  data[[5]] <- sim_array3 #Ix
  data[[6]] <- sim_array4 #Lx
  return(data)
}


### Function from Jonny's previous code
CalculateDiffTable <- function(lt1, lt2) {
  # If rates and sexes are provided, then the life table is calculated using
  # these inputs. Otherwise, the life tables of the two populations, lt1 and lt2, 
  # must be provided. It returns a merged life table for the two populations, 
  # used to calculate the contribution of each age group towards the change in 
  # life expectancy in CalculateAgeContribution()
  stopifnot(lt1$x == lt2$x)
  data.frame(
    age = lt1$x,
    lx1 = (lt1$Ix)/100000,
    Lx1 = (lt1$Lx)/100000, 
    lx2 = (lt2$Ix)/100000,
    Lx2 = (lt2$Lx)/100000, 
    ex1 = lt1$Ex, #Ex
    ex2 = lt2$Ex)#, #Ex
  #Year = lt1$Year) # removed year for now, could be put back but depends on later stuff
}


### Function to do error bars on decomposition

age_decomp_95_CI <- function(data1, data2){ # takes two dataframes
  end_list <- list() # to hold results
  data1[[1]] <- data1[[1]] %>% rename(x = ageband) # rename a variable
  data2[[1]] <- data2[[1]] %>% rename(x = ageband)
  
# prepare some matrices to hold the simulations
sim_array2 <- matrix(NA, nrow=nrow(data1[[1]]), ncol = 1000)
sim_array3 <- matrix(NA, nrow=nrow(data1[[1]]), ncol = 1000)
sim_array4 <- matrix(NA, nrow=nrow(data1[[1]]), ncol = 1000)

# cycle through the simulations
for (i in (1:ncol(data1[[4]]))){
  
  t_1 <- tibble(x=data1[[1]]$x, Ix=data1[[5]][,i], Lx=data1[[6]][,i],Ex=data1[[4]][,i])
  t_2 <- tibble(x=data2[[1]]$x, Ix=data2[[5]][,i], Lx=data2[[6]][,i],Ex=data2[[4]][,i])
  
  # Jonny's function
  lt_agedecomp_m <- CalculateDiffTable(lt1 = t_1, 
                                       lt2 = t_2)
  
  lt_agedecomp_m <- lt_agedecomp_m %>%
    #group_by(Year) %>%
    mutate(de #direct effect
           = lx1*((Lx2/lx2)-(Lx1/lx1)),
           ie #indirect effect 
           = (lx1*dplyr::lead(lx2)/lx2-dplyr::lead(lx1))*dplyr::lead(ex2),
           #There are no age categories beyond 90+ so indirect effect =0
           ie=if_else(age %in% c(90), 0, ie),
           te #total effect
           = de+ie
    )
  # store the simulations
  sim_array2[,i] <- lt_agedecomp_m$de
  sim_array3[,i] <- lt_agedecomp_m$ie
  sim_array4[,i] <- lt_agedecomp_m$te
}

# prepare output table
age_decomposed <- tibble(x=data1[[1]]$x, DE_median=NA, DE_lower_95_CI=NA, DE_upper_95_CI=NA,
                         IE_median=NA, IE_lower_95_CI=NA, IE_upper_95_CI=NA,
                         TE_median=NA, TE_lower_95_CI=NA, TE_upper_95_CI=NA)

# calculate stats for each age
for(i in 1:nrow(data1[[1]])){
  x <- quantile(sim_array2[i,],probs=c(.025,.5,.975), na.rm = T)
  y <- quantile(sim_array3[i,],probs=c(.025,.5,.975), na.rm = T)
  z <- quantile(sim_array4[i,],probs=c(.025,.5,.975), na.rm = T)
  age_decomposed$DE_median[i] <- x[2]
  age_decomposed$DE_lower_95_CI[i] <- x[1]
  age_decomposed$DE_upper_95_CI[i] <- x[3]
  age_decomposed$IE_median[i] <- y[2]
  age_decomposed$IE_lower_95_CI[i] <- y[1]
  age_decomposed$IE_upper_95_CI[i] <- y[3]
  age_decomposed$TE_median[i] <- z[2]
  age_decomposed$TE_lower_95_CI[i] <- z[1]
  age_decomposed$TE_upper_95_CI[i] <- z[3]
}

end_list <- list(age_decomposed,  sim_array2,  sim_array3,  sim_array4) # summary, DE , IE, TE

return(end_list)
}


#################################################################################
#### Final Function code
#################################################################################

age_cause_contrib_diff_95CI <- function(data, Sex_1, Quintile_1, 
                                        Quintile_2, combine_causes = FALSE,
                                        remove_other = FALSE, remove_ages_over = 1000){
set.seed(123)
mydata <- select(data, -X1) # drop unwanted column, this could probably be coded better, this column is probably an artefact of an export including row names previously

  if(combine_causes == TRUE){
    if(remove_other == TRUE){
      mydataOO <- mydata %>% filter(category != "Other") %>% group_by(Sex, ageband, Wales_Quintile_2019,popn,Year) %>%
        summarise(random=sum(random, na.rm=T)) %>% mutate(category = "Combined") %>% ungroup
      mydataO <- mydata %>% filter(category == "Other") %>% group_by(Sex, ageband, Wales_Quintile_2019,popn,Year) %>%
        summarise(random=sum(random, na.rm=T)) %>% mutate(category = "Other") %>% ungroup
      mydata <- bind_rows(mydataOO, mydataO)
    }
    else{
      mydata <- mydata %>% group_by(Sex, ageband, Wales_Quintile_2019,popn,Year) %>%
    summarise(random=sum(random, na.rm=T)) %>% mutate(category = "Combined") %>% ungroup
  #(print("Combining Causes"))
  }}


# select only one combination
years <- unique(mydata$Year)
final_table2 <- data.frame() # initialise final table
final_table2c <- data.frame() # initialise final table for cause total
# initialise progress bar based upon years
pb <- txtProgressBar(min = 0, max = length(years), style = 3)

for (y in 1:length(years)){ # loop through each Year
  # select the data
mydataT <- mydata %>% filter(Year == years[y] & Wales_Quintile_2019 == Quintile_1 & Sex == Sex_1)

# now generate simulated deaths
cause_list <- unique(mydataT$category) # make list of different category/causes of death
fitted_right = rep("NO", length(cause_list)) # to assess the fit later, start with no then change to yes if fit works
cause_specific_deaths <- list()

for(i in (1:length(cause_list))){ # loop for each cause
  tryCatch({ #some categories can't be smoothed
  # separate out age 0 from rest (no smoothing of age zero based upon Tim Riffe Advice)
  causesA <- filter(mydataT, category==cause_list[i])
  causes_age_1 <- filter(causesA, ageband == 0)
  causesA <- filter(causesA, ageband > 0)
  
  # smooth mortality rate
  fit1D_test_A <- Mort1Dsmooth(x = causesA$ageband, y = causesA$random, offset = log(causesA$popn))
  #plot(fit1D_test_A)
  
  #fit1D_test_A$logmortality # log mortality rate
  causesA$fitted_mortality <- exp(fit1D_test_A$logmortality[,1]) # convert back to actual mortality rate
  #head(causesA)
  
  test_dat <- causesA
  
  # Put age 1 back into dataframe
  test_dat <- bind_rows(causes_age_1,causesA)
  #head(test_dat)
  # fill in the age 0 fitted mortality with the observed mortality
  test_dat$fitted_mortality[1] <- causes_age_1$random[1]/causes_age_1$popn[1]
  #head(test_dat)
  # calculate fitted deaths based upon smoothing, to use in Poisson simulations
  test_dat$fitted_death <- test_dat$fitted_mortality * test_dat$popn
  #test_dat$fitted_death <- test_dat$random # add to remove smoothing effect as a test
  #head(test_dat)
  
  cause_specific_deaths[[i]] <- test_dat # store the cause specific simulated deaths
  fitted_right[i] <- "YES" # a marker for doing it correct
  }, # trycatch
  error=function(cond){# trycatch to use observed data if smoothing fails
    print(paste0(cause_list[i]," for ", years[y]," Quintile ",Quintile_1, " does not work"))# trycatch
    #causesA <- filter(mydataT, category==cause_list[i])
    #causesA$fitted_mortality <- causesA$random/causesA$popn
    #test_dat <- causesA
    #test_dat$fitted_death <- test_dat$random
    #cause_specific_deaths[[i]] <- test_dat
    #return(test_dat)
    })# trycatch
  if (fitted_right[i] == "NO"){
    causesA <- filter(mydataT, category==cause_list[i])
    causesA$fitted_mortality <- causesA$random/causesA$popn
    test_dat <- causesA
    test_dat$fitted_death <- test_dat$random
    cause_specific_deaths[[i]] <- test_dat
    fitted_right[i] <- "NO but Fixed"
  }
}

# now have smoothed amounts of deaths for each age/quintile/cause
full_dat <- bind_rows(cause_specific_deaths)

# now sum together to get all causes combined smoothed deaths
summary_dat <- full_dat %>% group_by(ageband) %>% summarise(total_fit_mort = sum(fitted_mortality),
                                                            total_fit_death = sum(fitted_death))

#head(summary_dat)
# make a dataframe holding the population numbers and age
causes_pop <- mydataT %>% select(popn, ageband)
#head(causes_pop)

# Join above to summary_dat
summary_dat <- left_join(summary_dat, causes_pop %>% unique()) %>% arrange(ageband) # building new df (equivalent to test dat above i think)


### now have fitted mortality and deaths for both cause specific and combined causes
# use the smoothed mortality rate the simulate new numbers of deaths 1000 times
test_dat2 <- full_dat %>% rename(Deaths = fitted_death, Population = popn)
#print(test_dat2)
simulated_deaths_per_cause <- list() # empty list to store results
for(i in (1:length(cause_list))){ # loop through the causes
  death_sim_dat <- filter(test_dat2, category==cause_list[i])
  #head(test_dat2)
  simulated_deaths_per_cause[[i]] <- as.data.frame(death_simulation(data=death_sim_dat)) # this does the death simulation based upon function at top of file
  category <- data.frame(category=rep(cause_list[i],length(death_sim_dat$ageband)))
  ages <- data.frame(ageband=death_sim_dat$ageband)
  simulated_deaths_per_cause[[i]] <- bind_cols(simulated_deaths_per_cause[[i]], category, ages) # store the simulated deaths with category and age labels as part of the list
  #head(mydata2[[1]])
}

#view(simulated_deaths_per_cause[[1]]) # each cause of death has 1000 simulations around the fitted poisson mean
# now calculate total deaths per replicate simulation
simulated_deaths_per_cause_dataframe <- bind_rows(simulated_deaths_per_cause) # list to dataframe
simulated_total_deaths_by_age <- simulated_deaths_per_cause_dataframe %>% group_by(ageband) %>%
  summarise(across(where(is.integer),sum))
#head(simulated_total_deaths_by_age)

###################################################################################
### now need to get life expectancies
###################################################################################
#need the following structure
# [[2]] is the death simulations
# [[1]] is the dataframe, needs:
# death
# population
# n
# ax # how far through the age interval most people die, default = halfway = 0.5
## Based on functions at top of file stemming from Jonny's previous work

# data prep
summary_dat <- summary_dat %>% rename(death = total_fit_death, Population = popn, Mortality_median=total_fit_mort)
#head(summary_dat)

# prepare data for function
test2 <- list(summary_dat, as.matrix(simulated_total_deaths_by_age[,2:1001]))
#head(test2[[1]])
#view(test2[[2]])

test_95 <- life_expect_95CI(data=test2) 
#str(test_95) # [[1]] is the final results
#view(test_95[[1]])
#ggplot(test_95[[1]], aes(x= ageband, y=Ex_median)) + geom_point() +
#  #geom_point(aes(y=log(Mortality_median)), colour = "blue") +
#  #geom_point(aes(y=log(fitted_mortality)), colour="green") +
#  geom_errorbar(aes(ymin=Ex_lower_95_CI, ymax=Ex_upper_95_CI), colour="blue")


######################################################################################


### Now load males (essentially a repeat of the females above)
# Can maybe replace entire section with loop over sex (? - not really needed at the moment)
# See comments in above section as less here 

mydata <- select(data, -X1) # drop unwanted column

if(combine_causes == TRUE){
  if(remove_other == TRUE){
    mydata00 <- mydata %>% filter(category != "Other") %>% group_by(Sex, ageband, Wales_Quintile_2019,popn,Year) %>%
      summarise(random=sum(random, na.rm=T)) %>% mutate(category = "Combined") %>% ungroup
    mydataO <- mydata %>% filter(category == "Other") %>% group_by(Sex, ageband, Wales_Quintile_2019,popn,Year) %>%
      summarise(random=sum(random, na.rm=T)) %>% mutate(category = "Other") %>% ungroup
    mydata <- bind_rows(mydata00, mydataO)
  }
  else{
    mydata <- mydata %>% group_by(Sex, ageband, Wales_Quintile_2019,popn,Year) %>%
      summarise(random=sum(random, na.rm=T)) %>% mutate(category = "Combined") %>% ungroup
    #(print("Combining Causes"))
  }}

# select only one combination

mydataTM <- mydata %>% filter(Year == years[y] & Wales_Quintile_2019 == Quintile_2 & Sex == Sex_1)

# now generate simulated deaths

cause_listM <- unique(mydataTM$category)
fitted_rightM <- rep("NO", length(cause_listM))
cause_specific_deathsM <- list()

for(i in (1:length(cause_listM))){
  tryCatch({
  causesA <- filter(mydataTM, category==cause_listM[i])
  causes_age_1 <- filter(causesA, ageband == 0)
  causesA <- filter(causesA, ageband > 0)
  
  
  fit1D_test_AM <- Mort1Dsmooth(x = causesA$ageband, y = causesA$random, offset = log(causesA$popn))
#  plot(fit1D_test_AM)
  
  causesA$fitted_mortality <- exp(fit1D_test_AM$logmortality[,1]) # convert back to actual mortality rate
#  head(causesA)
  
  test_dat <- causesA
  
  # if separating age 1 from smoothing do below
  test_dat <- bind_rows(causes_age_1,causesA)
  #head(test_dat)
  
  test_dat$fitted_mortality[1] <- causes_age_1$random[1]/causes_age_1$popn
  #head(test_dat)
  
  test_dat$fitted_death <- test_dat$fitted_mortality * test_dat$popn
  #test_dat$fitted_death <- test_dat$random # add to remove smoothing effect
  #  head(test_dat)
  
  cause_specific_deathsM[[i]] <- test_dat
  fitted_rightM[i] <- "Yes"
  }, # trycatch
error=function(cond){# trycatch
  print(paste0(cause_list[i]," for ", years[y]," Quintile ",Quintile_2, " does not work"))# trycatch
  #causesA <- filter(mydataTM, category==cause_listM[i])
  #causesA$fitted_mortality <- causesA$random/causesA$popn
  #test_dat <- causesA
  #test_dat$fitted_death <- test_dat$random
  })# trycatch
  if (fitted_rightM[i] == "NO"){
    causesA <- filter(mydataTM, category==cause_listM[i])
    causesA$fitted_mortality <- causesA$random/causesA$popn
    test_dat <- causesA
    test_dat$fitted_death <- test_dat$random
    cause_specific_deathsM[[i]] <- test_dat
    fitted_rightM[i] <- "NO but Fixed"
  }
}

# now have smoothed amounts of deaths for each age/quintile/cause
full_datM <- bind_rows(cause_specific_deathsM)
#table(full_datM$category)
# now sum together to get all causes combined smoothed deaths
summary_datM <- full_datM %>% group_by(ageband) %>% summarise(total_fit_mort = sum(fitted_mortality),
                                                              total_fit_death = sum(fitted_death))

#head(summary_datM)
causes_popM <- mydataTM %>% select(popn, ageband)
#head(causes_popM)

summary_datM <- left_join(summary_datM, causes_popM %>% unique()) %>% arrange(ageband) # building new df (equivalent to test dat above i think)


# use the smoothed mortality rate the simulate new numbers of deaths 1000 times per cause
test_dat2M <- full_datM %>% rename(Deaths = fitted_death, Population = popn)

simulated_deaths_per_causeM <- list()
for(i in (1:length(cause_list))){
  death_sim_dat <- filter(test_dat2M, category==cause_listM[i])
  #head(test_dat2)
  simulated_deaths_per_causeM[[i]] <- as.data.frame(death_simulation(data=death_sim_dat)) # this includes a mortality rate calculation and can probably be changed.
  category <- data.frame(category=rep(cause_list[i],19))
  ages <- data.frame(ageband=death_sim_dat$ageband)
  simulated_deaths_per_causeM[[i]] <- bind_cols(simulated_deaths_per_causeM[[i]], category, ages)
  #head(mydata2[[1]])
}

#view(simulated_deaths_per_cause[[1]]) # each cause of death has 1000 simulations around the fitted poisson mean
# now calculate total deaths per replicate simulation
simulated_deaths_per_cause_dataframeM <- bind_rows(simulated_deaths_per_causeM)
simulated_total_deaths_by_ageM <- simulated_deaths_per_cause_dataframeM %>% group_by(ageband) %>%
  summarise(across(where(is.integer),sum))
#head(simulated_total_deaths_by_ageM)


###################################################################################
### now need to get life expectancies
###################################################################################
#need the following structure
# [[2]] is the death simulations
# [[1]] is the dataframe, needs:
# death
# population
# n
# ax # how far through the age interval most people die, default = halfway = 0.5

#summary_datM$n <- 5
#summary_datM$ax <- 0.5
#summary_datM$ax[1] <- 0.1

#head(summary_datM)



summary_datM <- summary_datM %>% rename(death = total_fit_death, Population = popn, Mortality_median=total_fit_mort)
#head(summary_datM)

#test <- life_expect(data=test2[[1]])
#head(test)
test2M <- list(summary_datM, as.matrix(simulated_total_deaths_by_ageM[,2:1001]))
#head(test2[[1]])
#view(test2[[1]])

test_95M <- life_expect_95CI(data=test2M) 

##############################################################################
##### now for decomposition
##############################################################################

### Combine sexes (test_95M, test_95) and do decomp
### Using Jonny's previous function (slightly modified) inside a new loop

X_test <- age_decomp_95_CI(test_95M, test_95) # this produces estimates of the direct and indirect effects for each age

#view(X_test[[1]])

#str(X_test)
# X_test[[4]] is the TE effect raw simulations which we use in calculations later.

# visualise, how the DE changes over the ages
#ggplot(X_test[[1]], aes(x=x, y = TE_median)) + geom_point() + xlab("Age")+
#  ylab("Direct Effect (+/- 95% CI)")+
#  geom_errorbar(aes(ymin=TE_lower_95_CI, ymax=TE_upper_95_CI))

### OK now it has DE and TE 

### proportion contribution is simply the proportion of deaths from each cause

# OK so for an example, for a certain age say 0, TE = X, and each cause will contribute X * death_by_cause/total_deaths

# calculate total death rates for each simuation
simulated_total_death_rate_by_age_M <- simulated_total_deaths_by_ageM[,2:1001]/summary_datM$Population
simulated_total_death_rate_by_age_F <- simulated_total_deaths_by_age[,2:1001]/summary_dat$Population

# get difference in total death rates
difference_simulated_death_rates <- simulated_total_death_rate_by_age_M - simulated_total_death_rate_by_age_F
difference_simulated_death_rates$ageband <- simulated_total_deaths_by_ageM$ageband #add ageband labels
####
### Now loop over the causes to get cause contributions
#####
final_table <- data.frame() 
new_tableC_all <- data.frame()

for(c in 1:length(cause_list)){
  # 1st bit as above
  simulated_cause_rates_2 <- simulated_deaths_per_causeM[[c]][,1:1000]/summary_datM$Population
  simulated_cause_rates_1 <- simulated_deaths_per_cause[[c]][,1:1000]/summary_dat$Population
  diference_simulated_cause_deaths <- simulated_cause_rates_2-simulated_cause_rates_1
  diference_simulated_cause_deaths$ageband <- simulated_total_deaths_by_ageM$ageband
  
  # how much does each cause contribute in each simuation
  diff_contrib_cause <- diference_simulated_cause_deaths[,1:1000]/difference_simulated_death_rates[,1:1000] # needs to be divide by difference in all cause mortality rate
  cause_cont_TE <- diff_contrib_cause*X_test[[4]] # This is the key for the aggregating up. By cause should be sum of this
  
  # Aggregate by cause
  new_tableC <- data.frame("category"=NA, "Contrib_median"=NA,
                           "Upper_95" = NA, "Lower_95"=NA, "Year" = years[y])
  
  ### The next two lines remove ages older than specified from the combined cause totals
  holding_var <- nrow(summary_dat %>% filter(ageband <= remove_ages_over))
  cause_cont_TE <- cause_cont_TE[1:holding_var,]
  
  total_TE_by_cause = base::colSums(cause_cont_TE)
  CT <- quantile(total_TE_by_cause, probs=c(.025,.5,.975), na.rm = T)
  new_tableC$Contrib_median[1] <- CT[2]
  new_tableC$Upper_95[1] <- CT[3]
  new_tableC$Lower_95[1] <- CT[1]  
  new_tableC$category[1] = cause_list[c]
  
  # make output table
  new_table <- data.frame("ageband"=simulated_total_deaths_by_ageM$ageband, "category"=NA, "Contrib_median"=NA,
                          "Upper_95" = NA, "Lower_95"=NA, "Year" = years[y])
  new_table <- new_table[1:holding_var,]
  for (i in 1:nrow(new_table)){ # for each ageband get the results out
    x <- quantile(cause_cont_TE[i,],probs=c(.025,.5,.975), na.rm = T)
    new_table$Contrib_median[i] <- x[2]
    new_table$Upper_95[i] <- x[3]
    new_table$Lower_95[i] <- x[1]  
    new_table$category = cause_list[c]
  }
  final_table <- bind_rows(final_table, new_table) # bind table together as it's build in the loop
  new_tableC_all <- bind_rows(new_tableC_all, new_tableC) # bind table of aggregated causes
}
final_table2 <- bind_rows(final_table2, final_table) # bind table together as it's build in the loop
final_table2c <- bind_rows(final_table2c, new_tableC_all)
setTxtProgressBar(pb, y) # update progress bar
}

if(combine_causes == TRUE){
  if (remove_other == TRUE){
    final_table2 <- final_table2 %>% filter(category != "Other")
    }
  return(final_table2)
} else{
  if (remove_other == TRUE){
    final_table2 <- final_table2 %>% filter(category != "Other")
    final_table2c <- final_table2c %>% filter(category != "Other")
  }
  output <- list(final_table2, final_table2c)
  return (output)
}

}
