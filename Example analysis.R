# Example code for Currie et al, all calculations are contained in a single function
# Input = dummy data provided, specify years and quantiles of interest
# Output = list of two parts:
# [[1]] table of median and 95%CI of contributions to difference in Life expectancy
# [[2]] table of total contribution by cause with 95% CI
# If you combine categories then you get a single dataframe output

library(tidyverse)
library(MortalitySmooth) # this has install issues (removed from CRAN) but can be solved by using below:
# install.packages("https://cran.r-project.org/src/contrib/Archive/svcm/svcm_0.1.2.tar.gz", repos = NULL, type = "source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/MortalitySmooth/MortalitySmooth_2.3.4.tar.gz", repos = NULL, type = "source")


source("Function/decomp_simulation_fn.R")

############################################################
### Example data
### To run full simulations you need a data file in the same format and it loops over the years
### Note the dummy data has some issues which are not present in the full dataset, generally those with very small sample sizes
####################################

mydata <- read_csv("Data/dummy_data_v3.csv")
mydata$Year <- "2002-04" # to make it run with my mega-function which loops over years

# combine male 1st and 5th quantiles
test_out <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                        Quintile_2 = 5)

# age by cause [[1]]
ggplot(test_out[[1]], aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")

sum(test_out[[1]]$Contrib_median) # should be equal to total difference in life expectancy (it is!)

# Combined by cause [[2]]
head(test_out[[2]])

ggplot(test_out[[2]],aes(x=category, y = Contrib_median)) + geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Lower_95, ymax=Upper_95)) + coord_flip()


#### Now to do by age - I think this is what we wanted, (combine_causes = TRUE)
##### note the extra argument in the function.
#### Only has one output not a list of two

age_only_test_out <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                        Quintile_2 = 5, combine_causes = TRUE)

ggplot(age_only_test_out, aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")


#### Now removing age > 75
mydata <- read_csv("dummy_data_v3.csv")
mydata$Year <- "2002-04" # to make it run with my mega-function which loops over years


test_out <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                        Quintile_2 = 5, remove_other = FALSE, remove_ages_over = 75)

# age by cause [[1]]
ggplot(test_out[[1]], aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")

# Combined by cause [[2]]
head(test_out[[2]])

ggplot(test_out[[2]],aes(x=category, y = Contrib_median)) + geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Lower_95, ymax=Upper_95)) + coord_flip()


## Trying to remove 'other deaths' component
mydata <- read_csv("Data/dummy_data_v3.csv")
mydata$Year <- "2002-04" # to make it run with my mega-function which loops over years
mydata <- mydata %>% mutate(category = case_when(category == "Category_I" ~ "Other",
                                                 TRUE ~ as.character(category))) # to make other for example

test_out <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                        Quintile_2 = 5, remove_other = TRUE) #

# age by cause [[1]]
ggplot(test_out[[1]], aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")

head(test_out[[2]])

ggplot(test_out[[2]],aes(x=category, y = Contrib_median)) + geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Lower_95, ymax=Upper_95)) + coord_flip()

test_out <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                        Quintile_2 = 5,  combine_causes = TRUE) #

ggplot(test_out, aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")

test_out2 <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                        Quintile_2 = 5,  combine_causes = FALSE, remove_other = TRUE) #

sum(test_out2[[2]]$Contrib_median)

ggplot(test_out2[[1]], aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")

### It looks OK and i think it works

### BOTH TOGETHER (combine all, remove other, remove over 75)

test_out2 <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                         Quintile_2 = 5,  combine_causes = TRUE, remove_other = TRUE, remove_ages_over = 75) #



ggplot(test_out2, aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")


test_out2 <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                         Quintile_2 = 5,  combine_causes = FALSE, remove_other = TRUE, remove_ages_over = 75) #

ggplot(test_out2[[1]], aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")
