# troublshooting code for Jonny, all work contained in a single function
# Input = dummy data provided, specify years and quantiles of interest
# Output = list of two parts:
# [[1]] table of median and 95%CI of contributions to difference in Life expectancy
# [[2]] table of total contribution by cause with 95% CI

library(tidyverse)
library(MortalitySmooth) # this has install issues (removed from CRAN) but can be solved by using below:
# install.packages("https://cran.r-project.org/src/contrib/Archive/svcm/svcm_0.1.2.tar.gz", repos = NULL, type = "source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/MortalitySmooth/MortalitySmooth_2.3.4.tar.gz", repos = NULL, type = "source")


source("Life_expect_funs_trycatch_all_years_15_9_2021.R")

mydata <- read_csv("New Data/sample_data.csv") %>% rename(X1 = `...1`) # the rename bit might not be needed but seems to be needed with R v4

test_out <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                        Quintile_2 = 5)
head(test_out[[1]])
head(test_out[[2]])

combined_causes_out <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                        Quintile_2 = 5, combine_causes = TRUE)

head(combined_causes_out)

# Remove the other from the combined cause estimates
combined_causes_out_no_other <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                                   Quintile_2 = 5, combine_causes = TRUE, remove_other = TRUE)

head(combined_causes_out_no_other)


# Remove the other from the combined cause estimates and remove older than 75
combined_causes_out_no_other_75_or_less <- age_cause_contrib_diff_95CI(data = mydata, Sex_1 = "Male", Quintile_1 = 1,
                                                            Quintile_2 = 5, combine_causes = TRUE,
                                                            remove_other = TRUE, remove_ages_over = 75)

head(combined_causes_out_no_other_75_or_less)


#test_out2 <- test_out[[1]] %>% filter(Year == "2002-04"|Year == "2016-18")
#test_out2b <- test_out[[2]] %>% filter(Year == "2002-04"|Year == "2016-18")

### A test plot works if the above runs

#ggplot(test_out2, aes(x=ageband, y = Contrib_median, col=Year)) + geom_point()+
#  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
#  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")

# check total
#testX <- test_out %>% group_by(Year) %>% summarise(LE_diff = sum(Contrib_median))
#testX # this is dummy data so will be ~0 i think

## need to zoom in to see this properly
#ggplot(test_out2b,aes(x=category, y = Contrib_median, col=Year, fill=Year)) + 
#  geom_bar(stat="identity", alpha=0.5)+
#  geom_errorbar(aes(ymin=Lower_95, ymax=Upper_95)) + coord_flip()


# check sums on the de-identied real data to see if it looks right
## Note the function returns a list of two parts,
### [[1]] is the age by cause contribution
### [[2]] is the combined contribtion by cause (summed over the ages)


############################################################
### BELOW IS JUST SMALLER SCALE TESTING

####################################

mydata <- read_csv("dummy_data_v3.csv")
mydata$Year <- "2002-04" # to make it run with my mega-function which loops over years


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


#### Now to do by age - I think this is what we wanted, 
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


## Trying to remove other component
mydata <- read_csv("dummy_data_v3.csv")
mydata$Year <- "2002-04" # to make it run with my mega-function which loops over years
mydata <- mydata %>% mutate(category = case_when(category == "Category_I" ~ "Other",
                                                 TRUE ~ as.character(category)))

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

ggplot(test_out2, aes(x=ageband, y = Contrib_median)) + geom_point()+
  geom_errorbar(aes(ymax=Upper_95, ymin=Lower_95)) + ylab("Median + 95% CI") +
  facet_wrap(~category, scales = "free_y") + geom_hline(aes(yintercept=0), col="red")

### It looks OK and i think it works

### BOTH TOGETHER

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
