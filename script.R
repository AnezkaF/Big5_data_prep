##################################################################
# BIG 5 Dataset preparation for Correlation matrix tableau report
#################################################################

# Setting stage
#################

# Load libraries

library(dplyr)
library(countrycode)
library(tidyr)


# Data quality checks & reformatting
#####################################

# Read dataset (csv file with sep = "\t")

data <- read.delim("./data/data.csv", sep = "")

# Explore data 

View(data)
str(data)
summary(data)

#Compare allowed values from the data codebook for fields (race, age, engnat, gender, hand, source, country)
# as well as replace codes to values for better visual representation

# race - itegers 0-13: correct
table(data$race, useNA = "ifany")

# replace codes
races <- c(NA,"Mixed Race", "Arctic", "Caucasian (European)",
           "Caucasian (Indian)","Caucasian (Middle East)",
           "Caucasian (North African, Other)", "Indigenous Australian",
           "Native American", "North East Asian","Pacific", "South East Asian",
           "West African", "Other")

for (i in 0:13){
  
    data$race[data$race == i] <- races[i+1]
  
}

# age: 
table(data$age, useNA = "ifany")

# any observations >80 will be set to NA 
data$age[data$age >= 80] <- NA

# age into buckets

b <- c(10,15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 80)
data$age_group <- cut(data$age, breaks = b)

table(data$age_group, useNA = "ifany")

# engnat - itegers 0-2: correct
table(data$engnat, useNA = "ifany")

# replace codes
data$engnat[data$engnat == 0] <- NA
data$engnat[data$engnat == 1] <- "Native English"
data$engnat[data$engnat == 2] <- "Non-native English"

# gender - itegers 0-3: correct
table(data$gender, useNA = "ifany")

# replace codes
data$gender[data$gender == 0] <- NA
data$gender[data$gender == 1] <- "Male"
data$gender[data$gender == 2] <- "Female"
data$gender[data$gender == 3] <- "Other"

# hand - itegers 0-3: correct
table(data$hand, useNA = "ifany")

data$hand[data$hand == 0] <- NA
data$hand[data$hand == 1] <- "Right"
data$hand[data$hand == 2] <- "Left"
data$hand[data$hand == 3] <- "Both"

# source - 1-5: correct
table(data$source, useNA = "ifany")

data$source[data$source == 1] <- "Test website"
data$source[data$source == 2] <- "Google"
data$source[data$source == 3] <- "Facebook"
data$source[data$source == 4] <- ".edu website"
data$source[data$source == 5] <- "Other"

# country
table(data$country, useNA = "ifany")

# (nu and other irregular values to NA

data$country[data$country == "(nu"] <- NA
data$country[data$country == "4"] <- NA
data$country[data$country == "A1"] <- NA
data$country[data$country == "A2"] <- NA
data$country[data$country == "AP"] <- NA
data$country[data$country == "A2"] <- NA

# add regions

data$region <- countrycode(sourcevar = data$country,origin = "iso2c", destination = "region")


# Order the data frame

data <- data %>%
  select(race, age, age_group, engnat, gender, hand, source, country, region, everything())


# Check question responses columns for allowed values

unique(as.vector(as.matrix(data[,10:59]))) # check for unique values

# Replace 0 with NAs
data[,10:59][data[,10:59]==0]<-NA

# Add User ID


# Final formatting and Export data to csv
#############################

# Add User ID

data$user_id <- rownames(data)

# Order the data frame

data <- data %>%
  select(user_id, race, age, age_group, engnat, gender, hand, source, country, region, everything())

# wide to long format (to be able to create the corrmat in tableau)

data_long <- pivot_longer(data, cols = 11:60, names_to = "question_code", values_to = "question_score")

# add column with question wording for better interpretation

question_wording <- read.csv("data/question_wordings.csv")

data_long <- left_join(data_long,question_wording, by = "question_code")

# Order the data frame
data_long <- data_long %>%
  select(user_id, race, age, age_group, engnat, gender, hand, source, country, region, question_code, question_wording, question_score)

# Export the data
write.csv(data_long, file = "./output/big5data_for_tableau.csv", row.names = FALSE)




