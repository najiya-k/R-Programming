
#Read the data

data<- read.csv("clipboard",sep="\t",header= TRUE)

#data <- data %>% rename(CUST_Id = install.packages.dplyr.)

#Display the first few lines of the dataset

head(data)

#Display the column names of the dataset

col_names <- names(data)
print(col_names)


#Create a new column for date, month, year

library(dplyr)

data1 <- data %>% rename (date_string = MONTH) %>%
  mutate(year = as.integer(substr(date_string, 1,4)),
         month = as.integer(substr(date_string, 5,6)),
         date = as.Date(paste(year, month, "01", sep = '-')))


# 1. How many mortgages per month were funded from 2015-11 to 2016-10 ?

# Group the data by month and count the number of rows in each group
result <- data1 %>%
  group_by(year, month) %>%
  summarise(number_of_mortgages = n())


print(result)

# 2. How many mortgages are assigned to Advisor FA500_ON in 201604?

filtered_data <- data1 %>% 
  filter(Finadvisor == "FA500_ON", date_string == "201604") %>%
  summarise(total_mortgages = n())

print(paste("Mortgages assigned to FA500_ON IN 201604 is", filtered_data))



# 3. How many new mortgages in segment “4-Accumulators[45-54]” only in 2016

filtered_data2 <- data1 %>%
  filter(age_segment == "4-Accumulators[45-54]", year == 2016)

new_mortgages_2016 <- filtered_data2 %>%
  group_by(date) %>%
  summarise(new_mortgages = n_distinct(paste(Finadvisor, age_segment)))
  

print(new_mortgages_2016)
      
     


# 4. Build one or more queries to retrieve all mortgage clients grouped by region

# In order to access region, we need to merge data sets

data_fin <- read.csv("clipboard",sep="\t",header= TRUE)
data_region <- read.csv("clipboard",sep="\t",header= TRUE)
data3 <- left_join(data_fin, data_region, by ="Province")

merged_data <- left_join(data1, data3, by = c("Finadvisor"= "FinAdvID"))


# a. Number of mortgage clients for each region

merged_data <- merged_data %>% rename(CUST_Id = install.packages.dplyr.)

mortgage_clients <- merged_data %>%
  group_by(Region) %>%
  summarise (total_mortgage_clients =n())

print(mortgage_clients)

# b. Dollar value for the mortgages for each region

dollarval_of_mortgage <- merged_data %>%
  group_by(Region) %>%
  summarise(total_mortgage_region = sum(AMT_OF_LOAN))

install.packages("ggplot2")
library(scales)

dollarval_of_mortgage <- dollarval_of_mortgage %>%
  mutate(dollar_total_mortgage_region = dollar_format()(total_mortgage_region)) %>%
  select(Region, dollar_total_mortgage_region)


print(dollarval_of_mortgage)

# c. Number of Advisors under each region
# d. Number of delinquent mortgages for each region
# e. Number of credit card delinquent clients for each region

print(names(data3))
print(names(data1))
