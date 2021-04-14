#### BRFSS 2019 Data: Choropleath Map Data

##Changing the working directory:

setwd("~/school/DataVisualization_CAP5738/Project")



### Import the libraries:

library(SASxport)
library(Hmisc)
library(devtools)
library(readxl)        # for importing an excel file
library(xlsx)          # For Reading and Writing Excel files
library(dplyr)         # for Data Manipulation
library(readr)         #The goal of 'readr' is to provide a fast and friendly way to read rectangular data (like 'csv', 'tsv', and 'fwf')
library(lubridate)     # For date and time manipulation
library(stringr)       # For string manupulation
library(openxlsx)      # To export the file into Microsoft Excel
library(tidyverse)     #Suite of Different Packages
library(ggplot2)       #For Data Visualization
library(magrittr)      # for easier syntax in one or two areas
library(gridExtra)     # for generating some comparison plots
library(directlabels)  # for labels in line graph
library(reshape)       # for reshaping the data
library(tibble)
library(knitr)
library(kableExtra)
library(janitor)
library(zoo)
library(expss)        # for countif and vlookup operation




### Read the csv file:
brfss_new <- read.csv("brfss2019.csv")
brfss_new

### Let's glimpse the imported data:
glimpse(brfss_new)

### Let's View the imported data:
View(brfss_new)

##### Data Manipulation  ##########
brfss_new <- 
  brfss_new %>% 
  
  # Rename the respondent ID below:
  dplyr::rename(., ID=X) %>%
  
  # Create a Month-Year Variable below:
  mutate(Date_response=mdy(idate))%>% 
  mutate(Month_Year = format(Date_response, "%b-%Y")) %>% 
  mutate(Month_Year = factor(Month_Year, levels = c("Jan-2019", 
                                                    "Feb-2019",
                                                    "Mar-2019",
                                                    "Apr-2019",
                                                    "May-2019",
                                                    "Jun-2019",
                                                    "Jul-2019",
                                                    "Aug-2019",
                                                    "Sep-2019",
                                                    "Oct-2019",
                                                    "Nov-2019",
                                                    "Dec-2019",
                                                    "Jan-2020",
                                                    "Feb-2020",
                                                    "Mar-2020",
                                                    "Apr-2020"))) %>%  
  
  
  # Recode the Sex Variable and change the level of the factor below: 1= Male, 2= Female
  mutate(sex= case_when(
    x.sex %in% c("1") ~ "Male",
    x.sex %in% c("2") ~ "Female",
    TRUE ~ NA_character_
  )) %>% 
  mutate(sex = factor(sex, levels = c("Male", "Female"))) %>% 
  
  
  # Recode the Education Level Variable and change the level of the factor below:
  mutate(Education_Level= case_when(
    educa %in% c("1") ~ "Never attended school or only kindergarten",
    educa %in% c("2") ~ "Grades 1 through 8 (Elementary)",
    educa %in% c("3") ~ "Grades 9 through 11 (Some high school)",
    educa %in% c("4") ~ "Grade 12 or GED (High school graduate)",
    educa %in% c("5") ~ "College 1 year to 3 years (Some college or technical school)",
    educa %in% c("6") ~ "College 4 years or more (College graduate)",
    educa %in% c("9") ~ "Refused",
    TRUE ~ "Not asked/Missing"
  )) %>% 
  mutate(Education_Level = factor(Education_Level, 
                                  levels = c("Never attended school or only kindergarten", 
                                             "Grades 1 through 8 (Elementary)",
                                             "Grades 9 through 11 (Some high school)",
                                             "Grade 12 or GED (High school graduate)",
                                             "College 1 year to 3 years (Some college or technical school)",
                                             "College 4 years or more (College graduate)",
                                             "Refused",
                                             "Not asked/Missing"))) %>% 
  
  
  # Recode the Health Coverage Variable and change the level of the factor below:
  mutate(health_coverage= case_when(
    hlthpln1 %in% c("1") ~ "Yes",
    hlthpln1 %in% c("2") ~ "No",
    hlthpln1 %in% c("7") ~ "Don't Know/Not Sure",
    hlthpln1 %in% c("9") ~ "Refused",
    TRUE ~ "Not asked/Missing"
  )) %>% 
  mutate(health_coverage = factor(health_coverage, 
                                  levels = c("Yes", 
                                             "No",
                                             "Don't Know/Not Sure",
                                             "Refused",
                                             "Not asked/Missing")))










## Question:  Do you have any kind of health care coverage, including health insurance, prepaid plans such as HMOs, or government plans such as Medicare, or Indian Health Service?

brfss_new %>% 
  group_by(health_coverage) %>% 
  summarise(n())




### State:

brfss_new %>% 
  mutate(state= case_when(
    x.state %in% c("1")	~ "Alabama",
    x.state %in% c("2")	~ "Alaska",
    x.state %in% c("4")	~ "Arizona",	
    x.state %in% c("5")	~ "Arkansas",
    x.state %in% c("6")	~ "California",
    x.state %in% c("8")	~ "Colorado",
    x.state %in% c("9")	~ "Connecticut",
    x.state %in% c("10")~ "Delaware",
    x.state %in% c("11")~ "District of Columbia",
    x.state %in% c("12")~ "Florida",
    x.state %in% c("13")~ "Georgia",
    x.state %in% c("15")~ "Hawaii",
    x.state %in% c("16")~ "Idaho",
    x.state %in% c("17")~ "Illinois",
    x.state %in% c("18")~ "Indiana",
    x.state %in% c("19")~ "Iowa",
    x.state %in% c("20")~ "Kansas",	
    x.state %in% c("21")~ "Kentucky",
    x.state %in% c("22")~ "Louisiana",
    x.state %in% c("23")~ "Maine",
    x.state %in% c("24")~ "Maryland",	
    x.state %in% c("25")~ "Massachusetts",
    x.state %in% c("26")~ "Michigan",
    x.state %in% c("27")~ "Minnesota",	
    x.state %in% c("28")~ "Mississippi",
    x.state %in% c("29")~ "Missouri",
    x.state %in% c("30")~ "Montana",
    x.state %in% c("31")~ "Nebraska",
    x.state %in% c("32")~ "Nevada",
    x.state %in% c("33")~ "New Hampshire",
    x.state %in% c("35")~ "New Mexico",
    x.state %in% c("36")~ "New York",
    x.state %in% c("37")~ "North Carolina",
    x.state %in% c("38")~ "North Dakota",
    x.state %in% c("39")~ "Ohio",
    x.state %in% c("40")~ "Oklahoma",
    x.state %in% c("41")~ "Oregon",
    x.state %in% c("42")~ "Pennsylvania",
    x.state %in% c("44")~ "Rhode Island",
    x.state %in% c("45")~ "South Carolina",
    x.state %in% c("46")~ "South Dakota",
    x.state %in% c("47")~ "Tennessee",
    x.state %in% c("48")~ "Texas",
    x.state %in% c("49")~ "Utah",
    x.state %in% c("50")~ "Vermont",
    x.state %in% c("51")~ "Virginia",
    x.state %in% c("53")~ "Washington",	
    x.state %in% c("54")~ "West Virginia",
    x.state %in% c("55")~ "Wisconsin",
    x.state %in% c("56")~ "Wyoming",
    x.state %in% c("66")~ "Guam",
    x.state %in% c("72")~ "Puerto Rico",
    TRUE ~ NA_character_
  )) %>% 
  group_by(state) %>% 
  summarise(n()) %>% 
  View()




#### Data for choloropleath map:

HCA_State <-
  brfss_new %>% 
  
  mutate(health_coverage= case_when(
    hlthpln1 %in% c("1") ~ "Yes",
    hlthpln1 %in% c("2") ~ "No",
    hlthpln1 %in% c("7") ~ "Don't Know/Not Sure",
    hlthpln1 %in% c("9") ~ "Refused",
    TRUE ~ "Not asked/Missing"
  )) %>% 
  mutate(health_coverage = factor(health_coverage, 
                                  levels = c("Yes", 
                                             "No",
                                             "Don't Know/Not Sure",
                                             "Refused",
                                             "Not asked/Missing"))) %>% 
  mutate(state= case_when(
    x.state %in% c("1")	~ "Alabama",
    x.state %in% c("2")	~ "Alaska",
    x.state %in% c("4")	~ "Arizona",	
    x.state %in% c("5")	~ "Arkansas",
    x.state %in% c("6")	~ "California",
    x.state %in% c("8")	~ "Colorado",
    x.state %in% c("9")	~ "Connecticut",
    x.state %in% c("10")~ "Delaware",
    x.state %in% c("11")~ "District of Columbia",
    x.state %in% c("12")~ "Florida",
    x.state %in% c("13")~ "Georgia",
    x.state %in% c("15")~ "Hawaii",
    x.state %in% c("16")~ "Idaho",
    x.state %in% c("17")~ "Illinois",
    x.state %in% c("18")~ "Indiana",
    x.state %in% c("19")~ "Iowa",
    x.state %in% c("20")~ "Kansas",	
    x.state %in% c("21")~ "Kentucky",
    x.state %in% c("22")~ "Louisiana",
    x.state %in% c("23")~ "Maine",
    x.state %in% c("24")~ "Maryland",	
    x.state %in% c("25")~ "Massachusetts",
    x.state %in% c("26")~ "Michigan",
    x.state %in% c("27")~ "Minnesota",	
    x.state %in% c("28")~ "Mississippi",
    x.state %in% c("29")~ "Missouri",
    x.state %in% c("30")~ "Montana",
    x.state %in% c("31")~ "Nebraska",
    x.state %in% c("32")~ "Nevada",
    x.state %in% c("33")~ "New Hampshire",
    x.state %in% c("35")~ "New Mexico",
    x.state %in% c("36")~ "New York",
    x.state %in% c("37")~ "North Carolina",
    x.state %in% c("38")~ "North Dakota",
    x.state %in% c("39")~ "Ohio",
    x.state %in% c("40")~ "Oklahoma",
    x.state %in% c("41")~ "Oregon",
    x.state %in% c("42")~ "Pennsylvania",
    x.state %in% c("44")~ "Rhode Island",
    x.state %in% c("45")~ "South Carolina",
    x.state %in% c("46")~ "South Dakota",
    x.state %in% c("47")~ "Tennessee",
    x.state %in% c("48")~ "Texas",
    x.state %in% c("49")~ "Utah",
    x.state %in% c("50")~ "Vermont",
    x.state %in% c("51")~ "Virginia",
    x.state %in% c("53")~ "Washington",	
    x.state %in% c("54")~ "West Virginia",
    x.state %in% c("55")~ "Wisconsin",
    x.state %in% c("56")~ "Wyoming",
    x.state %in% c("66")~ "Guam",
    x.state %in% c("72")~ "Puerto Rico",
    TRUE ~ NA_character_
  )) %>% 
  group_by(state, health_coverage) %>% 
  summarise(cnt=n()) %>% 
  ungroup()%>%
  spread(health_coverage,cnt, fill=0) %>% 
  adorn_totals(where = c("row")) %>%
  adorn_percentages("row")%>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns("front") 


## Export the data for choropleath map:
## Export the document:
#write.csv(HCA_State, "HCA_State_Map.csv")








