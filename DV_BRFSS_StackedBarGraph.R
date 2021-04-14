#### BRFSS 2019 Data: Stacked Bar Graph

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









### Stacked Bar Graph:
brfss_bar_Edu_HealthCoverage <-
  brfss_new %>% 
  group_by(Education_Level, health_coverage) %>% 
  dplyr::summarise (n=n())%>% 
  mutate(percent = round(100*n/sum(n),0)) 




brfss_bar_Edu_HealthCoverage
brfss_bar_Edu_HealthCoverage$label <- paste0(sprintf("%.0f", brfss_bar_Edu_HealthCoverage$percent), "%")




library(stringr)

brfss_bar_Edu_HealthCoverage$Education_Level <-
  str_wrap(brfss_bar_Edu_HealthCoverage$Education_Level, width=12)

# bar plot of counts by industry with in group proportions 
ggplot(brfss_bar_Edu_HealthCoverage, aes(x = Education_Level, y = percent, fill = health_coverage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = percent, label = label), size = 4, position = position_stack(vjust = .5)) + 
  labs(title = "Proportion of Health Coverage by Educational Attainment", 
       x="Educational Attainment", 
       y="Percentage") +
  theme(plot.title = element_text(hjust = 0.5,size=16, color = "black", face="bold"))+
  theme(axis.text.x = element_text(size=10, angle=0, hjust = 0.5, color = "black", face="bold")) +
  theme(axis.text.y = element_text(size=1, hjust = 1, color = "black", face="bold")) +
  theme(axis.title.x = element_text(size=10, hjust = 0.5, color = "black", face="bold")) +
  theme(axis.title.y = element_text(size=10, hjust = 0.5, color = "black", face="bold"))








