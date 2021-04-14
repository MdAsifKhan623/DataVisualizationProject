#### BRFSS 2019 Data:

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














##### Pie Chart #########

## Pie Chart: Gender
brfss_sex_Pie_Per <-
  brfss_new %>%
  group_by(sex) %>%
  summarise(cnt = n())%>%
  mutate(per=cnt/sum(cnt)) 

## Create the Percent Label
brfss_sex_Pie_Per$label <- 
  scales::percent(brfss_sex_Pie_Per$per)


## Actual Pie Chart:

library(ggrepel)

ggplot(data = brfss_sex_Pie_Per, mapping = aes('', per, fill = sex)) +
  geom_col(position = 'fill', width = 1) +
  geom_text_repel(aes(label = label, x = 1.0), position = position_fill(vjust = 0.5)) +
  labs(title = "Proportion of Survey Respondents by Sex (n=418,268)")+
  coord_polar(theta = 'y') +
  scale_fill_manual(values=c("#56B4E9","pink"))+
  theme_grey() +
  
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_rect(color = "black", size = 1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold",color = "black", hjust = 0.5),
        legend.position = "right",
        legend.key.size = unit(1.5, "lines"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5, color = "black", face="bold")) +
  guides(fill = guide_legend(override.aes = list(colour = NA)))



## Export final plot ----
ggsave(filename = file.path("/Users/prasadbhoite/school/DataVisualization_CAP5738/Project/NewGraphs",
                            "PieChart.png"), 
       dpi = 600, width = 6.80, height = 4.85)