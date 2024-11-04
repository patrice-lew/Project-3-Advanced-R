# Project-3-Advanced-R
- Project 3 examined the epidemiological patterns of total COVID-19 cases in three (3) Florida counties; Miami-Dade, Broward, Palm Beach.
- These 3 counties have the highest population in Florida respectively.
- CDC COVID data was used as the data source


# Title: "COVID-19 Total Cases Informational Graph: Miami-Dade, Broward & Palm Beach Counties"




# Introduction




The COVID-19 pandemic completely revolutionized the world systems including science and healthcare. Data visualization allows for greater understanding of new findings that can inform and improve public health. The aim of this project is to examine the epidemiological patterns of total COVID-19 cases in three (3) Florida counties; Miami-Dade, Broward, Palm Beach. These 3 counties have the highest population in Florida respectively. 





# Total Cases
```{r data-setup}


library(tidyverse)

# CSV file containing cleaned data
COVID_clean <- read_csv("data_clean/CDC_COVID_wrangled.csv")


```

```{r average-cases}

# stores the three FL counties of interest
fl_counties <- c("Miami-Dade","Broward", "Palm Beach")

covid_cases <- 
  ggplot(data = filter(COVID_clean,county %in% fl_counties)
  ) +
  aes(
    x = report_date,
    y = cases_7dayAve,
    color = county
  ) +
  labs(
    x = "Date",
    y = "Count of COVID-19 Cases",
    title = "Total COVID-19 Cases",
    subtitle = "for Miami-Dade, Broward and Palm Beach County",
    caption = "Source: CDC",
  ) +
  geom_line(linewidth = 1)

covid_cases
```

This graph depicts the number of COVID-19 cases per day from 12/17/2020 - 8/23/22 in Miami-Dade, Broward and Palm Beach. All three counties follow similar patterns with peaks in early 2021 and a general decline through mid-2021. After July 2021, total cases in all three counties increased, and then declined once again by the end of the year. For the year 2022, the highest record of COVID-19 cases thus far was reported in January. This may be as a result of the new  Omicron variant that was declared by the WHO in November 2021. This variant was known to be more transmissible than all previous strains and traveled throughout the world within 4 weeks after WHO's announcement. This was known as the "Omicron effect" and resulted in the most deaths worldwide. A sharp decline was seen shortly after in the year. 

Among the 3 counties, Miami-Dade maintained the highest number of total cases throughout the reporting period. This is congruent with the fact that this county is the most populated in Florida, followed by Broward and Palm Beach. Miami-Dade is also home to Miami International Airport, a central hub for visitors and tourists from all over the US and the world.  





# Conclusion




COVID-19 total cases were successfully depicted using the informational graph for Miami-Dade, Broward and Palm Beach counties. These three counties followed the global trend of COVID-cases based on the different variants. The highest peak in early 2022 can be accounted for by the Omicron variant.



