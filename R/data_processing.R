## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            DISCRIMINATION DYNAMIC REPORT - Data Processing
##
## Author:            Carlos Toruño   (ctoruno@worldjusticeproject.org)
##
## Creation date:     January 2nd, 2023
##
## This version:      January 2nd, 2023
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required libraries
library(haven)
library(tidyverse)

# SharePoint path
if (Sys.info()["user"] == "carlostorunopaniagua") {
  path2SP <- paste0("/Users/carlostorunopaniagua/OneDrive - World Justice Project/Data Analytics/")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics/")
  
} else if (Sys.info()["user"] == "jaeheelee"){
  path2SP <- paste0("/Users/jaeheelee/Library/CloudStorage/OneDrive-SharedLibraries-WorldJusticeProject/",
                    "Research - Data Analytics/")
  
} else if (Sys.info()["user"] == "macbookprosolido") {
  path2SP <- paste0("/Users/macbookprosolido/Documents/WJP/")
} else{
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
}

# Loading data
master_data.df <- read_dta(paste0(path2SP,
                                  "Data/GPP/LAC - Merged.dta"))

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#                1.  Discrimination Data                                                                 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

discrimination1.df <- master_data.df %>%
  select(country, year, 
         q18a, q18b, q18c, q18f, q18e, EXP_q17g, EXP_q17h, EXP_q17j) %>%
  mutate(across(!country,
                as.double),
         across(!country,
                ~if_else(.x == 99, NA_real_, .x))) %>%
  group_by(country, year) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  mutate(across(!year,
                ~.x*100)) %>%
  group_by(year) %>%
  bind_rows(summarise(., across(where(is.numeric), 
                                mean,
                                na.rm = T),
                      across(where(is.character), ~'LAC avg')))
  
discrimination2.df <- master_data.df %>%
  filter(year == 2022) %>%
  select(country, 
         inc_value  = q18a, 
         gend_value = q18b, 
         skin_value = EXP_q17g,
         inc_cat    = fin, 
         gend_cat   = gend, 
         skin_cat   = COLOR) %>%
  mutate(
    inc_cat   = case_when(
      inc_cat < 3 ~ "Financially Insecured",
      inc_cat > 2 & inc_cat < 6 ~ "Financially Secured"
    ),
    gend_cat  = case_when(
      gend_cat == 1 ~ "Male",
      gend_cat == 2 ~ "Female"
    ),
    skin_cat = case_when(
      skin_cat < 7 ~ "Dark Skin",
      skin_cat > 6 & skin_cat < 12 ~ "Light Skin"
    ),
    across(ends_with("_value"),
           as.double),
    across(ends_with("_value"),
           ~if_else(.x == 99, NA_real_, .x))
  )

discrimination2.df <- map_dfr(c("inc", "gend", "skin"),
                function(targetVar){
                  discrimination2.df %>%
                    select(country, starts_with(targetVar)) %>%
                    rename(value    = 2,
                           category = 3) %>%
                    group_by(country, category) %>%
                    mutate(value = value*100) %>%
                    summarise(
                      mean  = mean(value, na.rm = T),
                      n     = n(),
                      sd    = sd(value, na.rm = T)
                    ) %>%
                    mutate(
                      upper = mean+(1.960*(sd/(sqrt(n)))),
                      lower = mean-(1.960*(sd/(sqrt(n))))
                    ) %>%
                    filter(!is.na(category)) %>%
                    bind_rows(
                      discrimination2.df %>%
                        select(country, starts_with(targetVar)) %>%
                        rename(value    = 2,
                               category = 3) %>%
                        group_by(category) %>%
                        mutate(value = value*100) %>%
                        summarise(
                          mean  = mean(value, na.rm = T),
                          n     = n(),
                          sd    = sd(value, na.rm = T)
                        ) %>%
                        mutate(
                          upper = mean+(1.960*(sd/(sqrt(n)))),
                          lower = mean-(1.960*(sd/(sqrt(n)))),
                          country = "LAC avg"
                        ) %>%
                        filter(!is.na(category))
                    )
                }) 

write_csv(discrimination1.df, "Data/discrimination1.csv")
write_csv(discrimination2.df, "Data/discrimination2.csv")

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#                2.  Criminal Justice Actors Data                                                        ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

