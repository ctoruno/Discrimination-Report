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

discrimination1_prev.df <- master_data.df %>%
  select(country, year, 
         q18a, q18b, q18c, q18f, q18e, EXP_q17g, EXP_q17h, EXP_q17j) %>%
  mutate(
    across(!country,
           as.double),
    across(!country,
           ~if_else(.x == 99, NA_real_, .x))
  )

discrimination1.df <- discrimination1_prev.df %>%
  group_by(country, year) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  bind_rows(
    discrimination1_prev.df %>%
      mutate(country = "LAC avg") %>%
      group_by(country, year) %>%
      summarise(across(everything(),
                       mean,
                       na.rm = T))
  ) %>%
  mutate(across(!year,
                ~.x*100))
  
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

actors1_prev.df <- master_data.df %>%
  select(country, year, 
         q1d, q1e, q1f, q1f, q1g, q2d, q2e, q2f, q2g) %>%
  mutate(
    across(!country,
           as.double),
    across(starts_with("q1"),
           ~case_when(
             .x < 3          ~ 1,
             .x > 2 & .x < 5 ~ 0,
             .x > 5          ~ NA_real_
           )),
    across(starts_with("q2"),
           ~case_when(
             .x < 3          ~ 0,
             .x > 2 & .x < 5 ~ 1,
             .x > 5          ~ NA_real_
           ))
  )

actors1.df <- actors1_prev.df %>%
  group_by(country, year) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  bind_rows(
    actors1_prev.df %>%
      mutate(country = "LAC avg") %>%
      group_by(country, year) %>%
      summarise(across(everything(),
                       mean,
                       na.rm = T))
  ) %>%
  mutate(across(!year,
                ~.x*100))

actors2.df <- master_data.df %>%
  filter(year == 2022) %>%
  select(country, 
         inc_cat    = fin, 
         gend_cat   = gend, 
         skin_cat   = COLOR,
         q1d, q1e, q1f, q1f, q1g, q2d, q2e, q2f, q2g) %>%
  mutate(
    inc_cat   = case_when(
      inc_cat < 3               ~ "Financially Insecured",
      inc_cat > 2 & inc_cat < 6 ~ "Financially Secured"
    ),
    gend_cat  = case_when(
      gend_cat == 1 ~ "Male",
      gend_cat == 2 ~ "Female"
    ),
    skin_cat = case_when(
      skin_cat < 7                 ~ "Dark Skin",
      skin_cat > 6 & skin_cat < 12 ~ "Light Skin"
    ),
    across(starts_with("q1"),
           ~case_when(
             .x < 3          ~ 1,
             .x > 2 & .x < 5 ~ 0,
             .x > 5          ~ NA_real_
           )),
    across(starts_with("q2"),
           ~case_when(
             .x < 3          ~ 0,
             .x > 2 & .x < 5 ~ 1,
             .x > 5          ~ NA_real_
           ))
  )

actors2.df <- map_dfr(c("inc", "gend", "skin"),
                      function(targetVar){
                        actors2.df %>%
                          select(country, 
                                 starts_with(targetVar),
                                 starts_with("q")) %>%
                          rename(category = 2) %>%
                          group_by(country, category) %>%
                          summarise(
                            across(starts_with("q"),
                                   mean,
                                   na.rm = T,
                                   .names = "mean_{col}"),
                            across(starts_with("q"),
                                   sd,
                                   na.rm = T,
                                   .names = "sd_{col}"),
                            n = n()
                          ) %>%
                          filter(!is.na(category))
                      }) %>%
  mutate(across(!c(category, n),
                ~.x*100))

actors2.df <- actors2.df %>%
  bind_cols(
    map_dfc(c("q1d", "q1e", "q1f", "q1g", "q2d", "q2e", "q2f", "q2g"),
            function(targetVar){
              tname <- as.name(targetVar)
              actors2.df %>%
                ungroup() %>%
                select(ends_with(targetVar), n) %>%
                rename(mean = 1,
                       sd   = 2) %>%
                mutate(
                  "upper_{{ tname }}" := mean+(1.960*(sd/(sqrt(n)))),
                  "lower_{{ tname }}" := mean-(1.960*(sd/(sqrt(n))))
                ) %>%
                select(4, 5)
            })
  )



write_csv(actors1.df, "Data/actors1.csv")
write_csv(actors2.df, "Data/actors2.csv")

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#         3.  Perceptions Criminal Justice System                                                       ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

yrs <- master_data.df %>%
  group_by(country) %>%
  slice_max(order_by = year,
            n = 2) %>%
  ungroup() %>%
  group_by(year, country) %>%
  summarise() %>%
  pull(year)

perceptions <- master_data.df %>%
  select(country, year, 
         inc_cat    = fin, 
         gend_cat   = gend, 
         skin_cat   = COLOR,
         q48e_G1, q48f_G1, q48g_G1, q49a_G1, q49a_G2, q49b_G1, q49c_G1, EXP_q23d_G1, q49e_G1, EXP_q23f_G1, 
         q48e_G2, q48f_G2, q48g_G2, q49b_G2, q49c_G2, q49e_G2)  %>%
  mutate(
    across(!c(country,inc_cat, gend_cat, skin_cat),
           as.double),
    across(!c(country,inc_cat, gend_cat, skin_cat),
           ~if_else(.x == 99, NA_real_, .x)),
    across(!c(country, year, inc_cat, gend_cat, skin_cat),
           ~case_when(
             .x < 3          ~ 1,
             .x > 2 & .x < 5 ~ 0,
             .x > 5          ~ NA_real_
           ))) %>%
  select(!starts_with("EXP")) %>%
  select(!c(starts_with("q49")))

perceptions1 <- perceptions %>%
  filter(year %in% yrs) %>%
  select(!c(inc_cat, gend_cat, skin_cat)) %>%
  group_by(country, year) %>%
  summarise(across(everything(),
                   mean,
                   na.rm = T)) %>%
  rename(group = year) %>%
  mutate(group = as.character(group))

perceptions2 <- perceptions %>%
  filter(year == 2022) %>%
  mutate(
    inc_cat   = case_when(
      inc_cat < 3               ~ "Financially Insecured",
      inc_cat > 2 & inc_cat < 6 ~ "Financially Secured"
    ),
    gend_cat  = case_when(
      gend_cat == 1 ~ "Male",
      gend_cat == 2 ~ "Female"
    ),
    skin_cat = case_when(
      skin_cat < 7                 ~ "Dark Skin",
      skin_cat > 6 & skin_cat < 12 ~ "Light Skin"
    )) %>%
  rename(group = country)

perceptions2 <- map_dfr(c("inc", "gend", "skin"),
                        function(targetVar){
                          perceptions2 %>%
                            select(group, 
                                   starts_with(targetVar),
                                   starts_with("q")) %>%
                            rename(category = 2) %>%
                            group_by(group, category) %>%
                            summarise(
                              across(starts_with("q"),
                                     mean,
                                     na.rm = T
                              )) %>%
                            filter(!is.na(category))
                        })


write_csv(actors1.df, "Data/perceptions1.csv")
write_csv(actors2.df, "Data/perceptions2.csv")




