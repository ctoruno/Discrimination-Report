## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            DISCRIMINATION DYNAMIC REPORT - Settings
##
## Author:            Carlos Toruño   (ctoruno@worldjusticeproject.org)
##
## Creation date:     January 2nd, 2023
##
## This version:      January 2nd, 2023
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#                1.  Loading Libraries                                                                   ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required libraries
library(haven)
library(shiny)
library(shinyWidgets)
library(showtext)
library(tidyverse)


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#                2.  SharePoint Path                                                                     ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "carlostorunopaniagua") {
  path2SP <- paste0("/Users/carlostorunopaniagua/OneDrive - World Justice Project/Data Analytics/")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics/")
  
} else if (Sys.info()["user"] == "jaeheelee"){
  path2SP <- paste0("/Users/jaeheelee/Library/CloudStorage/OneDrive-SharedLibraries-WorldJusticeProject/",
                    "Research - Data Analytics/")
  
} else{
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
}

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#                3.  Data Loading                                                                        ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

discrimination.ls <- list(
  "Overview"      = readRDS("Data/discrimination1.rds"),
  "Disaggregated" = read_csv("Data/discrimination2.csv") 
)

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#                4.  Fonts & ggplo Theme                                                                 ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading fonts
font_add_google("Lato", "Lato")
showtext_auto()

# Defining a ggplot WJP theme
WJP_theme <- function() {
  theme(legend.title = element_blank(),
        legend.text  = element_text(size   = 12,
                                    family = "Lato",
                                    face   = "bold",
                                    margin = margin(10, 0, 10, 0)),
        panel.background   = element_rect(fill = "white",
                                          size = 2),
        panel.grid.major   = element_line(size     = 0.5,
                                          colour   = "grey93",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato",
                                          face     = "bold",
                                          size     = 13,
                                          color    = "black",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato",
                                          face     = "bold",
                                          size     = 13,
                                          color    = "black",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato",
                                          face     = "plain",
                                          size     = 11,
                                          color    = "black"),
        axis.text.x = element_text(family = "Lato",
                                   face   = "plain",
                                   size   = 11,
                                   color  = "black"),
        axis.ticks  = element_blank(),
        plot.title  = element_text(family = "Lato",
                                   face   = "bold",
                                   size   = 20,
                                   color  = "black",
                                   margin = margin(10, 0, 0, 0)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Lato",
                                     face   = "italic",
                                     size   = 16,
                                     color  = "black",
                                     margin = margin(10, 0, 15,0)),
        plot.caption = element_text(family  = "Lato",
                                    face    = "italic",
                                    size    = 11,
                                    color   = "black",
                                    margin  = margin(10, 0, 10,0)),
        plot.margin  = unit(c(2.5, 7.5, 7.5, 2.5), "mm")
  ) 
}
  
