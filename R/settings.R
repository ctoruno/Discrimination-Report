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
library(ggtext)
library(plotly)
library(shinyWidgets)
library(showtext)
library(tidyverse)

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
#                3.  Fonts & ggplot Theme                                                                ----
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading fonts
font_add_google("Lato", "Lato")
showtext_auto()

# Defining a ggplot WJP theme
WJP_theme <- function() {
  theme(legend.title = element_blank(),
        legend.text  = element_text(size   = 14,
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
                                          size     = 14,
                                          color    = "black",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato",
                                          face     = "bold",
                                          size     = 14,
                                          color    = "black",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato",
                                          face     = "plain",
                                          size     = 13,
                                          color    = "black"),
        axis.text.x = element_text(family = "Lato",
                                   face   = "plain",
                                   size   = 13,
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
                                     size   = 17,
                                     color  = "black",
                                     margin = margin(10, 0, 15,0)),
        plot.caption = element_markdown(family  = "Lato",
                                        face    = "italic",
                                        size    = 14,
                                        color   = "black",
                                        margin  = margin(15, 0, 5,0)),
        plot.margin  = unit(c(2.5, 7.5, 7.5, 2.5), "mm")
  ) 
}
  
