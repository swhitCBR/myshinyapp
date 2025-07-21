library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(ggplot2)
library(shinyjs)
library(vitality)
library(DT) # datatables


# GGPLOT THEME
mytheme=theme(axis.line = element_line(colour = "black"),
              axis.title=element_text(colour = "black",size = 18),
              axis.text = element_text(colour = "black",size=12),
              axis.ticks = element_line(colour = 'black'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border =  element_rect(fill=NA,colour = "black"), #element_blank(),
              panel.background = element_blank(),
              plot.margin = unit(c(1,1,1,1), "cm"),
              strip.text.x = element_text(size = 12),
              strip.background = element_blank(),
              axis.title.y=element_text(margin=margin(0,20,0,0)),
              axis.title.x=element_text(margin=margin(20,0,0,0)),
              legend.key = element_blank(),
              legend.title = element_text(size=16),
              legend.text =element_text(size=12),
              plot.title = element_text(hjust = 0,size=16))

