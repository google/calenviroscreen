# Copyright 2014 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


library(shiny)
shinyUI(pageWithSidebar(  
  headerPanel("Visualizing CES Data"),
  sidebarPanel(
    selectInput("region.name", "Region",
                c("California" = "california",
                  "Bay Area" = "bay.area",
                  "LA / San Diego" = "la.sd"),
                  selected = "california"),
    tags$hr(),
    conditionalPanel(condition = "input.conditionedPanels == 'Map' || input.conditionedPanels == 'Correlation'",
      selectInput("var1", "Variable 1:",
                  c("None", "Users", "Population", "CES Score", "Ozone",
                    "PM2.5",
                    "Diesel PM", "Drinking Water", "Pesticides",
                    "Toxic Release", "Traffic", "Cleanup Sites",
                    "Groundwater Threats", "Hazardous Waste",
                    "Impaired Water Bodies", "Solid Waste", "Pollution Burden",
                    "Age", "Asthma", "Low Birth Weight", "Education",
                    "Linguistic Isolation", "Poverty", "Unemployment",
                    "Population Characteristics",
                    "Population Characteristics Score"),
                  selected = c("CES Score"))
    ),
    conditionalPanel(condition = "input.conditionedPanels == 'Map'",
      radioButtons("type1", "Variable Type:",
                   c("Raw" = "raw",
                     "Percentile" = "percentile"),
                     selected = "raw"),
      selectInput("granularity1", "Granularity:",
                  c("County" = "county"),
                  selected = "county"),
      checkboxInput("exclude1", "Exclude Zeroes", value = TRUE)
    ),
    conditionalPanel(condition = "input.conditionedPanels == 'Map' || input.conditionedPanels == 'Correlation'",
      tags$hr(),     
      selectInput("var2", "Variable 2:",
                  c("None", "Users", "Population", "CES Score", "Ozone",
                    "PM2.5",
                    "Diesel PM", "Drinking Water", "Pesticides",
                    "Toxic Release", "Traffic", "Cleanup Sites",
                    "Groundwater Threats", "Hazardous Waste",
                    "Impaired Water Bodies", "Solid Waste", "Pollution Burden",
                    "Age", "Asthma", "Low Birth Weight", "Education",
                    "Linguistic Isolation", "Poverty", "Unemployment",
                    "Population Characteristics",
                    "Population Characteristics Score"),
                selected = c("CES Score"))      
    ),
    conditionalPanel(condition = "input.conditionedPanels == 'Map'",
      radioButtons("type2", "Variable Type:",
                   c("Raw" = "raw",
                     "Percentile" = "percentile"),
                     selected = "raw"),
      selectInput("granularity2", "Granularity:",
                  c("County" = "county",
                    "Zipcode" = "zipcode",
                    "Census Tract" = "census"),
                  selected = "county"),
      checkboxInput("exclude2", "Exclude Zeroes", value = TRUE)
    ),
    conditionalPanel(condition = "input.conditionedPanels == 'Correlation'",
      tags$hr(),
      selectInput("granularity.corr", "Granularity:",
                  c("County" = "county",
                    "Zipcode" = "zipcode",
                    "Census Tract" = "census"),
                  selected = "county"),
      checkboxInput("exclude.corr", "Exclude Zeroes", value = TRUE)
    ),
    conditionalPanel(condition = "input.conditionedPanels == 'TopCorrelations'",
      selectInput("filter1", "Filter 1:",
                  c("None", "Users", "Population", "CES Score", "Ozone",
                    "PM2.5",
                    "Diesel PM", "Drinking Water", "Pesticides",
                    "Toxic Release", "Traffic", "Cleanup Sites",
                    "Groundwater Threats", "Hazardous Waste",
                    "Impaired Water Bodies", "Solid Waste", "Pollution Burden",
                    "Age", "Asthma", "Low Birth Weight", "Education",
                    "Linguistic Isolation", "Poverty", "Unemployment",
                    "Population Characteristics"),
                  selected = c("None")),
      selectInput("filter2", "Filter 2:",
                  c("None", "Users", "Population", "CES Score", "Ozone",
                    "PM2.5",
                    "Diesel PM", "Drinking Water", "Pesticides",
                    "Toxic Release", "Traffic", "Cleanup Sites",
                    "Groundwater Threats", "Hazardous Waste",
                    "Impaired Water Bodies", "Solid Waste", "Pollution Burden",
                    "Age", "Asthma", "Low Birth Weight", "Education",
                    "Linguistic Isolation", "Poverty", "Unemployment",
                    "Population Characteristics"),
                  selected = c("None")),
      selectInput("granularity.topcorr", "Granularity:",
                  c("County" = "county",
                    "Zipcode" = "zipcode",
                    "Census Tract" = "census"),
                  selected = "county"),
      selectInput("splitby.topcorr", "Split by:",
                  c("None" = "none",
                    "County" = "county",
                    "Zipcode" = "zipcode"),
                  selected = "none"),
      checkboxInput("exclude.topcorr", "Exclude Zeroes", value = TRUE),
      numericInput("num.obs", "Top Correlations:", 15),
      numericInput("min.sample.size", "Minimum Sample Size:", 0)
    ),
    tags$hr(),
    conditionalPanel("input.conditionedPanels != 'TopCorrelations' & (input.var1 == 'Users' || input.var2 == 'Users')",
      checkboxGroupInput("user.restrictions", "Optional User Restrictions",
                         c("Fleet" = "fleet",
                           "Retail" = "retail",
                           "Diesel" = "diesel",
                           "FFV" = "ffv"),
                           selected = c("fleet", "retail", "diesel", "ffv"))
    ),
  width = 3),

  mainPanel(
    tabsetPanel(
      tabPanel("Map",
               plotOutput("california.plot", width = "100%", height = "100%"),
               verbatimTextOutput("dictionary.v1.t1"),
               verbatimTextOutput("dictionary.v2.t1")
      ),
      tabPanel("Correlation",
               plotOutput("correlation.plot", width = "100%", height = "100%"),
               verbatimTextOutput("dictionary.v1.t2"),
               verbatimTextOutput("dictionary.v2.t2")
      ),
      tabPanel("TopCorrelations",
               tableOutput("top.cor")
      ),
      id = "conditionedPanels"                
    )
  )  
))

