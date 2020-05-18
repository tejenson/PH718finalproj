# ui.r
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)
library(shinyjs)
library(broom)

# Define UI for data upload app ----
fluidPage(theme = shinytheme("darkly"),
                # App title ----
                titlePanel("Blood Cadmium & Lead Measures and Age-Related Cognitive Decline, NHANES 2011-2014"),
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput("file1", "Choose CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    # Horizontal line ----
                    tags$hr(),
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    # Input: Select separator ----
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    # Input: Select quotes ----
                    radioButtons("quote", "Quote",
                                 choices = c(None = "",
                                             "Double Quote" = '"',
                                             "Single Quote" = "'"),
                                 selected = '"'),
                    # Horizontal line ----
                    tags$hr(),
                    # Input: Select number of rows to display ----
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             All = "all"),
                                 selected = "head")
                  ),
                  # Main panel for displaying outputs ----
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Data", h2("Dataset Upload & View"),
                                         h4("<----- Upload csv file, then view header or entirety below"),
                                         # Output: Data file ----
                                         DT::dataTableOutput("contents")),
                                tabPanel("Missingness", h2("Missingness & inclusion overview"),
                                         imageOutput("image1")),
                                tabPanel("Overview", h2("Project Aims, Questions & Hypotheses"),
                                         h3("Aims:"),
                                         tags$ul(
                                           tags$li("Investigate relationship between blood cadmium and blood lead concentrations and cognitive function in U.S. adults 60+ years old"), 
                                           tags$ul(
                                             tags$li("Data analyzed from 2011-2014 National Health And Nutrition Examination Survey (NHANES)"), 
                                             tags$li("Inclusion criteria:"),
                                             tags$ul(
                                               tags$li("Adult participants 60+"),
                                               tags$li("Completed NHANES cognitive assessment"),
                                               tags$li("Blood lead and cadmium lab values"),),),
                                           tags$li("In addition, examine potential confounding and/or effect modification of:"),
                                           tags$ul(
                                             tags$li("Potential confounders: age, sex, marital status, education level, poverty income ratio, smoking status, and race/ethnicity"),
                                             tags$li("Potential modifiers of interest: smoking status, sex, cadmium*lead"),),
                                         ),
                                         h3("Scientific Questions of Interest:"),
                                         tags$ul(
                                           tags$li("Is there a dose/response relationship between composite cognitive assessment and concurrent blood lead and blood cadmium concentrations?"), 
                                           tags$li("Does sex or smoking status modify the hypothesized relationship between composite cognitive assessment and concurrent blood lead and blood cadmium concentrations?"), 
                                           tags$li("Do lead and cadmium together interact to have a synergistic effect on the hypothesized relationship between composite cognitive assessment and concurrent blood lead and blood cadmium concentrations assessed separately?"),
                                         ),
                                         h3("Statistical Questions of Interest:"),
                                         tags$ul(
                                           tags$li("Does having higher blood lead or blood cadmium concentration result in higher odds of low composite cognitive function score after adjusting for relevant confounders?"), 
                                           tags$li("Does sex or smoking status interact with blood lead or cadmium concentrations to modify the association between composite cognitive assessment and concurrent blood lead and blood cadmium concentrations?"),
                                         ),
                                         h3("Hypotheses:"),
                                         tags$ul(
                                           tags$li("Individuals with higher levels of blood lead or higher levels of blood cadmium, seaprately, will have higher odds of low cognitive score."), 
                                           tags$li("Sex and smoking status will modify the effects of blood lead and blood cadmium, spearately, on cognitive score."),
                                           tags$li("Lead and cadmium blood levels will interact to result in worse cognitive scores than blood lead and blood cadmium alone on cognitive score."),
                                         ),),
                                tabPanel("Variables", h2("Variable definitions"),
                                         tags$ul(
                                           tags$li("Exposures:"), 
                                           tags$ul(
                                             tags$li("Blood cadmium levels (μg/L)"), 
                                             tags$li("Blood lead levels (μg/dL)"),
                                           ),
                                           br(),
                                           tags$li("Outcome: Composite Cognitive Z-Score"),
                                           tags$ul(
                                             tags$li("Composite score calculated from the mean of standardized scores from four separate cognitive assessments conducted at the time of survey & blood draw"), 
                                             tags$li("Four individual cognitive tests: word recall, delayed word recall, animal fluency recall, and digit symbol test"),
                                             tags$li("Composite score is a z-score ranging from -3 (low) to 3 (high)"),
                                             tags$li("Final sample cogcompz score mean = 0.0713, stdev = 0.742"),
                                           ),
                                           br(),
                                           tags$li("Assessed Confounders:"),
                                           tags$ul(
                                             tags$li("Age(continous), marital status (yes/no), education level (5 levels), race/ethnicity (6 categories), poverty income ratio (6 levels), smoking status (never, active, former)"), 
                                           ),
                                           br(),
                                           tags$li("Assessed Effect Modifiers:"),
                                           tags$ul(
                                             tags$li("Sex (male/female)"), 
                                             tags$li("Smoking status"),
                                           ),
                                         ),),
                                tabPanel("Descriptives", h2("Histograms describing the data:"),
                                         h4("Counts of individuals' blood cadmium levels by sex"),
                                         plotOutput("histcdsex"), 
                                         tags$hr(), # Horizontal line ----
                                         h4("Counts of individuals' blood lead levels by sex"),
                                         plotOutput("histpbsex"),
                                         tags$hr(),# Horizontal line ----
                                         h4("Counts of individuals' blood cadmium levels by smoking status"),
                                         plotOutput("histcdsmk"), 
                                         tags$hr(), # Horizontal line ----
                                         h4("Counts of individuals' blood lead levels by smoking status"),
                                         plotOutput("histpbsmk"),
                                         tags$hr(),# Horizontal line ----
                                         h4("Counts of individuals' composite cognitive scores by sex"),
                                         plotOutput("histcogsex"), 
                                         tags$hr(), # Horizontal line ----
                                         h4("Counts of individuals' composite cognitive scores by smoking status"),
                                         plotOutput("histcogsmk"),
                                         tags$hr(),# Horizontal line ----
                                ),
                                tabPanel("Correlations", h2("Plots of exposures (cadmium & lead) and outcome (composite cognitive score)"),
                                         plotOutput("scatcdcog"), 
                                         tags$hr(), # Horizontal line ----
                                         plotOutput("scatpbcog"),
                                         tags$hr(),# Horizontal line ----
                                         plotOutput("scatcdpb"),
                                         tags$hr(),# Horizontal line ----
                                         plotOutput("scatcdage"), 
                                         tags$hr(), # Horizontal line ----
                                         plotOutput("scatpbage"),
                                         tags$hr(),# Horizontal line ----
                                         plotOutput("scatcogage"),
                                         tags$hr(),# Horizontal line ----
                                ),
                                tabPanel("Analysis", h2("Summary of Logistic Regression Analysis of Low Cognitive Score vs Blood Cadmium and Blood Lead (Separately)"),
                                         ("First ran separate logistic regressions of low cognitive score versus blood lead (LBXBPB) and blood cadmium (LBXBCD)."),
                                         verbatimTextOutput("summary_cdbasemodel"),
                                         verbatimTextOutput("summary_pbbasemodel"),
                                         ("Based on p-values, blood lead (LBXBPB) is a better predictor of low cognitive score than cadmium (LBXBCD)."),
                                         tags$hr(),# Horizontal line ----
                                         ("Using blood lead as the main predictor of low cognitive, ran stepwise selection on a saturated model that included covariates of interest and interaction term for lead*cadmium "),
                                         verbatimTextOutput("stepwise_select"),
                                         tags$hr(),# Horizontal line ----
                                         ("Logistic Regression of Composite Cognitive Score vs Blood Lead, adjusted for age, education level and race/ethnicity:"),
                                         verbatimTextOutput("exp_results"),
                                         tags$hr(),# Horizontal line ----
                                         ("Logistic Regression of Composite Cognitive Score vs Blood Lead Stratified by:"),
                                         br(),
                                         ("Males"),
                                         verbatimTextOutput("summary_stratmale"),
                                         ("Females"),
                                         verbatimTextOutput("summary_stratfemale"),
                                         tags$hr(),# Horizontal line ----
                                         ("Logistic Regression of Composite Cognitive Score vs Blood Lead Stratified by:"),
                                         br(),
                                         ("Never Smokers"),
                                         verbatimTextOutput("summary_stratnvrsmk"),
                                         ("Active Smokers"),
                                         verbatimTextOutput("summary_stratactsmk"),
                                         ("Former Smokers"),
                                         verbatimTextOutput("summary_stratfmrsmk"),
                                ),
                                tabPanel("Conclusions", h3("In conclusion"),
                                         tags$ul(
                                           tags$li("After adjusting for age, education level and race/ethnicity, blood lead level is associated with low cognitive score (adjOR = 1.08), albeit at a marginal level of statistical significance (p=0.07)."), 
                                           tags$li("Stratifying results by sex showed no appreciable effect modification (adjOR males: 1.054, adjOR females: 1.057) "), 
                                           tags$li("Stratifying results by smoking status showed some difference in association of blood level with low cognitive score:"),
                                           tags$ul(
                                             tags$li("Never smoker adjOR = 1.152"), 
                                             tags$li("Active smoker adjOR = 1.018"), 
                                             tags$li("Former smoker adjOR = 1.071"),
                                           ),
                                         ),
                                         
                                )
                    )
                  )
                )
)

