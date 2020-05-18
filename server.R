# server.r
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)
library(shinyjs)
library(broom)

# Define server logic to read selected file ----
function(input, output) {
  
  output$contents <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$histcdsex <- renderPlot({
    # feel like there must be a simpler way of pulling in the uploaded file than having to put this in each function
    # but for now it works so going with it!
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot() +  
      geom_histogram(aes(x=LBXBCD,fill = "g", colour="g"), alpha = 0.4, data=subset(df,RIAGENDR == '1'), binwidth = 0.1) +
      geom_histogram(aes(x=LBXBCD,fill = "b", colour="b"), alpha = 0.4, data=subset(df,RIAGENDR == '2'), binwidth = 0.1) +
      scale_colour_manual(name="Sex", values=c("g" = "green", "b"="blue"), labels=c("b"="Females", "g"="Males")) +
      scale_fill_manual(name="Sex", values=c("g" = "green", "b"="blue"), labels=c("b"="Females", "g"="Males")) +
      labs(title = "Blood Cadmium Concentration of Adults 60+ Years Old,NHANES 2011-2014. N=1806",
           y = "Count", x = "Blood Cadmium (μg/L)")
  })
  
  output$histpbsex <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot() + 
      geom_histogram(aes(x=LBXBPB,fill = "g", colour="g"), alpha = 0.4, data=subset(df,RIAGENDR == '1'), binwidth = 0.5, position="dodge") +
      geom_histogram(aes(x=LBXBPB,fill = "b", colour="b"), alpha = 0.4, data=subset(df,RIAGENDR == '2'), binwidth = 0.5, position="dodge" ) +
      scale_colour_manual(name="Sex", values=c("g" = "green", "b"="blue"), labels=c("b"="Females", "g"="Males")) +
      scale_fill_manual(name="Sex", values=c("g" = "green", "b"="blue"), labels=c("b"="Females", "g"="Males")) +
      labs(title = "Blood Lead Concentration of Adults 60+ Years Old,NHANES 2011-2014. N=1806",
           y = "Count", x = "Blood Lead (μg/L)")
  })
  
  output$histcdsmk <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot() +  
      geom_histogram(aes(x=LBXBCD,fill = "r", colour="r"), alpha = 0.4, data=subset(df,truesmkstat == '0'), binwidth = 0.1) +
      geom_histogram(aes(x=LBXBCD,fill = "g", colour="g"), alpha = 0.4, data=subset(df,truesmkstat == '1'), binwidth = 0.1) +
      geom_histogram(aes(x=LBXBCD,fill = "b", colour="b"), alpha = 0.4, data=subset(df,truesmkstat == '2'), binwidth = 0.1) +
      scale_colour_manual(name="Smoker Status", values=c("r" = "red", "g" = "green", "b"="blue"), labels=c("r" = "Never", "b"="Active", "g"="Former")) +
      scale_fill_manual(name="Smoker Status", values=c("r" = "red", "g" = "green", "b"="blue"), labels=c("r" = "Never", "b"="Active", "g"="Former")) +
      labs(title = "Blood Cadmium Concentration of Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Count", x = "Blood Cadmium (μg/L)")
  })
  
  output$histpbsmk <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot() + 
      geom_histogram(aes(x=LBXBPB,fill = "r", colour="r"), alpha = 0.4, data=subset(df,truesmkstat == '0'), binwidth = 0.5) +
      geom_histogram(aes(x=LBXBPB,fill = "g", colour="g"), alpha = 0.4, data=subset(df,truesmkstat == '1'), binwidth = 0.5) +
      geom_histogram(aes(x=LBXBPB,fill = "b", colour="b"), alpha = 0.4, data=subset(df,truesmkstat == '2'), binwidth = 0.5) +
      scale_colour_manual(name="Smoker Status", values=c("r" = "red", "g" = "green", "b"="blue"), labels=c("r" = "Never", "b"="Active", "g"="Former")) +
      scale_fill_manual(name="Smoker Status", values=c("r" = "red", "g" = "green", "b"="blue"), labels=c("r" = "Never", "b"="Active", "g"="Former")) +
      labs(title = "Blood Lead Concentration of Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Count", x = "Blood Lead (μg/L)")
  })
  
  output$histcogsex <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot() + 
      geom_histogram(aes(x=cogcompz_new,fill = "g", colour="g"), alpha = 0.4, data=subset(df,RIAGENDR == '1'), binwidth = 0.1) +
      geom_histogram(aes(x=cogcompz_new,fill = "b", colour="b"), alpha = 0.4, data=subset(df,RIAGENDR == '2'), binwidth = 0.1) +
      scale_colour_manual(name="Sex", values=c("g" = "green", "b"="blue"), labels=c("b"="Females", "g"="Males")) +
      scale_fill_manual(name="Sex", values=c("g" = "green", "b"="blue"), labels=c("b"="Females", "g"="Males")) +
      labs(title = "Composite Cognitive Standardized Z-Scores among Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Count", x = "Standardized Composite Z-Score")
  })
  
  output$histcogsmk <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot() +
      geom_histogram(aes(x=cogcompz_new,fill = "r", colour="r"), alpha = 0.4, data=subset(df,truesmkstat == '0'), binwidth = 0.1) +
      geom_histogram(aes(x=cogcompz_new,fill = "g", colour="g"), alpha = 0.4, data=subset(df,truesmkstat == '1'), binwidth = 0.1) +
      geom_histogram(aes(x=cogcompz_new,fill = "b", colour="b"), alpha = 0.4, data=subset(df,truesmkstat == '2'), binwidth = 0.1) +
      scale_colour_manual(name="Smoker Status", values=c("r" = "red", "g" = "green", "b"="blue"), labels=c("r" = "Never", "b"="Active", "g"="Former")) +
      scale_fill_manual(name="Smoker Status", values=c("r" = "red", "g" = "green", "b"="blue"), labels=c("r" = "Never", "b"="Active", "g"="Former")) +
      labs(title = "Composite Cognitive Standardized Z-Scores in Adults 60+ Years Old,\nNHANES 2011-2014. N=1806",
           y = "Count", x = "Standardized Composite Z-Score")
  })
  
  output$scatcdcog <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot(df, aes(x = LBXBCD, y = cogcompz_new)) +
      geom_point(color="dark green") +
      geom_smooth(model = lm) +
      labs(title = "Composite Cognitive Standardized Z-Scores vs Blood Cadmium Levels in Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Composite Cognitive Z-Score", x = "Blood Cadmium (μg/L)")
  })
  
  output$scatpbcog <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot(df, aes(x = LBXBPB, y = cogcompz_new)) +
      geom_point(color="dark blue") +
      geom_smooth(model = lm) +
      labs(title = "Composite Cognitive Standardized Z-Scores vs Blood Lead Levels in Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Composite Cognitive Z-Score", x = "Blood Lead (μg/dL)")
  })
  
  output$scatcdpb <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot(df, aes(x = LBXBPB, y = LBXBCD)) + 
      geom_point(color="dark red") +
      #geom_smooth(model = lm) +
      labs(title = "Blood Cadmium vs Blood Lead Levels in Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Blood Cadmium (μg/L)", x = "Blood Lead (μg/dL)") +
      annotate("text", x=15,  y=3.0, color = "dark blue", label= "Pearson correlation test: r=0.25, p-value < 2.2e-16", size = 6)
  })
  
  output$scatcdage <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot(df, aes(x = RIDAGEYR, y = LBXBCD)) +
      geom_point(color="light blue") +
      geom_smooth(model = lm) +
      labs(title = "Blood Cadmium vs Age in Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Blood Cadmium (μg/L)", x = "Age")
  })
  
  output$scatpbage <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot(df, aes(x = RIDAGEYR, y = LBXBPB)) +
      geom_point(color="light green") +
      geom_smooth(model = lm) +
      labs(title = "Blood Lead vs Age in Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Blood Lead (μg/dL)", x = "Age")
  })
  
  output$scatcogage <- renderPlot({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ggplot(df, aes(x = RIDAGEYR, y = cogcompz_new)) +
      geom_point(color="pink") +
      geom_smooth(model = lm) +
      labs(title = "Composite Cognitive Standardized Z-Scores vs Age in Adults 60+ Years Old, NHANES 2011-2014. N=1806",
           y = "Composite Cognitive Z-Score", x = "Age")
  })
  
  # Generate summares of the models ----
  output$summary_cdbasemodel <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    model1 <- glm(formula = lowcog ~ LBXBCD, data = df, family = binomial)
    summary(model1) 
  })
  
  output$summary_pbbasemodel <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    model2 <- glm(formula = lowcog ~ LBXBPB, data = df, family = binomial)
    summary(model2) 
  })
  
  # Stepwise model selection starting with saturated cog vs pb model
  output$stepwise_select <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    sat_model <- glm(formula = lowcog ~ LBXBPB + RIDAGEYR + RIDAGEYR*RIDAGEYR + factor(marStat) + 
                       factor(edulev) + factor(povInc_cat) + factor(truesmkstat) + factor(RIDRETH3) + 
                       LBXBCD + LBXBPB*LBXBCD, data = df, family = binomial)
    stepwise <- step(sat_model)
    tidy(stepwise)
  })
  
  output$exp_results <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    final_model <- glm(formula = lowcog ~ LBXBPB + RIDAGEYR + factor(edulev) +  
                         factor(RIDRETH3), data = df, family = binomial)
    exp(coef(final_model))
  })
  
  output$summary_stratmale <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    model_males <- glm(formula = lowcog ~ LBXBPB + RIDAGEYR + factor(edulev) +  
                         factor(RIDRETH3), data = df, family = binomial, 
                       subset= RIAGENDR=="1") # stratified by male
    exp(coef(model_males))
  })
  
  output$summary_stratfemale <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    model_females <- glm(formula = lowcog ~ LBXBPB + RIDAGEYR + factor(edulev) +  
                           factor(RIDRETH3), data = df, family = binomial, 
                         subset= RIAGENDR=="2") # stratified by female
    exp(coef(model_females))
  })
  
  output$summary_stratnvrsmk <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    model_nevsmk <- glm(formula = lowcog ~ LBXBPB + RIDAGEYR + factor(edulev) +  
                          factor(RIDRETH3), data = df, family = binomial, 
                        subset= truesmkstat=="0") # stratified by neversmk
    exp(coef(model_nevsmk))
  })
  
  output$summary_stratactsmk <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    model_activsmk <- glm(formula = lowcog ~ LBXBPB + RIDAGEYR + factor(edulev) +  
                            factor(RIDRETH3), data = df, family = binomial, 
                          subset= truesmkstat=="1") # stratified by activesmk
    exp(coef(model_activsmk))
  })
  
  output$summary_stratfmrsmk <- renderPrint({
    req(input$file1) 
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    model_formersmk <- glm(formula = lowcog ~ LBXBPB + RIDAGEYR + factor(edulev) +  
                             factor(RIDRETH3), data = df, family = binomial, 
                           subset= truesmkstat=="2") # stratified by formersmk
    exp(coef(model_formersmk))
  })
  
  output$image1 <- renderImage({
    return(list(  
      src = "images/missingness.jpg",
      filetype = "image/jpeg",
      alt = "This is a flowchart showing inclusion of observations analzyed"
    ))
    
  },deleteFile = FALSE)
  
  
}