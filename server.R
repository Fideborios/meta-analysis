library(lme4)
library(shiny)
library(RColorBrewer)
library(ggpubr)
library(metafor)
library(dplyr)
library(plyr)
library(tidyr)
library(foreign)
library(readr)
library(haven)
library(xlsx)
library(broom)
library(ggplot2)


shinyServer(function(input, output, session) {
  #######
  #Importing Data Code#
  ### Argument names:
ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    
    selectInput("arg","Argument:",ArgNames())
  })
  
  ## Arg text field:
output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
    }
  })
# Select variables:
output$varselect <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Variable selection:    
    selectInput(inputId = "vars", label = "Variables to use:", 
                choices = names(Dataset()),selected = names(Dataset())[1:length(names(Dataset()))], multiple =TRUE)            
  })
  
### Data import:
Dataset <- eventReactive(input$choice, {
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call(input$readFunction,
                                     c(list(input$file$datapath),argList)))
    
    

    # Changes in read.table 
    vars <- names(Dataset)
    # Update select input immediately after clicking on the action button. 
    updateSelectInput(session, "slab","Select the trial names ", choices = c("choose", vars))
    
    # Update select input immediately after clicking on the action button. 
    # Dichotomous outcomes
    updateSelectInput(session, "ai","ai: ", choices = c("choose", vars))
    updateSelectInput(session, "bi","bi:" , choices = c("choose", vars))
    updateSelectInput(session, "ci","ci:" , choices = c("choose", vars))
    updateSelectInput(session, "di","di:" , choices = c("choose", vars))
    updateSelectInput(session, "n1i","Total treatment B (n1i):" , choices = c("choose", vars))
    updateSelectInput(session, "n2i","Total treatment A (n2i):" , choices = c("choose", vars))
    
    
    # Update select input immediately after clicking on the action button. 
    # Event outcomes  
    updateSelectInput(session, "x1i","the number of events (first group): ", choices = c("choose", vars))
    updateSelectInput(session, "x2i","the number of events (second group):" , choices = c("choose", vars))
    updateSelectInput(session, "t1i","the total person-times (first group):" , choices = c("choose", vars))
    updateSelectInput(session, "t2i","the total person-times (second group):" , choices = c("choose", vars))

    # Event outcomes  
    updateSelectInput(session, "m1i","the means (first group or time point): ", choices = c("choose", vars))
    updateSelectInput(session, "m2i","the means (second group or time point):" , choices = c("choose", vars))
    updateSelectInput(session, "sd1i","the standard deviations (first group or time point):" , choices = c("choose", vars))
    updateSelectInput(session, "sd2i","the standard deviations (second group or time point):" , choices = c("choose", vars))

    return(Dataset)
  })
  

# Show table:
output$table <- renderTable({
    
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    
    Dataset2 = head(Dataset()[,input$vars,drop=FALSE], n = input$number.of.observations)
    
    return(Dataset2)
  })

escalculator <-  reactive({
    dat <- Dataset()
    ai = dat[,names(dat)== input$ai]
    bi = dat[,names(dat)== input$bi]
    ci = dat[,names(dat)== input$ci]
    di = dat[,names(dat)== input$di]
    n1i = dat[,names(dat)== input$n1i]
    n2i = dat[,names(dat)== input$n2i]
    x1i = dat[,names(dat)== input$x1i]
    t1i = dat[,names(dat)== input$t1i]
    x2i = dat[,names(dat)== input$x2i]
    t2i = dat[,names(dat)== input$t2i]
    m1i = dat[,names(dat)== input$m1i]
    m2i = dat[,names(dat)== input$m2i]
    sd1i = dat[,names(dat)== input$sd1i]
    sd2i = dat[,names(dat)== input$sd2i]
    slab = dat[,names(dat)== input$slab]
    
    if(input$EStype == "Dichotomous" & input$ES == "Odds-ratio"){
  ES = escalc(ai = ai,bi = bi,ci = ci,di = di,n2i = n2i,n1i =  n1i, measure = "OR", slab = slab)
    }else  if(input$EStype == "Dichotomous" & input$ES == "Risk-ratio"){
      ES = escalc(ai = ai,bi = bi,ci = ci,di = di,n2i = n2i,n1i =  n1i, measure = "RR", slab = slab)
    }else  if(input$EStype == "Dichotomous" & input$ES == "Risk difference"){
      ES = escalc(ai = ai,bi = bi,ci = ci,di = di,n2i = n2i,n1i =  n1i, measure = "RD", slab = slab)
    }else  if(input$EStype == "Dichotomous" & input$ES == "Arcsine square root transformed risk difference"){
      ES = escalc(ai = ai,bi = bi,ci = ci,di = di,n2i = n2i,n1i =  n1i, measure = "AS", slab = slab)
    }else if(input$EStype == "Dichotomous" & input$ES == "PETO odds-ratio"){
      ES = escalc(ai = ai,bi = bi,ci = ci,di = di,n2i = n2i,n1i =  n1i, measure = "PETO", slab = slab)
    }else  if(input$EStype == "Event-Counts" & input$ES == "Incidence rate ratio"){
      ES = escalc(x1i = x1i,t1i = t1i,x2i = x2i,t2i = t2i, measure = "IRR", slab = slab)
    }else  if(input$EStype == "Event-Counts" & input$ES == "Incidence rate difference"){
      ES = escalc(x1i = x1i,t1i = t1i,x2i = x2i,t2i = t2i, measure = "IRD", slab = slab)
    }else  if(input$EStype == "Event-Counts" & input$ES == "Square root transformed incidence rate difference"){
      ES = escalc(x1i = x1i,t1i = t1i,x2i = x2i,t2i = t2i, measure = "IRSD", slab = slab)
    }else if(input$EStype == "Continuous" & input$ES == "Mean difference"){
      ES = escalc(m1i = m1i,sd1i = sd1i,m2i = m2i,sd2i = sd2i, measure = "MD", slab = slab)
    }else if(input$EStype == "Continuous" & input$ES == "Standardized mean difference"){
      ES = escalc(m1i = m1i,sd1i = sd1i,m2i = m2i,sd2i = sd2i, measure = "SMD", slab = slab)
    }else if(input$EStype == "Continuous" & input$ES == "Standardized mean difference with heteroscedastic population variances"){
      ES = escalc(m1i = m1i,sd1i = sd1i,m2i = m2i,sd2i = sd2i, measure = "SMDH", slab = slab)
    }
    
    return(ES = ES)
  })

output$escalculator.summary <- renderTable({
    summary=escalculator()
    return(summary)
  })

meta.analysis <- reactive({
    summary <- escalculator()
    if (input$typeofmeta == "Fixed-effect"){
    meta.analysis <- rma(yi, vi, measure = "GEN",method = "FE", data= summary)
    }else{
      meta.analysis <- rma(yi, vi, measure = "GEN",method = input$tau, data= summary)
    }
  })

output$meta.analysis.summary <- renderPrint({
  meta.analysis <- meta.analysis()
  summary(meta.analysis)

})


output$forest <- renderPlot({
  meta.analysis <- meta.analysis()
  forest(meta.analysis)
  
})

output$funnelplot <- renderPlot({
  meta.analysis <- meta.analysis()
  funnel(meta.analysis)
  
})




ForestNames <- reactive({
  For.Names <- names(formals(forest.default))
  For.Names <- For.Names[For.Names!="..."]
  return(For.Names)
})

# Argument selector:
output$ForestSelect <- renderUI({
  if (length(ForestNames())==0) return(NULL)
  
  selectInput("forest.argument","Forest-plot options:",ForestNames())
})

## Arg text field:
output$ForestText <- renderUI({
  fun__arg <- paste0(forest,"__",input$forest.argument)
  
  if (is.null(input$forest.argument)) return(NULL)
  
  Defaults <- formals(forest.default)
  
  if (is.null(input[[fun__arg]]))
  {
    textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
  } else {
    textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
  }
})


  
})