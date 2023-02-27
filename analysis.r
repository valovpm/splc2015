library(dplyr)
library(ggplot2)
library(randtoolbox)
library(sensitivity)
library(stringr)
library(sqldf)
library(tidyr)
library(xtable)

source("data.r")
source("monitor.r")
source("regmodels.r")

Analyse <- function() {
  # Performs analysis of configurable software systems using different
  # regression methods

  # Analyse systems using regression methods
  for (m in 1:length(methodNames)) {
    for (s in 1:length(systemNames)) {

      # Perform data analysis
      sysDataInfo <- GetSysDataInfo(s)
      methodInfo <- GetMethodInfo(m)
      accuracyData <- AnalysePredError(sysDataInfo, methodInfo)

      # Export analysis results
      accuracyFile <- file.path(outputFolder,
                                paste(methodNames[m], "_",
                                      systemNames[s], ".csv", sep = ""))
      write.csv(accuracyData, file = accuracyFile, row.names = FALSE)
    }
  }
  
  # Aggregate all results into final comparison table
  SummariseResults()
}

AnalysePredError <- function(sysDataInfo, methodInfo) {
  # Performs regression analysis of the given configurable software systems
  # using specified regression method
  # 
  # Args:
  #   sysDataInfo: system data information (sample size, samples, etc)
  #   methodInfo: method information (sobol sequence, parameter sequence, etc)
  Monitor()
  Monitor(sysDataInfo, 3)
  Monitor(methodInfo, 3)

  # Initialize dataset for storing experiment results
  resultData <- data.frame()
  resultFormat <- c("SamplingSize", "SobolSampleID")
  resultFormat <- c(resultFormat, methodsParams[[methodInfo$methodID]]$names)
  resultFormat <- c(resultFormat, "MeanRowsError", "SdRowsError")
  resultData <- rbind(resultData, 1:length(resultFormat))
  colnames(resultData) <- resultFormat

  resultData <- tbl_df(resultData)
  Monitor(resultData, 3)

  # Iterate over all different sampling sizes (N, 2N, 3N...)
  for (sampleSizeID in 1:experParams$sampleSizes) {
    
    minError <- 10^6

    # Iterate over all different Sobol-sampled parameters
    for (sobolIter in 1:methodsParams[[methodInfo$methodID]]$sobolLen) {
      Monitor(paste("Method: ", methodInfo$methodName, "; ",
                    "System: ", sysDataInfo$sysName, "; ",
                    "Sample size ID: ", sampleSizeID, "; ",
                    "Sobol iteration: ", sobolIter,
                    sep = ""), 2)

      errorData <- NULL

      # Convert Sobol parameters to method parameters
      methodParams <- GetMethodParams(methodInfo, sobolIter,
                                      sysDataInfo, sampleSizeID)
      
      # Iterate over all different samples of the same size
      for (sampleID in 1:experParams$sampleRep) {

        Monitor("Starting regression model training", 3)
        
        error <- GetPredError(methodInfo, methodParams,
                              sysDataInfo, sampleSizeID, sampleID)

        Monitor("Regression model is trained", 3)

        if (is.null(errorData)) {
          errorData <- data.frame(row.names = 1:length(error))
        }
        errorData <- cbind(errorData, error)
        
      } # for (sample in 1:experParams$sampleRep)
      
      # Process all results
      meanRowsError <- mean(rowMeans(errorData))
      sdRowsError <- sd(rowMeans(errorData))

      # Generate data row that contains summary of experiment iteration:
      #   1) Add data sample size and ID of parameters Sobol sample
      #   2) Add actual parameters of Sobol sample
      #   3) Add actual iteration results
      resultRow <- c(sysDataInfo$sampleSizes[sampleSizeID], sobolIter)
      resultRow <- c(resultRow, methodParams)
      resultRow <- c(resultRow, meanRowsError, sdRowsError)

      # Monitor results
      # Monitor("Results for Sobol iteration:", 2)
      # Monitor(resultRow, 2)

      # Add to other results
      resultData <- rbind(resultData, resultRow)
      
    } # for (sobolIter in 1:experParams$sobolLen)
  } # for (sampleSize in experParams$sampleSizes)

  # Remove pseudo-row from the beginning
  resultData <- resultData[-1, ]

  Monitor("Results", 3)
  Monitor(resultData, 3)

  return (resultData)
}

GetPredError <- function(methodInfo, methodParams, sysDataInfo, sampleSizeID, sampleID) {
  # Generates regression model using specified method (CART, Bagging, etc)
  # 
  # Args:
  #   methodID: ID of the method to be used
  #   methodParams: method paramters to be used in training
  #   sysDataInfo: data info of the target configurable system
  #   sampleSizeID: ID of the sample size to be used from data info
  #   sampleID: ID of the particular sample to be used
  Monitor("Method Parameters:", 2)
  Monitor(methodParams, 2)

  # Build the training/test datasets
  set.seed(1)
  trainObs <- sysDataInfo$samples[[sampleSizeID]][[sampleID]]
  testObs <- setdiff(seq_len(nrow(sysDataInfo$sysData)), trainObs)

  Monitor("Training and testing samples", 2)
  Monitor(trainObs, 2)
  Monitor(testObs, 2)

  # Select input and target system features
  input <- setdiff(colnames(sysDataInfo$sysData), "PERF")
  target <- "PERF"

  Monitor("Input and target system features:", 2)
  Monitor(input, 2)
  Monitor(target, 2)

  # Train regression model
  trainData <- sysDataInfo$sysData[trainObs, ]
  model <- Trainer(methodInfo$methodName, methodParams, trainData)

  # Predict system performance and get actual performance values
  predicted <- predict(model, newdata = sysDataInfo$sysData[testObs, input])
  actual <- subset(sysDataInfo$sysData[testObs, ], select = c("PERF"))

  Monitor("Predicted and actual system performance from training sample", 2)
  Monitor(predicted, 2)
  Monitor(t(actual), 2)

  # Calculate relative error
  error <- abs(actual - predicted) / actual * 100

  Monitor("Relative Error")
  Monitor(t(error), 2)

  return(error)
}

SummariseResults <- function() {
  
  plotData <- NULL
  
  # Aggregate data for plot
  for (m in 1:length(methodNames)) {
    for (s in 1:length(systemNames)) {
      
      # Generate data path
      dataPath <- file.path("data", "results")
      dataPath <- file.path(dataPath,
                            paste(methodNames[m], "_",
                                  systemNames[s], ".csv", sep = ""))
      
      # Load method/system data
      tempData <- read.csv(dataPath,
                           na.strings = c(".", "NA", "", "?"),
                           strip.white = TRUE, encoding = "UTF-8")
      
      # Filter data
      tempData <- select_(tempData,
                          .dots = list("SamplingSize", "SobolSampleID",
                                       "MeanRowsError", "SdRowsError"))
      
      # Apply new factor labels
      tempData[, "SamplingSize"] <-
        factor(tempData[,"SamplingSize"],
               labels = c("N", "2*N", "3*N", "4*N", "5*N"))
      
      # Add system column
      tempData <- cbind(systemNames[s], tempData)
      colnames(tempData)[1] <- "system"
      
      # Add method column
      tempData <- cbind(methodNames[m], tempData)
      colnames(tempData)[1] <- "method"
      
      # Update aggregated data
      if (is.null(plotData)) {
        plotData <- tempData
      }
      else {
        plotData <- bind_rows(plotData, tempData)
      }
      
    } # for (s in 1:length(systemNames)) {
  } # for (m in 1:length(methodNames)) {
  
  plotData <- tbl_df(plotData)
  print(plotData)
  
  grpCols <- c("system", "method", "SamplingSize")
  dots <- lapply(grpCols, as.symbol)
  
  avgGrpCols <- c("method", "SamplingSize")
  avgDots <- lapply(avgGrpCols, as.symbol)
  
  # Latex table ###############################################################
  avgData <-
    plotData %>%
    
    group_by_(.dots = dots) %>%
    summarise(bestVal = min(MeanRowsError),
              bestStd = min(SdRowsError),
              avgVal  = mean(MeanRowsError),
              avgStd  = mean(SdRowsError),
              wrstVal = max(MeanRowsError),
              wrstStd = max(SdRowsError)) %>%
    print() %>%
    
    group_by_(.dots = avgDots) %>%
    summarise(bestVal = format(round(mean(bestVal), 2), nsmall = 2),
              bestStd = format(round(mean(bestStd), 2), nsmall = 2),
              avgVal  = format(round(mean(avgVal), 2),  nsmall = 2),
              avgStd  = format(round(mean(avgStd), 2),  nsmall = 2),
              wrstVal = format(round(mean(wrstVal), 2), nsmall = 2),
              wrstStd = format(round(mean(wrstStd), 2), nsmall = 2)) %>%
    cbind("Average")
  
  colnames(avgData)[ncol(avgData)] <- "system"
  print(avgData)
  
  
  
  tempData <-
    plotData %>%
    
    group_by_(.dots = dots) %>%
    summarise(bestVal = format(round(min(MeanRowsError), 2),  nsmall = 2),
              bestStd = format(round(min(SdRowsError), 2),    nsmall = 2),
              avgVal  = format(round(mean(MeanRowsError), 2), nsmall = 2),
              avgStd 	= format(round(mean(SdRowsError), 2),   nsmall = 2),
              wrstVal = format(round(max(MeanRowsError), 2),  nsmall = 2),
              wrstStd	= format(round(max(SdRowsError), 2),    nsmall = 2)) %>%
    print() %>%
    
    bind_rows(avgData) %>%
    print() %>%
    
    unite(best, c(bestVal, bestStd), sep = "$\\pm$")    %>%
    unite(avg, c(avgVal, avgStd), sep = "$\\pm$")       %>%
    unite(wrst, c(wrstVal, wrstStd), sep = "$\\pm$")    %>%
    unite(best_avg_wrst, c(best, avg, wrst), sep = "_") %>%
    print() %>%
    
    spread(method, best_avg_wrst) %>%
    print() %>%
    
    separate("CART",    into = c("cart_best", "cart_avg", "cart_wrst"), sep = "_") %>%
    separate("BAGGING", into = c("bagg_best", "bagg_avg", "bagg_wrst"), sep = "_") %>%
    separate("FOREST",  into = c("frst_best", "frst_avg", "frst_wrst"), sep = "_") %>%
    separate("ESVR",    into = c("esvr_best", "esvr_avg", "esvr_wrst"), sep = "_") %>%
    print() %>%
    
    select(system, SamplingSize,
           cart_best, bagg_best, frst_best, esvr_best,
           cart_avg, bagg_avg, frst_avg, esvr_avg,
           cart_wrst, bagg_wrst, frst_wrst, esvr_wrst)
  
  print(tempData)
  tablePath <- file.path("data", "results", "results_with_std_for_latex.csv")
  write.table(tempData, tablePath, sep = ",", col.names = NA)
  
  # Excel table ###############################################################
  tempData <-
    plotData %>%
    
    group_by_(.dots = dots) %>%
    summarise(bestVal = format(round(min(MeanRowsError), 2), nsmall = 2),
              avgVal  = format(round(mean(MeanRowsError), 2), nsmall = 2),
              wrstVal = format(round(max(MeanRowsError), 2), nsmall = 2)) %>%
    print() %>%
    
    unite(best_avg_wrst, c(bestVal, avgVal, wrstVal), sep = "_") %>%
    print() %>%
    
    spread(method, best_avg_wrst) %>%
    print() %>%
    
    separate("CART",    into = c("cart_best", "cart_avg", "cart_wrst"), sep = "_") %>%
    separate("BAGGING", into = c("bagg_best", "bagg_avg", "bagg_wrst"), sep = "_") %>%
    separate("FOREST",  into = c("frst_best", "frst_avg", "frst_wrst"), sep = "_") %>%
    separate("ESVR",    into = c("esvr_best", "esvr_avg", "esvr_wrst"), sep = "_") %>%
    print() %>%
    
    select(system, SamplingSize,
           cart_best, bagg_best, frst_best, esvr_best,
           cart_avg, bagg_avg, frst_avg, esvr_avg,
           cart_wrst, bagg_wrst, frst_wrst, esvr_wrst)
  
  print(tempData)
  tablePath <- file.path("data", "results", "results.csv")
  write.table(tempData, tablePath, sep = ",", col.names = NA)

  # Latex table ###############################################################
  tempData <-
    plotData %>%
    
    group_by_(.dots = dots) %>%
    summarise(bestVal = format(round(min(MeanRowsError), 2), nsmall = 2),
              bestStd = format(round(min(SdRowsError), 2), nsmall = 2),
              avgVal  = format(round(mean(MeanRowsError), 2), nsmall = 2),
              avgStd  = format(round(mean(SdRowsError), 2), nsmall = 2),
              wrstVal = format(round(max(MeanRowsError), 2), nsmall = 2),
              wrstStd = format(round(max(SdRowsError), 2), nsmall = 2)) %>%
    print() %>%
    
    unite(best, c(bestVal, bestStd), sep = "+/-")    %>%
    unite(avg, c(avgVal, avgStd), sep = "+/-")       %>%
    unite(wrst, c(wrstVal, wrstStd), sep = "+/-")    %>%
    unite(best_avg_wrst, c(best, avg, wrst), sep = "_") %>%
    print() %>%
    
    spread(method, best_avg_wrst) %>%
    print() %>%
    
    separate("CART",    into = c("cart_best", "cart_avg", "cart_wrst"), sep = "_") %>%
    separate("BAGGING", into = c("bagg_best", "bagg_avg", "bagg_wrst"), sep = "_") %>%
    separate("FOREST",  into = c("frst_best", "frst_avg", "frst_wrst"), sep = "_") %>%
    separate("ESVR",    into = c("esvr_best", "esvr_avg", "esvr_wrst"), sep = "_") %>%
    print() %>%
    
    select(system, SamplingSize,
           cart_best, bagg_best, frst_best, esvr_best,
           cart_avg, bagg_avg, frst_avg, esvr_avg,
           cart_wrst, bagg_wrst, frst_wrst, esvr_wrst)
  
  print(tempData)
  tablePath <- file.path("data", "results", "results_with_std.csv")
  write.table(tempData, tablePath, sep = ",", 
              col.names = NA, fileEncoding="UTF-8")
}


