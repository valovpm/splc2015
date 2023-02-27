library(dplyr)
library(ggplot2)
library(tidyr)

source(file = "init.r")

PlotBoxGrid <- function() {

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

      # Print
      tempData <-tbl_df(tempData)
      # print(tempData)

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
  plotData$method <- as.factor(plotData$method)
  print(plotData$method)

  systems01 <- c("Apache", "BerkeleyC", "BerkeleyJ")
  systems02 <- c("LLVM", "Sqlite", "x264")
  systems03 <- c("Apache", "BerkeleyC", "BerkeleyJ",
                 "LLVM", "Sqlite", "x264")

  plotData01 <- filter(plotData, system %in% systems01)
  plotData02 <- filter(plotData, system %in% systems02)
  plotData03 <- filter(plotData, system %in% systems03)

  plotPath01 <- file.path("data", "results", "FACET_BOXPLOT_01.png")
  plotPath02 <- file.path("data", "results", "FACET_BOXPLOT_02.png")
  plotPath03 <- file.path("data", "results", "FACET_BOXPLOT_03.png")

  # Generate plot 01
  dataPlot <-
    ggplot(data = plotData01,
           aes_string(x = "SamplingSize", y = "MeanRowsError",
                      fill = "SamplingSize")) +
    stat_boxplot(geom ='errorbar') +
    geom_boxplot() +
    scale_fill_grey(start = 0.5, end = 1) +
    facet_grid(system~method, scales = "free") +
    xlab("Sampling Size") +
    ylab("Relative Error (%)") +
    guides(fill = FALSE)
  
  ggsave(dataPlot, filename = plotPath01, width = 8.5, height = 11)

  # Generate plot 02
  dataPlot <-
    ggplot(data = plotData02,
           aes_string(x = "SamplingSize", y = "MeanRowsError",
                      fill = "SamplingSize")) +
    stat_boxplot(geom ='errorbar') +
    geom_boxplot() +
    scale_fill_grey(start = 0.5, end = 1) +
    facet_grid(system~method, scales = "free") +
    xlab("Sampling Size") +
    ylab("Relative Error (%)") +
    guides(fill = FALSE)
  
  ggsave(dataPlot, filename = plotPath02, width = 8.5, height = 11)

  # Generate plot 03
  dataPlot <-
    ggplot(data = plotData03,
           aes_string(x = "SamplingSize", y = "MeanRowsError",
                      fill = "SamplingSize")) +
    stat_boxplot(geom ='errorbar') +
    geom_boxplot() +
    scale_fill_grey(start = 0.5, end = 1) +
    facet_grid(system~method, scales = "free") +
    xlab("Sampling Size") +
    ylab("Relative Error (%)") +
    guides(fill = FALSE)
  
  ggsave(dataPlot, filename = plotPath03, width = 8.5, height = 11)
}


