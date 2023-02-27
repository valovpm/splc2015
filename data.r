library(dplyr)
library(tidyr)

source("monitor.r")

GetSysDataInfo <- function(sysID) {
  # Loads data for specified configurable software system
  # 
  # Args:
  #   sysID: ID of the system to be loaded

  # Load system data
  sysDataFile <- file.path("data", "systems",
                           paste(systemNames[sysID], ".csv", sep = ""))

  sysData <- read.csv(sysDataFile,
                      na.strings = c(".", "NA", "", "?"),
                      strip.white = TRUE, encoding = "UTF-8")

  # Non-feature columns: PERF
  features <- colnames(sysData)
  nonFeat <- c("PERF")
  features <- features[!(features %in% nonFeat)]
  featureNum <- length(features)

  # 'samples' is a list of 'sampleNum' lists of length 'sampleRep'
  # 'samples' contains all necessary data samples for analysing the specified
  # system
  samples <- list()
  samples <- c(samples, 1:experParams$sampleSizes)

  sampleSizes <- 1:experParams$sampleSizes
  sampleSizes <- sampleSizes * featureNum

  # Fill 'samples' with newly generated data samples
  # 'i' iterates over lists
  # 'j' iterates over individual samples in lists
  for (i in 1:experParams$sampleSizes) {
    equisizedSamples <- list()

    # Populate new list with 'sampleRep' samples of the same size
    # TODO: check randomization seed
    for (j in 1:experParams$sampleRep) {
      smpl <- sample(nrow(sysData), sampleSizes[i])
      equisizedSamples <- c(equisizedSamples, list(smpl))
    }

    samples[[i]] <- equisizedSamples
  }

  # Initialize list that will contain all system data information
  dataInfo <- list()
  dataInfo$sysID <- sysID                 # Configurable system ID
  dataInfo$sysName <- systemNames[sysID]  # Configurable system name
  dataInfo$sysData <- sysData             # Configurable system data

  dataInfo$features <- features           # Configuration features
  dataInfo$featureNum <- featureNum       # Number of configuration features
  
  dataInfo$sampleSizes <- sampleSizes     # List of sample sizes
  dataInfo$samples <- samples             # List (for each machine) of
                                          #   Lists (for each sample size) of
                                          #     Samples

  Monitor(paste("System ID:", dataInfo$sysID))
  Monitor(paste("System Name:", dataInfo$sysName))
  Monitor(paste("Features:", dataInfo$featureNum))
  Monitor(dataInfo$features)
  Monitor("Sampling sizes:")
  Monitor(dataInfo$sampleSizes)
  Monitor("Samples:")
  Monitor(dataInfo$samples)

  return(dataInfo)
}


