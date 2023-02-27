library(rpart)
library(randomForest)
library(gbm)
library(kernlab)

source("monitor.r")

GetMethodInfo <- function(methodID) {
  # Performs Sobol sampling of parameters of the specified regression method
  # 
  # Args:
  #   methodID: ID of the regression method to be sampled

  # Generate Sobol sequence for the specified regression method
  sobolSeq <- randtoolbox::sobol(n = methodsParams[[methodID]]$sobolLen,
                                 dim = length(methodsParams[[methodID]]$names),
                                 init = TRUE,
                                 scrambling = 1,
                                 seed = 1)

  paramSeq <- NULL

  # Convert values in Sobol sequence to actual parameter values
  for (j in 1:ncol(sobolSeq)) {

    # Extract parameter properties
    minVal <- methodsParams[[methodID]]$minVals[j]
    maxVal <- methodsParams[[methodID]]$maxVals[j]
    dgts   <- methodsParams[[methodID]]$dgts[j]

    # Convert Sobol sequence:
    #   1) Select all Sobol numbers for the parameter
    #   2) Convert Sobol numbers to parameter range
    #   3) Add converted numbers to final parameter sequence
    paramCol <- sobolSeq[, j]
    paramCol <- sapply(paramCol, ConvertSobol, minVal = minVal,
                       maxVal = maxVal, dgts = dgts)
    paramSeq <- cbind(paramSeq, paramCol)
    colnames(paramSeq)[ncol(paramSeq)] <-
      methodsParams[[methodID]]$names[ncol(paramSeq)]
  }

  # Populate methodInfo data structure
  methodInfo <- list()
  methodInfo$methodID <- methodID
  methodInfo$methodName <- methodNames[[methodID]]
  methodInfo$sobolSeq <- sobolSeq

  Monitor(paste("Method ID:",   methodInfo$methodID))
  Monitor(paste("Method Name:", methodInfo$methodName))
  Monitor(paste("Sobol sequence:"))
  Monitor(methodInfo$sobolSeq)

  return(methodInfo)
}

ConvertSobol <- function(minVal, maxVal, sobol, dgts) {
  # Converts number from sobol hypercube to a number from the given parameter
  # range
  #
  # Args:
  #   minVal: minimal value of the target value range
  #   maxVal: maximal value of the target value range
  #   sobol: number from Sobol hypercube to be converted
  #   dgts: number of digits after decimal point
  return(minVal + round((maxVal - minVal) * sobol, dgts))
}

GetMethodParams <- function(methodInfo, sobolIter, sysDataInfo, sampleSizeID, sampleID) {
  # Sobol parameters to convert into method parameters
  sobolParams <- methodInfo$sobolSeq[sobolIter, ]

  # Data properties
  obsNum <- sysDataInfo$sampleSizes[sampleSizeID]
  ftrNum <- sysDataInfo$featureNum

  # Output method parameters
  methodParams <- NULL

  if (methodInfo$methodName == "CART") {
    # minSplit   <- ConvertSobol(2, floor(obsNum / 2), sobolParams[1], 0)
    # minBucket  <- ConvertSobol(1, floor(obsNum / 2), sobolParams[2], 0)
    minSplit   <- ConvertSobol(2, min(floor(obsNum / 2), 10), sobolParams[1], 0)
    minBucket  <- ConvertSobol(1, min(floor(obsNum / 2), 10), sobolParams[2], 0)
    maxDepth   <- ConvertSobol(2, 10, sobolParams[3], 0)
    complexity <- ConvertSobol(0, 0.01, sobolParams[4], 4)

    methodParams <- c(minSplit, minBucket, maxDepth, complexity)
  }

  else if (methodInfo$methodName == "BAGGING") {
    ntree     <- ConvertSobol(2, 20, sobolParams[1], 0)
    # minBucket <- ConvertSobol(1, floor(obsNum / 2), sobolParams[2], 0)
    minBucket <- ConvertSobol(1, min(floor(obsNum / 2), 10), sobolParams[2], 0)

    methodParams <- c(ntree, minBucket)
  }

  else if (methodInfo$methodName == "FOREST") {
    ntree     <- ConvertSobol(2, 20, sobolParams[1], 0)
    # minBucket <- ConvertSobol(1, floor(obsNum / 2), sobolParams[2], 0)
    minBucket <- ConvertSobol(1, min(floor(obsNum / 2), 10), sobolParams[2], 0)
    mtry      <- ConvertSobol(floor(ftrNum / 2), ftrNum - 1, sobolParams[3], 0)

    methodParams <- c(ntree, minBucket, mtry)
  }

  else if (methodInfo$methodName == "ESVR") {
    epsilon <- ConvertSobol(0.01, 1, sobolParams[1], 2)
    sigma   <- ConvertSobol(0.01, 1, sobolParams[2], 2)
    cParam  <- ConvertSobol(1, 100, sobolParams[3], 0)

    methodParams <- c(epsilon, sigma, cParam)
  }

  return(methodParams)
}

# Regression models training methods ##########################################
Trainer <- function(methodName, params, data) {
  #
  #
  # Args:
  #   methodName:
  #   params:
  #   data:

  model <- NULL

  # Select regression method implementation
  regMethod <<- switch(methodName,
                       "CART" = TrainCart,
                       "BAGGING" = TrainBag,
                       "FOREST" = TrainFrst,
                       # "4" = TrainBst,
                       "ESVR" = TrainEsvr)
                       # "6" = TrainNusvr)
  
  # Train regression model
  model <- regMethod(params, data)
  return(model)
}

TrainCart <- function(params, data) {
  #
  #
  # Args:
  #   methodID:
  #   params:
  #   data:

  Monitor()
  Monitor("CART Training params:", 2)
  Monitor(params, 2)

  minSplit   <- params[1]
  minBucket  <- params[2]
  maxDepth   <- params[3]
  complexity <- params[4]

  Monitor(paste("CART. ",
                "minsplit: ", minSplit, "; ",
                "minBucket: ", minBucket, "; ",
                "maxDepth: ", maxDepth, "; ",
                "complexity: ", complexity, "; ",
                sep = ""))

  # Train regression model
  Monitor("Starting CART model training...", 3)
  require(rpart, quietly = TRUE)
    set.seed(1)
    model <-
      rpart(PERF ~ .,
            data = data,
            method = "anova",
            parms = list(split = "information"),
            control = rpart.control(minsplit = minSplit,
                                    minbucket = minBucket,
                                    maxdepth = maxDepth,
                                    cp = complexity,
                                    usesurrogate = 0,
                                    maxsurrogate = 0))
  Monitor("Finished CART model training", 3)

  return(model)
}

TrainBag <- function(params, data) {
  #

  Monitor()

  ntree    <- params[1]
  nodesize <- params[2]
  mtry     <- ncol(data) - 1

  Monitor(paste("BAGGING. ",
                "ntree: ", ntree, "; ",
                "nodesize: ", nodesize, "; ",
                "mtry: ", mtry, "; ",
                sep = ""))

  # Train regression model
  Monitor("Starting BAGGING model training...")
  require(randomForest, quietly = TRUE)
  set.seed(1)
  model <-
    randomForest(PERF ~ .,
                 data = data,

                 ntree = ntree,
                 nodesize = nodesize,
                 mtry = mtry,

                 importance = TRUE,
                 na.action = na.omit,
                 replace = FALSE)
  Monitor("Finished BAGGING model training")

  return(model)
}

TrainFrst <- function(params, data) {
  # 

  Monitor()

  ntree    <- params[1]
  nodesize <- params[2]
  mtry     <- params[3]

  Monitor(paste("FOREST. ",
              "ntree: ", ntree, "; ",
              "nodesize: ", nodesize, "; ",
              "mtry: ", mtry, "; ",
              sep = ""))

  # Train regression model
  Monitor("Starting FOREST model training...")
  require(randomForest, quietly = TRUE)
  set.seed(1)
  model <-
    randomForest(PERF ~ .,
                 data = data,

                 ntree = ntree,
                 nodesize = nodesize,
                 mtry = mtry,

                 importance = TRUE,
                 na.action = na.omit,
                 replace = FALSE)
  Monitor("Finished FOREST model training")

  return(model)
}

TrainEsvr <- function(params, data) {
  # 

  Monitor()

  # Initialize parameters
  epsilon <- params[1]
  sigma   <- params[2]
  C       <- params[3]

  Monitor(paste("SVM. ",
                "epsilon: ", epsilon, "; ",
                "sigma: ", sigma, "; ",
                "C: ", C, "; ",
                sep = ""))

  # Train regression model
  Monitor("Starting ESVR model training...")
  set.seed(1)
  model <-
    ksvm(PERF~.,
         data = data,
         type = "eps-svr",
         kernel = "rbfdot",

         epsilon = epsilon,
         kpar = list(sigma = sigma),
         C = C,

         cross = 10)
  Monitor("Finished ESVR model training")

  return(model)
}

TrainNusvr <- function(params, data) {
  # 

  Monitor()

  # Initialize parameters
  nu <- params[1]
  sigma <- params[2]
  C <- params[3]

  # Train regression model
  Monitor("Starting NUSVR model training...")
  set.seed(seedIter)
  model <-
    ksvm(PERF~.,
         data = data,
         type = "nu-svr",
         kernel = "rbfdot",

         nu = nu,
         kpar = list(sigma = sigma),
         C = C,

         cross = 10)
  Monitor("Finished NUSVR model training")

  return(model)
}


