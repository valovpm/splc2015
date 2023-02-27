source("monitor.r")

Init <- function() {
  # Aggregated function for initializing all parameters
  InitMonitorParams()
  InitMethodsParams()
  InitParamsExperiment()
  Monitor("Initialization is complete")
}

InitMonitorParams <- function() {
  # Specifies global parameters used by debugging functions

  # List specifies debugging levels for functions
  # Messages from functions not in the list will not be printed
  # Messages from functions that have negative level will not be printed
  # Messages with level exceeding specified in the list for function will not
  # be printed
  funLevels <<- list(# analysis.r
                     AnalysePredError = 2,
                     GetPredError = -2,

                     # data.r
                     GetSysDataInfo = -2,

                     # init.r
                     Init = 2,

                     # regmodels.r
                     GetMethodInfo = -2,
                     Trainer = 2,
                     TrainCart = 2,

                     # scaling.r
                     GetSameRows = -2,
                     GetScale = -2)
}

InitMethodsParams <- function() {
  # Initializes parameter ranges for CART regression method
  methodNames <<- list(cart = "CART", bag = "BAGGING", frst = "FOREST",
                       esvr = "ESVR")

  # Initialize CART parameters
  # minsplit, minbucket - depend on number of observations
  cartNames   <- c("minsplit", "minbucket", "maxdepth", "complexity")
  cartMinVals <- c(0, 0, 0, 0)
  cartMaxVals <- c(1, 1, 1, 1)
  cartDgts    <- c(4, 4, 4, 4)
  cartSobol   <- 10^4
  cart <- list(names = cartNames, minVals = cartMinVals, maxVals = cartMaxVals,
               dgts = cartDgts, sobolLen = cartSobol)

  # Initialize BAGGING parameters
  # minbucket - depends on number of observations
  bagNames    <- c("ntree", "minbucket")
  bagMinVals  <- c(2,   0)
  bagMaxVals  <- c(20,  1)
  bagDgts     <- c(0,   4)
  bagSobol    <- 10^2
  bag <- list(names = bagNames, minVals = bagMinVals, maxVals = bagMaxVals,
              dgts = bagDgts, sobolLen = bagSobol)

  # Initialize FOREST parameters
  # minbucket - depends on number of observations
  # mtry - depends on number of features
  frstNames   <- c("ntree", "minbucket", "mtry")
  frstMinVals <- c(2,  0, 0)
  frstMaxVals <- c(20, 1, 1)
  frstDgts    <- c(0,  4, 4)
  frstSobol   <- 10^3
  frst <- list(names = frstNames, minVals = frstMinVals, maxVals = frstMaxVals,
               dgts = frstDgts, sobolLen = frstSobol)

  # Initialize ESVR parameters
  esvrNames   <- c("epsilon", "sigma", "c")
  esvrMinVals <- c(0.01,  0.01, 1)
  esvrMaxVals <- c(1,     1,    100)
  esvrDgts    <- c(2,     2,    0)
  esvrSobol   <- 10^3
  esvr <- list(names = esvrNames, minVals = esvrMinVals, maxVals = esvrMaxVals,
               dgts = esvrDgts, sobolLen = esvrSobol)

  methodsParams <<- list(cart = cart, bag = bag, frst = frst,
                         esvr = esvr)
}

InitParamsExperiment <- function() {
  # Initializes parameters relevant to the whole experiment
  systemNames <<- c("Apache", "BerkeleyC", "BerkeleyJ",
                    "LLVM", "Sqlite", "x264")

  experParams <<- list()
  experParams$sampleSizes <<- 5   # Sizes to generate (1*N, 2*N, ..., 5*N)
  experParams$sampleRep <<- 10    # Number of equisized samples to generate

  outputFolder <<- file.path("data", "results")
}


