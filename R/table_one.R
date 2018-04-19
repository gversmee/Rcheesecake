##' Create an object summarizing both continuous and categorical variables
##'
##' Create an object summarizing all baseline variables (both continuous and categorical) optionally stratifying by one or more startifying variables and performing statistical tests. The object gives a table that is easy to use in medical research papers.
##'
##' @param vars Variables to be summarized given as a character vector. Factors are handled as categorical variables, whereas numeric variables are handled as continuous variables. If empty, all variables in the data frame specified in the data argument are used.
##' @param strata Stratifying (grouping) variable name(s) given as a character vector. If omitted, the overall results are returned.
##' @param data A data frame in which these variables exist. All variables (both vars and strata) must be in this data frame.
##' @param factorVars Numerically coded variables that should be handled as categorical variables given as a character vector. Do not include factors, unless you need to relevel them by removing empty levels. If omitted, only factors are considered categorical variables. The variables specified here must also be specified in the \code{vars} argument.
##' @param includeNA If TRUE, NA is handled as a regular factor level rather than missing. NA is shown as the last factor level in the table. Only effective for categorical variables.
##' @param test If TRUE, as in the default and there are more than two groups, groupwise comparisons are performed.
##' @param testNormal A function used to perform the normal assumption based tests. The default is \code{oneway.test}. This is equivalent of the t-test when there are only two groups.
##' @param argsNormal A named list of arguments passed to the function specified in \code{testNormal}. The default is \code{list(var.equal = TRUE)}, which makes it the ordinary ANOVA that assumes equal variance across groups.
##' @param testNonNormal A function used to perform the nonparametric tests. The default is \code{kruskal.test} (Kruskal-Wallis Rank Sum Test). This is equivalent of the wilcox.test (Man-Whitney U test) when there are only two groups.
##' @param argsNonNormal A named list of arguments passed to the function specified in \code{testNonNormal}. The default is \code{list(NULL)}, which is just a placeholder.
##' @param testApprox A function used to perform the large sample approximation based tests. The default is \code{chisq.test}. This is not recommended when some of the cell have small counts like fewer than 5.
##' @param argsApprox A named list of arguments passed to the function specified in testApprox. The default is \code{list(correct = TRUE)}, which turns on the continuity correction for \code{chisq.test}.
##' @param testExact A function used to perform the exact tests. The default is \code{fisher.test}. If the cells have large numbers, it will fail because of memory limitation. In this situation, the large sample approximation based should suffice.
##' @param argsExact A named list of arguments passed to the function specified in testExact. The default is \code{list(workspace = 2*10^5)}, which specifies the memory space allocated for \code{fisher.test}.
##' @param smd If TRUE, as in the default and there are more than two groups, standardized mean differences for all pairwise comparisons are calculated.
##'
##' @details The definitions of the standardized mean difference (SMD) are available in \href{http://www.tandfonline.com/doi/abs/10.1080/00031305.1986.10475403}{Flury \emph{et al} 1986} for the univariate case and the multivariate case (essentially the square root of the Mahalanobis distance). Extension to binary variables is discussed in \href{http://www.tandfonline.com/doi/abs/10.1080/03610910902859574}{Austin 2009} and extension to multinomival variables is suggested in \href{http://support.sas.com/resources/papers/proceedings12/335-2012.pdf}{Yang \emph{et al} 2012}. This multinomial extesion treats a single multinomial variable as multiple non-redundant dichotomous variables and use the Mahalanobis distance. The off diagonal elements of the covariance matrix on page 3 have an error, and need negation. In weighted data, the same definitions can be used except that the mean and standard deviation estimates are weighted estimates (\href{http://www.ncbi.nlm.nih.gov/pubmed/23902694}{Li \emph{et al} 2013} and \href{http://onlinelibrary.wiley.com/doi/10.1002/sim.6607/full}{Austin \emph{et al} 2015}). In tableone, all weighted estimates are calculated by weighted estimation functions in the \code{survey} package.
##'
##' @return An object of class \code{TableOne}, which is a list of three objects.
##' @return \item{ContTable}{object of class \code{ContTable}, containing continuous variables only}
##' @return \item{CatTable}{object of class \code{CatTable}, containing categorical variables only}
##' @return \item{MetaData}{list of metadata regarding variables}
##'
##' @references Flury, BK. and Riedwyl, H. (1986). Standard distance in univariate and multivariate analysis. \emph{The American Statistician}, \bold{40}, 249-251.
##' @references Austin, PC. (2009). Using the Standardized Difference to Compare the Prevalence of a Binary Variable Between Two Groups in Observational Research. \emph{Communications in Statistics - Simulation and Computation}, \bold{38}, 1228-1234.
##' @references Yang, D. and Dalton, JE. (2012). A unified approach to measuring the effect size between two groups using SAS. SAS Global Forum 2012, Paper 335-2012.
##' @references Li, L. and Greene, T. (2013). A weighting analogue to pair matching in propensity score analysis. \emph{International Journal of Biostatistics}, \bold{9}, 215-234.
##' @references Austin, PC. and Stuart, EA. (2015). Moving towards best practice when using inverse probability of treatment weighting (IPTW) using the propensity score to estimate causal treatment effects in observational studies. \emph{Statistics in Medicine}, Online on August 3, 2015.
##'
##' @author Kazuki Yoshida, Justin Bohn
##' @seealso
##' \code{\link{print.TableOne}}, \code{\link{summary.TableOne}}
##' @examples
##'
##' ## Load
##' library(tableone)
##'
##' ## Load Mayo Clinic Primary Biliary Cirrhosis Data
##' library(survival)
##' data(pbc)
##' ## Check variables
##' head(pbc)
##'
##' ## Make categorical variables factors
##' varsToFactor <- c("status","trt","ascites","hepato","spiders","edema","stage")
##' pbc[varsToFactor] <- lapply(pbc[varsToFactor], factor)
##'
##' ## Create a variable list
##' dput(names(pbc))
##' vars <- c("time","status","age","sex","ascites","hepato",
##'           "spiders","edema","bili","chol","albumin",
##'           "copper","alk.phos","ast","trig","platelet",
##'           "protime","stage")
##'
##' ## Create Table 1 stratified by trt
##' tableOne <- CreateTableOne(vars = vars, strata = c("trt"), data = pbc)
##'
##' ## Just typing the object name will invoke the print.TableOne method
##' tableOne
##'
##' ## Specifying nonnormal variables will show the variables appropriately,
##' ## and show nonparametric test p-values. Specify variables in the exact
##' ## argument to obtain the exact test p-values. cramVars can be used to
##' ## show both levels for a 2-level categorical variables.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), cramVars = "hepato", smd = TRUE)
##'
##' ## Use the summary.TableOne method for detailed summary
##' summary(tableOne)
##'
##' ## See the categorical part only using $ operator
##' tableOne$CatTable
##' summary(tableOne$CatTable)
##'
##' ## See the continuous part only using $ operator
##' tableOne$ContTable
##' summary(tableOne$ContTable)
##'
##' ## If your work flow includes copying to Excel and Word when writing manuscripts,
##' ## you may benefit from the quote argument. This will quote everything so that
##' ## Excel does not mess up the cells.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), quote = TRUE)
##'
##' ## If you want to center-align values in Word, use noSpaces option.
##' print(tableOne, nonnormal = c("bili","chol","copper","alk.phos","trig"),
##'       exact = c("status","stage"), quote = TRUE, noSpaces = TRUE)
##'
##' ## If SMDs are needed as numericals, use ExtractSmd()
##' ExtractSmd(tableOne)
##'
##' @export CreateTableOne



CreateTableOne <-
  function(vars,                                      # character vector of variable names
           strata,                                    # character vector of variable names
           data,                                      # data frame
           factorVars,                                # variables to be transformed to factors
           includeNA     = FALSE,                     # include NA as a category (categoricals only)
           test          = TRUE,                      # whether to include p-values
           ## Test configuration for categorical data
           testApprox    = chisq.test,                # function for approximation test
           argsApprox    = list(correct = TRUE),      # arguments passed to testApprox
           testExact     = fisher.test,               # function for exact test
           argsExact     = list(workspace = 2*10^5),  # arguments passed to testExact
           ## Test configuration for continuous data
           testNormal    = oneway.test,               # test for normally distributed variables
           argsNormal    = list(var.equal = TRUE),    # arguments passed to testNormal
           testNonNormal = kruskal.test,              # test for nonnormally distributed variables
           argsNonNormal = list(NULL),                # arguments passed to testNonNormal
           smd           = TRUE                       # whether to include standardize mean differences
  ) {
    
    ### Data check
    ## Check if the data given is a dataframe
    ModuleStopIfNotDataFrame(data)
    
    ## Check if vars argument is missing. If so, add all names in data.
    if (missing(vars)) {
      vars <- names(data)
    }
    
    ## Check if variables exist. Drop them if not.
    vars <- ModuleReturnVarsExist(vars, data)
    
    ## Abort if no variables exist at this point
    ModuleStopIfNoVarsLeft(vars)
    
    ## Get variable labels (named list of label string or NULL)
    ## Need to occur before applying factor().
    varLabels <- labelled::var_label(data[vars])
    
    ## Factor conversions if the factorVars argument exist
    if (!missing(factorVars)) {
      ## Check if variables exist. Drop them if not.
      factorVars <- ModuleReturnVarsExist(factorVars, data)
      ## Convert to factor
      data[factorVars] <- lapply(data[factorVars], factor)
    }
    
    ## Toggle test FALSE if no strata is given
    test <- ModuleReturnFalseIfNoStrata(strata, test)
    smd  <- ModuleReturnFalseIfNoStrata(strata, smd)
    
    ## Get the missing percentage for each variable (no strata).
    percentMissing <- ModulePercentMissing(data[vars])
    
    ## Get the classes of the variables
    varClasses  <- lapply(data[vars], class)
    
    ## Classify as varFactors if any one of these classes are contained
    varFactors <-sapply(varClasses, function(VEC) {
      any(VEC %in% c("factor", "ordered", "logical", "character", "labelled"))
    })
    varFactors <- names(varFactors)[varFactors]
    
    ## Classify as varNumerics if any one of these classes are contained
    varNumerics <-sapply(varClasses, function(VEC) {
      any(VEC %in% c("numeric", "integer"))
    })
    varNumerics <- names(varNumerics)[varNumerics]
    
    ## Drop variables that do not meet either because it is unsupported
    varDrop <- setdiff(vars, c(varFactors, varNumerics))
    if (length(varDrop) > 0) {
      warning("Dropping variable(s) ", paste0(varDrop, sep = " "),
              " due to unsupported class.\n")
      vars <- setdiff(vars, varDrop)
    }
    
    ## Create a logical vector indicator for factors (vars in varFactors = TRUE)
    logiFactors <- vars %in% varFactors
    
    ## Create lists of arguments
    argsCreateContTable <- list(data          = data,
                                test          = test,
                                testNormal    = testNormal,
                                argsNormal    = argsNormal,
                                testNonNormal = testNonNormal,
                                argsNonNormal = argsNonNormal,
                                smd           = smd)
    argsCreateCatTable  <- list(data          = data,
                                includeNA     = includeNA,
                                test          = test,
                                testApprox    = testApprox,
                                argsApprox    = argsApprox,
                                testExact     = testExact,
                                argsExact     = argsExact,
                                smd           = smd)
    
    ## Add strata = strata for argument only if strata is given
    if (!missing(strata)) {
      
      ## Check strata. This returns a DF. Returns a "Overall" DF if strata is missing.
      ## Must not be place outside if (!missing(strata)) {  }.
      dfStrata <- ModuleReturnStrata(strata, data)
      ## Return variable names. Code inefficient in exchange for code simplicity.
      strata   <- names(dfStrata)
      
      ## Create lists of arguments including strata
      argsCreateContTable <- c(list(strata = strata), argsCreateContTable)
      argsCreateCatTable  <- c(list(strata = strata), argsCreateCatTable)
    }
    
    
    ### If only varFactors/varNumerics are present, just call one constructor
    if (length(varNumerics) == 0) {
      ## No numerics
      ContTable <- NULL
      CatTable  <- do.call(CreateCatTable,
                           args = c(list(vars = varFactors), argsCreateCatTable))
      
    } else if (length(varFactors) == 0) {
      ## No factors
      ContTable <- do.call(CreateContTable,
                           args = c(list(vars = varNumerics), argsCreateContTable))
      CatTable  <- NULL
      
      ### Both types of variables are present, call both constructors
    } else if ((length(varFactors) > 0) & (length(varNumerics) > 0)) {
      
      ## ContTable
      ContTable <- do.call(CreateContTable,
                           args = c(list(vars = varNumerics), argsCreateContTable))
      ## CatTable
      CatTable  <- do.call(CreateCatTable,
                           args = c(list(vars = varFactors),  argsCreateCatTable))
    } else {
      
      ## vars never empty by data check with ModuleStopIfNoVarsLeft()
      ## Just to make sure
      warning("No variables left to analyzed in vars.")
    }
    
    
    ## Create a list for output
    ## Either one of the two tables may be NULL
    TableOneObject <- list(ContTable = ContTable,
                           CatTable  = CatTable,
                           MetaData  = list(vars        = vars,
                                            ## describes which pos is vars is factor
                                            logiFactors = logiFactors,
                                            ## names of vars of each type
                                            varFactors  = varFactors,
                                            varNumerics = varNumerics,
                                            ## Missing data percentage for each variable (no strata).
                                            percentMissing = percentMissing,
                                            ## Variable labels
                                            varLabels = varLabels))
    
    ## Give a class
    class(TableOneObject) <- "TableOne"
    
    ## Return the object
    return(TableOneObject)
  }


#####

## Check if the data given is a data frame
ModuleStopIfNotDataFrame <- function(data) {
  
  if (is.data.frame(data) == FALSE) {
    stop("The data argument needs to be a data frame (no quote).")
  }
}

#####

## Extract variables that exist in the data frame
## Also exclude variables that only have NA
ModuleReturnVarsExist <- function(vars, data) {
  
  ## Check if variables exist. Drop them if not.
  varsNotInData <- setdiff(vars, names(data))
  
  if (length(varsNotInData) > 0) {
    warning("The data frame does not have: ",
            paste0(varsNotInData, sep = " "), " Dropped")
    ## Only keep variables that exist
    vars <- intersect(vars, names(data))
  }
  
  ## Check if variables have at least some valid values (not NA/NaN)
  logiAllNaVars <- sapply(X   = data[vars],
                          FUN = function(VAR) {
                            all(is.na(VAR))
                          },
                          simplify = TRUE)
  
  if (any(logiAllNaVars)) {
    warning("These variables only have NA/NaN: ",
            paste0(vars[logiAllNaVars], sep = " "), " Dropped")
    
    vars <- vars[!logiAllNaVars]
  }
  
  ## Return existing and valid variables
  return(vars)
}

## Stop if not vars are left
ModuleStopIfNoVarsLeft <- function(vars) {
  if (length(vars) < 1) {stop("No valid variables.")}
}

## Toggle test FALSE if no strata are given
ModuleReturnFalseIfNoStrata <- function(strata, test) { # Give strata variable names
  
  if(missing(strata)) {
    ## test cannot be performed if no strata
    test <- FALSE
  }
  return(test)
}

## Check statra variables and conditionally create strata data frame
ModuleReturnStrata <- function(strata, data) {     # Give strata variable names
  ## strata: char vector; data: data frame given
  
  if(missing(strata)) {
    ## If there is no strata, give "Overall" to every subject (dim1 is nrow)
    strata <- rep("Overall", nrow(data))
    
  } else { # If strata is given
    
    ## unique it first to remove duplications
    strata <- unique(strata)
    
    ## Drop nonexisting and NA/NaN only variables
    strata <- ModuleReturnVarsExist(strata, data)
    
    ## Conditional on presence of remaining variable
    if (length(strata) == 0) {
      ## Stop if none left
      stop("None of the stratifying variables are present in the data frame.")
      
    } else {
      
      ## Check validity of the remaining variables
      logiSingleLevelOnly <-
        lapply(data[c(strata)],
               function(VEC) {
                 ## Check number of levels
                 nLevels <- ifelse(test = is.factor(VEC),
                                   yes  = nlevels(VEC),
                                   no   = nlevels(factor(VEC)))
                 ## Return logical indicating only one valid level
                 nLevels == 1
               })
      logiSingleLevelOnly <- unlist(logiSingleLevelOnly)
      
      ## Only keep variables that have 2+ levels
      if (any(logiSingleLevelOnly)) {
        warning("These variables have only one valid level: ",
                paste0(strata[logiSingleLevelOnly], sep = " "), " Dropped")
        
        strata <- strata[!logiSingleLevelOnly]
        
      }
      
      ## Stop if no variables are left
      if (length(strata) == 0) {
        ## Stop if none left
        stop("None of the stratifying variables have 2+ valid levels.")
      }
      
      ## Extract the stratifying variable vector (strata is a data frame)
      strata <- data[c(strata)]
    }
  }
  
  ## return DF with strata variable(s)
  return(strata)
}


###
### Modules for data creation
################################################################################

## Module to create a table for one categorical variable
## Taken from Deducer::frequencies()
ModuleCreateTableForOneVar <- function(x) { # Give a vector
  
  ## Create a one dimensional table (NA is intentionally dropped)
  freqRaw          <- table(x)
  
  ## Level names
  freq <- data.frame(level = names(freqRaw))
  
  ## Total n (duplicated as many times as there are levels)
  freq$n <- length(x)
  
  ## Total missing n (duplicated as many times as there are levels)
  freq$miss        <- sum(is.na(x))
  
  ## Total missing percentage
  freq$p.miss      <- (freq$miss / freq$n) * 100
  
  ## Category frequency
  freq$freq        <- freqRaw
  
  ## Category percent
  freq$percent     <- freqRaw / sum(freqRaw) * 100
  
  ## Category percent (cumulative)
  freq$cum.percent <- cumsum(freqRaw) / sum(freqRaw) * 100
  
  ## Reorder variables
  freq <- freq[c("n","miss","p.miss","level","freq","percent","cum.percent")]
  
  ## Return result as a data frame
  return(freq)
}


## Convert variables with NA to include NA as a level (for CatTable constructor)
ModuleIncludeNaAsLevel <- function(data) {
  ## Logical vector for variables that have any NA
  logiAnyNA <- (colSums(is.na(data)) > 0)
  
  ## Add NA as a new level unless already present
  data[logiAnyNA] <-
    lapply(data[logiAnyNA],
           function(var) {
             if (all(!is.na(levels(var)))) {
               var <- factor(var, c(levels(var), NA),
                             exclude = NULL)
             }
             var
           })
  data
}



###
### Modules for stratification
################################################################################

## Create StrataVarName from multiple dimension headers, for example sex:trt
ModuleCreateStrataVarName <- function(obj) {
  ## Combine variable names with : in between
  paste0(names(attr(obj, "dimnames")), collapse = ":")
}

## Create a single variable representation of multivariable stratification for individuals
## result: by object; strata: data frame of stratifying variable(s)
ModuleCreateStrataVarAsFactor <- function(result, strata) {
  
  ## Create all possible combinations of strata levels and collapse as a vector.
  dfStrataLevels <- expand.grid(attr(result, "dimnames")) # 1st var cycles fastest, consistent with by()
  ## Create a single variable representing all strata
  strataLevels   <- apply(X = dfStrataLevels, MARGIN = 1, FUN = paste0, collapse = ":")
  ## The length is the number of potential combinations. Used for the levels argument in the next part.
  
  ## Create the actual variable from the observed levels. NA if any one of the variables is NA.
  strataVar      <- as.character(interaction(strata, sep = ":"))
  ## Make it a factor (kruskal.test requires it). Use levels not to drop defined nonexisting levels.
  strataVar      <- factor(strataVar, levels = strataLevels)
  
  ## Return stratifying variable. The length is the number of observations in the dataset.
  ## NA for subjects with NA for any of the stratifying variables.
  return(strataVar)
}



###
### Modules for safe hypothesis testing and numeric summaries
################################################################################

## ModuleTryCatchWE
## Try catch function           # Taken from demo(error.catching)
## Used to define non-failing functions, that return NA when there is an error
ModuleTryCatchWE <- function(expr) {
  W <- NULL
  w.handler <- function(w) { # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

## Function to perform non-failing tests (obj should be xtabs or formula)
ModuleTestSafe <- function(obj, testFunction, testArgs = NULL) {
  
  ## Result from a function has to have $p.value element
  out <- ModuleTryCatchWE(do.call(testFunction, args = c(list(obj), testArgs))$p.value)
  
  ## If it contains a numeric value, return it. Otherwise, return NA.
  pValue <- ifelse(is.numeric(out$value), out$value, NA)
  
  ## When obj is an xtabs object
  if (any(class(obj) %in% "xtabs")) {
    ## and has 1 x M dimension, always return NA, and end there.
    if (dim(obj)[1] == 1) {
      ## ends here, returning NA
      return(NA)
      
    } else {
      ## If obj is a multi-row xtabs object, return the p-value
      pValue
    }
  } else {
    ## If obj is not an xtabs (formula for continuous variables), return the p-value
    pValue
  }
}


## Define special skewness and kurtosis functions that do not fail (SAS definitions)
ModuleSasSkewness <- function(x) {
  out <- ModuleTryCatchWE(e1071::skewness(x, na.rm = TRUE, type = 2))
  ## If it returns a numeric value, return it. Otherwise, return NaN.
  ifelse(is.numeric(out$value), out$value, NaN)
}
ModuleSasKurtosis <- function(x) {
  out <- ModuleTryCatchWE(e1071::kurtosis(x, na.rm = TRUE, type = 2))
  ## If it returns a numeric value, return it. Otherwise, return NaN.
  ifelse(is.numeric(out$value), out$value, NaN)
}


###
### Module for testing multiple variables
################################################################################

ModuleApproxExactTests <- function(result, strata, dat, strataVarName,
                                   testApprox, argsApprox,
                                   testExact,  argsExact) {
  ## Create a single variable representation of multivariable stratification
  strataVar <- ModuleCreateStrataVarAsFactor(result, strata)
  
  ## Loop over variables in dat, and create a list of xtabs
  ## Empty strata are kept in the corss tables. Different behavior than the cont counterpart!
  listXtabs <- sapply(X = names(dat),
                      FUN = function(var) {
                        ## Create a formula
                        formula <- as.formula(paste0("~ ", var, " + ", "strataVar"))
                        
                        ## Create a 2-dimensional crosstable
                        xtabs(formula = formula, data = dat)
                      },
                      simplify = FALSE)
  
  ## Rename the second dimension of the xtabs with the newly create name.
  for (i in seq_along(listXtabs)) {
    
    names(dimnames(listXtabs[[i]]))[2] <- strataVarName
  }
  
  ## Loop over xtabs, and create p-values
  pValues <-
    sapply(X = listXtabs,
           FUN = function(xtabs) {
             ## Perform tests and return the result as 1x2 DF
             data.frame(pApprox = ModuleTestSafe(xtabs, testApprox, argsApprox),
                        pExact  = ModuleTestSafe(xtabs, testExact,  argsExact))
           },
           simplify = FALSE)
  
  ## Create a single data frame (n x 2 (normal,nonormal))
  pValues <- do.call(rbind, pValues)
  
  ## Return both xtabs and p value df
  list(pValues = pValues, xtabs = listXtabs)
}


###
### Module for marginal missing percentage handling
################################################################################

## Returns a vector of missing percentage given data frame
ModulePercentMissing <- function(data) {
  unlist(lapply(data, function(x) {sum(is.na(x)) / length(x) * 100}))
}

#################
CreateContTable <-
  function(vars,                                   # character vector of variable names
           strata,                                 # character vector of variable names
           data,                                   # data frame
           funcNames    = c(                       # can pick a subset of them
             "n","miss","p.miss",
             "mean","sd",
             "median","p25","p75","min","max",
             "skew","kurt"),
           funcAdditional,                         # named list of additional functions
           test          = TRUE,                   # Whether to include p-values
           testNormal    = oneway.test,            # test for normally distributed variables
           argsNormal    = list(var.equal = TRUE), # arguments passed to testNormal
           testNonNormal = kruskal.test,           # test for nonnormally distributed variables
           argsNonNormal = list(NULL),             # arguments passed to testNonNormal
           smd           = TRUE                    # whether to include standardize mean differences
  ) {
    
    ### Data check
    ## Check if the data given is a dataframe
    ModuleStopIfNotDataFrame(data)
    
    ## Check if variables exist. Drop them if not.
    vars <- ModuleReturnVarsExist(vars, data)
    
    ## Abort if no variables exist at this point
    ModuleStopIfNoVarsLeft(vars)
    
    ## Get the missing percentage for each variable (no strata).
    percentMissing <- ModulePercentMissing(data[vars])
    
    ## Extract necessary variables
    dat <- data[c(vars)]
    
    ## Toggle test FALSE if no strata
    test <- ModuleReturnFalseIfNoStrata(strata, test)
    smd  <- ModuleReturnFalseIfNoStrata(strata, smd)
    
    ## Create strata data frame (data frame with only strata variables)
    strata <- ModuleReturnStrata(strata, data)
    
    
    ## Handle non-numeric elements (intergers give TRUE, and pass)
    if(any(!sapply(dat, is.numeric))){
      ## If there is any non-numeric variables
      dat <- dat[sapply(dat, is.numeric)]
      warning("Non-numeric variables dropped")
    }
    
    ## Check if all the variables are continuous, and stop if not
    if(!all(sapply(dat, is.numeric))) {stop("Can only be run on numeric variables")}
    
    
    ## Create indexes for default functions by partial string matching with the funcNames argument
    funcIndexes <- pmatch(funcNames, c("n","miss","p.miss",
                                       "mean","sd",
                                       "median","p25","p75","min","max",
                                       "skew","kurt"))
    ## Remove NA
    funcIndexes <- funcIndexes[!is.na(funcIndexes)]
    
    ## Create a list of default functions
    functions <- c("n"      = function(x) {length(x)},
                   "miss"   = function(x) {sum(is.na(x))},
                   "p.miss" = function(x) {(sum(is.na(x)) / length(x)) * 100},
                   "mean"   = function(x) {mean(x, na.rm = TRUE)},
                   "sd"     = function(x) {sd(x, na.rm = TRUE)},
                   "median" = function(x) {median(x, na.rm = TRUE)},
                   "p25"    = function(x) {quantile(x, probs = 0.25, na.rm = TRUE)},
                   "p75"    = function(x) {quantile(x, probs = 0.75, na.rm = TRUE)},
                   "min"    = function(x) {min(x, na.rm = TRUE)},
                   "max"    = function(x) {max(x, na.rm = TRUE)},
                   "skew"   = function(x) {ModuleSasSkewness(x)},
                   "kurt"   = function(x) {ModuleSasKurtosis(x)}
    )
    
    ## Keep only functions in use
    functions <- functions[funcIndexes]
    
    ## Check for additional functions
    if(!missing(funcAdditional)) {
      
      ## When additional functions are given
      if(!is.list(funcAdditional) || is.null(names(funcAdditional))) {
        ## Stop if not a named list
        stop("funcAdditional must be a named list of functions")
      }
      
      ## If a named list is given, add to the vector of functions and their names
      functions  <- c(functions, unlist(funcAdditional))
      funcNames  <- c(funcNames, names(funcAdditional))
    }
    
    
    ### Actual descriptive statistics are calculated here.
    ## strata-functions-variable structure alternative 2014-01-22
    ## Devide by strata
    result <- by(data = dat, INDICES = strata, # INDICES can be a multi-column data frame
                 
                 ## Work on each stratum
                 FUN = function(strataDat) { # Work on each stratum through by()
                   
                   ## Loop for functions
                   out <- sapply(X = functions,
                                 FUN = function(fun) {
                                   
                                   ## Loop for variables
                                   sapply(X = strataDat, FUN = fun, simplify = TRUE)
                                   
                                 }, simplify = FALSE)
                   
                   ## The 2nd-level loop does not simplify to avoid oversimplification
                   ## when there is only one variable.
                   do.call(cbind, out)
                 })
    
    ## Add stratification variable information as an attribute
    if (length(result) > 1 ) {
      ## strataVarName from dimension headers
      strataVarName <- ModuleCreateStrataVarName(result)
      ## Add an attribute for the stratifying variable name
      attributes(result) <- c(attributes(result),
                              list(strataVarName = strataVarName))
    }
    
    
    ### Perform tests when necessary
    ## Initialize to avoid error when it does not exist at the attribute assignment
    pValues <- NULL
    
    ## Create a single variable representation of multivariable stratification
    ## Respect ordering of levels in by()
    strataVar <- ModuleCreateStrataVarAsFactor(result, strata)
    
    ## Only when test is asked for
    if (test) {
      
      ## Loop over variables in dat, and obtain p values for two tests
      ## DF = 6 when there are 8 levels (one empty),
      ## i.e., empty strata dropped by oneway.test/kruskal.test
      pValues <-
        sapply(X = dat,
               FUN = function(var) {
                 ## Perform tests and return the result as 1x2 DF
                 data.frame(
                   pNormal    = ModuleTestSafe(var ~ strataVar, testNormal,    argsNormal),
                   pNonNormal = ModuleTestSafe(var ~ strataVar, testNonNormal, argsNonNormal)
                 )
               },
               simplify = FALSE)
      
      ## Create a single data frame (n x 2 (normal,nonormal))
      pValues <- do.call(rbind, pValues)
    } # Conditional for test == TRUE ends here.
    
    
    ### Perform SMD when requested
    smds <- NULL
    
    ## Only when SMD is asked for
    if (smd) {
      ## list of smds
      smds <- sapply(dat, function(var) {
        StdDiff(variable = var, group = strataVar)
      }, simplify = FALSE)
      ## Give name and add mean column
      smds <- FormatLstSmds(smds, nStrata = length(result))
    }
    
    
    ## Return object
    ## Give an S3 class
    class(result) <- c("ContTable", class(result))
    
    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues),
                            list(smd     = smds),
                            list(percentMissing = percentMissing))
    
    ## Return
    return(result)
  }


################

CreateCatTable <-
  function(vars,                                  # character vector of variable names
           strata,                                # character vector of variable names
           data,                                  # data frame
           includeNA  = FALSE,                    # include NA as a category
           test       = TRUE,                     # whether to include p-values
           testApprox = chisq.test,               # function for approximation test
           argsApprox = list(correct = TRUE),     # arguments passed to testApprox
           testExact  = fisher.test,              # function for exact test
           argsExact  = list(workspace = 2*10^5), # arguments passed to testExact
           smd        = TRUE                      # whether to include standardize mean differences
  ) {
    
    ### Data check
    ## Check if the data given is a dataframe
    ModuleStopIfNotDataFrame(data)
    
    ## Check if variables exist. Drop them if not.
    vars <- ModuleReturnVarsExist(vars, data)
    
    ## Abort if no variables exist at this point
    ModuleStopIfNoVarsLeft(vars)
    
    ## Get the missing percentage for each variable (no strata).
    ## This has to happen before includeNA is used.
    percentMissing <- ModulePercentMissing(data[vars])
    
    ## Extract necessary variables (unused variables are not included in dat)
    dat <- data[c(vars)]
    
    ## Toggle test FALSE if no strata
    test <- ModuleReturnFalseIfNoStrata(strata, test)
    smd  <- ModuleReturnFalseIfNoStrata(strata, smd)
    
    ## Create strata data frame (data frame with only strata variables)
    strata <- ModuleReturnStrata(strata, data)
    
    ## Convert to a factor if it is not a factor already. (categorical version only)
    ## Not done on factors, to avoid dropping zero levels.
    ## Probably this cannot handle Surv object??
    logiNotFactor <- sapply(dat, function(VEC) {
      ## Return TRUE if classes for a vector does NOT contain class "factor"
      ## VEC is a vector of a variable in the dat data frame, use class
      !any(class(VEC) %in% c("factor"))
    })
    
    dat[logiNotFactor] <- lapply(dat[logiNotFactor], factor)
    
    ## If including NA as a level, include NA as a factor level before subsetting
    if (includeNA) {
      dat <- ModuleIncludeNaAsLevel(dat)
    }
    
    ### Actual descriptive statistics are calculated here.
    ## strata--variable-CreateTableForOneVar structure
    ## Devide by strata
    result <- by(data = dat, INDICES = strata, # INDICES can be a multi-column data frame
                 
                 ## Work on each stratum
                 FUN = function(dfStrataDat) { # dfStrataDat should be a data frame
                   
                   ## Loop for variables
                   sapply(dfStrataDat,
                          FUN = ModuleCreateTableForOneVar,
                          simplify = FALSE)
                   
                 }, simplify = FALSE)
    
    
    ## Add stratification variable information as an attribute
    if (length(result) > 1 ) {
      ## strataVarName from dimension headers
      strataVarName <- ModuleCreateStrataVarName(result)
      ## Add an attribute for the stratifying variable name
      attributes(result) <- c(attributes(result),
                              list(strataVarName = strataVarName))
    }
    
    
    ### Perform tests when necessary
    ## Initialize
    pValues   <- NULL
    listXtabs <- list()
    
    ## Create a single variable representation of multivariable stratification
    ## Respect ordering of levels in by()
    strataVar <- ModuleCreateStrataVarAsFactor(result, strata)
    
    ## Only when test is asked for
    if (test) {
      lstXtabsPVals <-
        ModuleApproxExactTests(result        = result,
                               strata        = strata,
                               dat           = dat,
                               strataVarName = strataVarName,
                               testApprox    = testApprox,
                               argsApprox    = argsApprox,
                               testExact     = testExact,
                               argsExact     = argsExact)
      pValues   <- lstXtabsPVals$pValues
      listXtabs <- lstXtabsPVals$xtabs
    }
    
    
    ### Perform SMD when requested
    smds <- NULL
    
    ## Only when SMD is asked for
    if (smd) {
      ## list of smds
      smds <- sapply(dat, function(var) {
        StdDiffMulti(variable = var, group = strataVar)
      }, simplify = FALSE)
      ## Give name and add mean column
      smds <- FormatLstSmds(smds, nStrata = length(result))
    }
    
    
    ## Return object
    ## Give an S3 class
    class(result) <- c("CatTable", class(result))
    
    ## Give additional attributes
    attributes(result) <- c(attributes(result),
                            list(pValues = pValues),
                            list(xtabs   = listXtabs),
                            list(smd     = smds),
                            list(percentMissing = percentMissing))
    
    ## Return
    return(result)
  }

