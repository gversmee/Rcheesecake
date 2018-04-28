#' @title Query your study via the PIC-SURE API
#' @description For this beta version, it is only possible to query phenotypics data. Soon, the package will be upgraded to be able to query genotypics data from Hail.
#' @param env  The URL of the environment
#' @param key The key or the token to log in your environment
#' @param var  A vector with the variables of interest. You can put a variable, or a path, as you want. You can also use the {*} key if you want to use a wild card. If an argument corresponds to a node, it will return all the variables below the node
#' @param subset  By default, subset = ALL and gives you back all the patients that have at least one variable of interest. See the examples for more complex subsets. !! Similarly, the "subsetting" part needs to be slightly modified to handle the "find" function from picsure.
#' @param gabe if TRUE, will export the JSON query as a query.txt file in you working directory. By default, gabe = FALSE
#' @param verbose By default, {verbose = FALSE}. Set it to {verbose = TRUE} to get the log informations
#' @return Returns a data.frame
#' @author Gregoire Versmee, Laura Versmee, Mikael Dusenne, Alba Gutierrez
#' @export picsure
#' @examples
#' Without any subset, will return all the patients that have at least one value for a variable of interest
#' environment <- "https://nhanes.hms.harvard.edu"
#' key <- "yourkeyortoken"
#' pcb <- "PCB153 (ng per g)"
#' age <- "AGE*"
#' sex <- "demographics/SEX"
#' variables <- c(pcb, sex, age)
#' picsure(environment, key, variables)
#'
#' Adding a variable as subset will return only the patient that have a value for this specific variable (can be combined with AND, OR, NOT)
#' subset <- "(laboratory/pcbs/PCB153 (ng per g))"
#' picsure(environment, key, variables, subset)
#'
#' The continuous variable can be filtered by <, >, ==, !=, <=, >=.
#' subset <- "(/demographics/AGE > 60) | (/demographics/AGE < 20)"
#' picsure(environment, key, variables, subset)
#'
#' @import httr

picsure <- function(env, key, var, subset = "ALL", gabe = FALSE, verbose = FALSE) {

  # Is it a key or a token?
  if (nchar(key) < 27)  {
    if (verbose)  message(paste("Key detected, starting a session on", env))
    status <- new.session(env, key, verbose)
    token <- NULL
  }  else  token <- key

  # Check the token and say hello!
  about <- data.frame(content.get(paste0(env, "/rest/v1/systemService/about"), token), stringsAsFactors = FALSE)
  username <- about$userid
  if (!is.null(about$message))  stop("Token is invalid, please request a new one.")

  #message ("             ___        _ _ _            _       _           _
  #          / _ \\      (_) | |          | |     | |         | |
  #         / /_\\ \\_   ___| | | __ _  ___| |__   | |     __ _| |__
  #         |  _  \\ \\ / / | | |/ _` |/ __| '_ \\  | |    / _` | '_ \\
  #         | | | |\\ V /| | | | (_| | (__| | | | | |___| (_| | |_) |
  #         \\_| |_/ \\_/ |_|_|_|\\__,_|\\___|_| |_| \\_____/\\__,_|_.__/")
  #\n

   if (!is.null(username)) {
     username <- unlist(strsplit(username, "@"))[1]
     message(paste("Hi", username, "thank you for using picsuRe!"))
   } else {
     message(paste("Hi thank you for using picsuRe!"))
   }

  # build the query
    # build the "select" part of the query
      # Get the list of "full path"
      pathlist <- path.list(env, var, token, verbose) #returns a list of path

      # Apply the name of each variable to each path
      naming <- function(x, y) {
        names(x) <- rep(y, length(x))
        return(x)
      }

      if (is.null(names(var)))  allpaths <- unlist(pathlist)
      else  allpaths <- unlist(mapply(naming, pathlist, names(var)))

      # build the "where" part of the query
      where <- query.where(env, allpaths, subset, token, verbose)

      # build the "select" part of the query
      select <- query.select(allpaths, verbose)

    # combine select and where
      if (verbose)  message('\nCombining the "select" and "where" part of the query to build the json body')
      body <- paste0(select, where)

      if (gabe) {
        message(paste("Exporting the json query to", getwd()))
        write.table(body, "query.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
      }

  # run the query
      # get the result ID
  resultID <- result.ID(env, body, token, verbose)

      # wait for the result to be available
  available.result(env, resultID, token, verbose)

  # make the table pretty!!
      # get the response
      result <- get.result(env, resultID, allpaths, token, verbose)

      # check if categorical, and combine them
      result <- nicer.result(result, allpaths, verbose)

      # make valid column names
      result <- name.cols(result, verbose)

      message(paste("\nThe data.frame downloaded contains", nrow(result), "observations of", ncol(result), "variables. Its size is", format(object.size(result), units = "Kb")))

      return(result)
}
