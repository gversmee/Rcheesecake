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
#' @export getpath
#'
#' @import httr

getpath <- function(env, key, var, verbose = FALSE) {
  
  # Is it a key or a token?
  if (nchar(key) < 27)  {
    if (verbose)  message(paste("Key detected, starting a session on", env))
    status <- new.session(env, key, verbose)
    token <- NULL
  }  else  token <- key
  
  # Check the token and say hello!
  about <- data.frame(content.get(paste0(env, "/rest/v1/systemService/about"), token), stringsAsFactors = FALSE)
  username <- about$userid
  if (!is.null(about$message))  stop("Token is invalid, please request a new one.", call. = FALSE)
  
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
  
  allpaths <- unlist(pathlist)
  
  return(allpaths)
}
