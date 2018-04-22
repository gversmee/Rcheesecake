#' @author Gregoire Versmee

get.result <- function(env, resultID, allpaths, token, verbose = FALSE) {


  if (verbose)  message("\nDownloading the data frame")

  df <- suppressMessages(content.get(paste0(env, "/rest/v1/resultService/result/", resultID, "/CSV"), token))

  if (!any(class(df) == "data.frame"))  {
    stop("Internal server error, please check with the developpers\nProcess stopped", call. = FALSE)
  } else {
    data.frame(df, check.names = FALSE)
  }

  result <- data.frame(matrix(ncol = length(allpaths)+1, nrow = nrow(df)))
  result[,c(1:ncol(df))] <- df

  colnames(result) <- c(colnames(df), allpaths[!(allpaths %in% colnames(df))])

  return(result)

}

