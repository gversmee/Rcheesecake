#' @author Gregoire Versmee, Laura Versmee

available.result <- function(env, resultID, token, verbose = FALSE) {

  count <- 0L

  if (verbose)  message("\nWaiting for PIC-SURE to return the query")

  status <- content.get(paste0(env, "/rest/v1/resultService/resultStatus/", resultID), token)$status

  while (status != 'AVAILABLE')  {

    if (status == 'ERROR')  {
      stop("Query Failed", call. = FALSE)

    }  else  {
      Sys.sleep(0.2)
      count <- count + 1L
      if (count%%10 == 0)  message("  ...still waiting")
      status <- content.get(paste0(env, "/rest/v1/resultService/resultStatus/", resultID), token)$status
    }
  }
  if (verbose)  message("  Result available \\o/")
}
