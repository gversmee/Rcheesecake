#' @author Gregoire Versmee, Mikael Dusenne, Laura Versmee


flatten.tree <- function(env, nodelist, token, verbose = FALSE)  {



  fetchNode <- function(e)  content.get(paste0(env, "/rest/v1/resourceService/path", gsub("\\?", "%3F", e)), token)

  f <- function(l)  {
    unlist(sapply(l,
                  function(e)  {
                                  if (content.get(paste0(env, "/rest/v1/resourceService/path", gsub("\\?", "%3F", e)), token, status = TRUE)) == 404 {
                                    message(paste0('!!!There is an issue in the database with the path: "', e, '"\n-> discarding path. Please contact the developpers regarding this issue!!!\n'))
                                    return(NULL)
                                  }
                                  node <- fetchNode(e)
                                  if(length(node) == 0)  {
                                    if (verbose)  message(e)
                                    return(e)
                                  } else {
                                    if (!is.null(node$status)) {
                                      message(paste0('!!!There is an issue in the database with the path: "', e, '"\n-> discarding path. Please contact the developpers regarding this issue!!!\n'))
                                      return(NULL)
                                    } else {
                                      return(f(sapply(node, function(n) n$pui)))
                                    }
                                  }
                                },
                  USE.NAMES = FALSE))
  }
  return(as.vector(f((nodelist))))
}


