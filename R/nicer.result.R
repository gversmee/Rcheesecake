#' @author Gregoire Versmee, Laura Versmee, Mikael Dusenne

nicer.result <- function(result, allpaths, verbose = FALSE)  {

  if (verbose)  message("  combining the categorical variables")

  groups <- dirname(colnames(result)[-1])
  groups2 <- unique(groups)
  names(groups2) <- names(allpaths)[!duplicated(groups)]

  final <- result[1]
  cnames <- c("Patient_id")

  for (i in 1:length(groups2))  {
    subdf <- result[-1][groups == groups2[i]]
    if (length(unique(subdf[,1])) <= 2  & any(is.na(unique(subdf[,1]))))  {
      subdf[is.na(subdf)] <- ""
      final <- cbind(final, as.factor(apply(subdf, 1, paste0, collapse = "")))
      if (names(groups2[i]) == ""  & !is.null(names(groups2)))  cnames <- c(cnames, basename(dirname(colnames(subdf)[1])))  else cnames <- c(cnames, names(groups2[i]))
    } else {
      final <- cbind(final, subdf)
      if (names(groups2[i]) == ""  & !is.null(names(groups2)))  cnames <- c(cnames, basename(colnames(subdf)))  else  cnames <- c(cnames, names(groups2[i]))
    }
  }

  colnames(final) <- cnames

  return(final)
}
