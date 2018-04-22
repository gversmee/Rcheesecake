#' @author Gregoire Versmee, Laura Versmee, Mikael Dusenne

nicer.result <- function(result, verbose = FALSE)  {

  if (verbose)  message("  combining the categorical variables")

  groups <- dirname(colnames(result))
  groups2 <- unique(groups)

  final <- result[1]
  cnames <- c("Patient_id")

  for (i in 2:length(groups2))  {
    subdf <- result[which(groups == groups2[i])]

    if (length(unique(subdf[,1])) <= 2  & any(is.na(unique(subdf[,1]))))  {
      subdf[is.na(subdf)] <- ""
      final <- cbind(final, as.factor(apply(subdf, 1, paste0, collapse = "")))
      cnames <- c(cnames, basename(dirname(colnames(subdf)[1])))
    } else {
      final <- cbind(final, subdf)
      cnames <- c(cnames, basename(colnames(subdf)))
    }
  }

  colnames(final) <- cnames

  return(final)
}
