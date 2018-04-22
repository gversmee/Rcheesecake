#' @author Gregoire Versmee, Laura Versmee

query.where <- function(env, pathlist, subset = "ALL", token, verbose = FALSE)  {

  if (verbose)  message('\nBuilding the "where" part of the query')

  where <- '"where": ['

  if (subset == "ALL")  {

    if (verbose)  message('  default subset = "ALL"\n  -> will look for all the patients that have a value for at list one of the variable selected')

    return(paste0('"where": [{',
           paste0('"field": {"pui": "',
                   pathlist,
                   '","dataType": "STRING"},"predicate": "CONTAINS","fields": {"ENCOUNTER": "YES"}',
                   collapse = '},{"logicalOperator": "OR", '),
            '}]}'))

  }  else  {

    if (verbose)  message("Complex subset detected")

    ## Working on where clause, struct = "(path/to/var1) & (path/to/var2 > x) ! (path/to/var3 <= y)"
    #1. substitue AND, OR, NOT by &,|, !
    subset <- gsub("\\) AND \\(", "\\) & \\(", gsub("\\) OR \\(", "\\) \\| \\(", gsub("\\) NOT \\(", "\\) ! \\(", subset)))

    # 2. How many args? -> grep for logical operator between ) ( +1
    and <- unlist(gregexpr ("\\) & \\(", subset))
    and <- and[which(and > 0)]
    or <- unlist(gregexpr ("\\) \\| \\(", subset))
    or <- or[which(or >0)]
    not <- unlist(gregexpr ("\\) ! \\(", subset))
    not <- not[which(not>0)]

    sep <- c(2, and, or, not, nchar(subset))
    nargs <- length(sep)-1

    if (verbose)  message(paste(nargs, "argument(s) detected"))

    #3. Separate each args
    separg <- function(e) {
    # define logicaOperator
      if (e>1)  logicalOperator <- gsub("&", "AND", gsub("\\|", "OR", gsub("!", "NOT", substr(subset, sep[e]+2, sep[e]+2))))
      else logicalOperator <- "OR"

      # define pui
      arg <- substr(subset, sep[e], sep[e+1]-1)
      arg <- substr(arg, regexpr("\\(", arg) + 1, nchar(arg))

      # define dataType
      #4. look for logical operator in the first arg
      logicaleq1 <- regexpr("== ", arg)
      logicaleq <- c(logicaleq1, regexpr("= ", arg))
      logicaleq <- logicaleq[which(logicaleq > 0)]
      logicalne <- regexpr("!= ", arg)
      logicalne <- logicalne[which(logicalne > 0)]
      logicalgt <- regexpr("> ", arg)
      logicalgt <- logicalgt[which(logicalgt > 0)]
      logicalge <- regexpr(">= ", arg)
      logicalge <- logicalge[which(logicalge > 0)]
      logicallt <- regexpr("< ", arg)
      logicallt <- logicallt[which(logicallt > 0)]
      logicalle <- regexpr("<= ", arg)
      logicalle <- logicalle[which(logicalle > 0)]

      logical <- c(logicaleq, logicalne, logicalgt, logicalge, logicallt, logicalle)

      #5. if logical operator, split
      if (length(logical) > 0)  {
        pui <- path.list(env, trimws(substr(arg, 1, logical-2)), token)
        OPERATOR <- gsub("=", "EQ", gsub("==", "EQ", gsub("!=", "NE", gsub(">", "GT", gsub(">=", "GE", gsub("<", "LT", gsub("<=", "LE", trimws(substr(arg, logical, logical+1)))))))))
        CONSTRAINT <- trimws(substr(arg[i], logical+2, nchar(arg)))
        predicate <- "CONSTRAIN_VALUE"
            return(paste0(
                        '{"logicalOperator":"',
                        logicalOperator,
                        '","field": {"pui": "',
                        pui,
                        '","dataType": "INTEGER"},"predicate": "CONSTRAIN_VALUE","fields": {"OPERATOR": "',
                        OPERATOR,
                        '", "CONSTRAINT": "',
                        CONSTRAINT,
                        '","ENCOUNTER": "YES"}},'))
      }  else  {
        pui <- path.list(env, trimws(arg), token)
        return(paste0(
               '{"logicalOperator":"',
               logicalOperator,
               '","field": {"pui": "',
               pui,
               '","dataType": "STRING"},"predicate": "CONTAINS","fields": {"ENCOUNTER": "YES"}},'))
      }
    }

    where <- paste0('"where": [',
                    sub('"logicalOperator":"OR",', "",
                        paste0(sapply(1:nargs, separg), collapse = "")))

    return(paste0(substr(where, 1, (nchar(where)-1)), "]}"))
  }
}
