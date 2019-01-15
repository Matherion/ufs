#' crossTab, confIntV and cramersV
#'
#' These functions compute the point estimate and confidence interval for
#' Cramer's V. The crossTab function also shows a crosstable.
#'
#'
#' @aliases confIntV cramersV crossTab
#' @param x Either a crosstable to analyse, or one of two vectors to use to
#' generate that crosstable. The vector should be a factor, i.e. a categorical
#' variable identified as such by the 'factor' class).
#' @param y If x is a crosstable, y can (and should) be empty. If x is a
#' vector, y must also be a vector.
#' @param digits Minimum number of digits after the decimal point to show in
#' the result.
#' @param pValueDigits Minimum number of digits after the decimal point to show
#' in the Chi Square p value in the result.
#' @param conf.level Level of confidence for the confidence interval.
#' @param samples Number of samples to generate when bootstrapping.
#' @param method Whether to use Fisher's Z or bootstrapping to compute the
#' confidence interval.
#' @param storeBootstrappingData Whether to store (or discard) the data
#' generating during the bootstrapping procedure.
#' @param ...  Extra arguments to \code{crossTab} are passed on to
#' \code{confIntV}.
#' @return
#'
#' The cramersV and confIntV functions return either a point estimate or a
#' confidence interval for Cramer's V, an effect size to describe the
#' association between two categorical variables. The crossTab function is just
#' a wrapper around confIntV.
#' @keywords bivar
#' @examples
#'
#'
#' crossTab(infert$education, infert$induced, samples=50);
#'
#' ### Get confidence interval for Cramer's V
#' ### Note that by using 'table', and so removing the raw data, inhibits
#' ### bootstrapping, which could otherwise take a while.
#' confIntV(table(infert$education, infert$induced));
#'
#'
#' @rdname crossTab
#' @export
crossTab <- function(x, y=NULL, conf.level=.95,
                     digits=2, pValueDigits=3, ...) {

  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());

  if (is.null(y)) {
    if (!is.table(x) && !is.matrix(x)) {
      stop("If argument 'y' is empty, argument 'x' must be a matrix or a ",
           "table! Instead, it has class ", class(x), ".");
    } else {
      res$intermediate$n <- sum(x);
      res$intermediate$table <- x;
      res$intermediate$confIntV <- confIntV(res$intermediate$table,
                                            conf.level=conf.level, ...);
    }
  } else {
    if (length(x) != length(y)) {
      stop("The length of arguments 'x' and 'y' is not the same; are you ",
           "sure they're both vectors of equal length?");
    }

    res$intermediate$table <- table(x, y);
    res$intermediate$n <- sum(res$intermediate$table);
    res$intermediate$varNames <- c(deparse(substitute(x)), deparse(substitute(y)));
    res$intermediate$validForBoth <- stats::complete.cases(cbind(x, y));

    if (length(unique(x[res$intermediate$validForBoth])) < 2) {
      stop("The variable specified as 'x' ('", res$intermediate$varNames[1],
           "') has less than two unique ",
           "values!");
    }
    if (length(unique(y[res$intermediate$validForBoth])) < 2) {
      stop("The variable specified as 'y' ('", res$intermediate$varNames[2],
           "') has less than two unique ",
           "values!");
    }
    res$intermediate$confIntV <- confIntV(x, y, conf.level=conf.level, ...);
  }

  names(attributes(res$intermediate$table)$dimnames) <- c(NULL, NULL);

  res$output <- res$intermediate$confIntV$output;
  res$output$chisq <- list(statistic =
                             res$intermediate$confIntV$intermediate$cramersV$intermediate$chisq.test$statistic,
                           parameter =
                             res$intermediate$confIntV$intermediate$cramersV$intermediate$chisq.test$parameter,
                           p.value =
                             res$intermediate$confIntV$intermediate$cramersV$intermediate$chisq.test$p.value);

  class(res) <- 'crossTab';
  return(res);

}

#' @method print crossTab
#' @rdname crossTab
#' @export
print.crossTab <- function(x, digits=x$input$digits,
                           pValueDigits=x$input$pValueDigits, ...) {
  print(x$intermediate$table);
  cat("\n");
  print(x$intermediate$confIntV, digits=digits);
  cat("\nChi-square[", x$output$chisq$parameter, "] = ",
      round(x$output$chisq$statistic, digits), ", ",
      formatPvalue(x$output$chisq$p.value, pValueDigits), sep="");
}

#' @method pander crossTab
#' @rdname crossTab
#' @importFrom pander pander
#' @export
pander.crossTab <- function(x, digits=x$input$digits,
                            pValueDigits=x$input$pValueDigits, ...) {
  cat("\n");
  pander(x$intermediate$table);
  cat("\n");
  print(x$intermediate$confIntV, digits=digits);
  cat("  \nChi-square[", x$output$chisq$parameter, "] = ",
      round(x$output$chisq$statistic, digits), ", ",
      formatPvalue(x$output$chisq$p.value, pValueDigits), sep="");
}

