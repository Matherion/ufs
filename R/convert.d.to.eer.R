
#' Helper functions for Numbers Needed for Change
#'
#' These two functions are used by \code{\link{nnc}} to compute the Numbers Needed for Change.
#'
#' @param d The value of Cohen's d.
#' @param cer The Control Event Rate.
#' @param r The correlation between the determinant and behavior (for mediated Numbers Needed for Change).
#' @param eventDesirable Whether an event is desirable or undesirable.
#' @param eventIfHigher Whether scores above or below the threshold are considered 'an event'.
#'
#' @return The converted value.
#' @export
#'
#' @seealso \code{\link{nnc}}
#'
#' @author Gjalt-Jorn Peters & Stefan Gruijters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>
#'
#' @references Gruijters, S. L. K., & Peters, G.-J. Y. (2017). Introducing
#' the Numbers Needed for Change (NNC): A practical measure of effect size for intervention research.
#'
#' @examples
#'
#' convert.d.to.eer(d=.5, cer=.25);
#' convert.d.to.nnc(d=.5, cer=.25);

convert.d.to.eer <- function(d, cer, eventDesirable=TRUE, eventIfHigher=TRUE) {
  if (eventIfHigher) {
    return(pnorm((qnorm(cer) + d)));
  } else {
    return(1 - pnorm((qnorm(1-cer) + d)));
  }
}
