#' Two-dimensional visualisation of factor analyses
#'
#' This function uses the [diamondPlot()] to visualise the results of
#' a factor analyses. Because the factor loadings computed in factor analysis
#' are point estimates, they may vary from sample to sample. The factor
#' loadings for any given sample are usually not relevant; samples are but
#' means to study populations, and so, researchers are usually interested in
#' population values for the factor loadings. However, tables with lots of
#' loadings can quickly become confusing and intimidating. This function aims
#' to facilitate working with and interpreting factor analysis based on
#' confidence intervals by visualising the factor loadings and their confidence
#' intervals.
#'
#'
#' @param fa The object produced by the [psych::fa()] function from the
#' [psych::psych] package. It is important that the `n.iter` argument
#' of [psych::fa()] was set to a realistic number, because otherwise, no
#' confidence intervals will be available.
#' @param xlab The label for the x axis.
#' @param colors The colors used for the factors. The default uses the discrete
#' [viridis()] palette, which is optimized for perceptual uniformity,
#' maintaining its properties when printed in grayscale, and designed for
#' colourblind readers. A vector can also be supplied; the colors must be valid
#' arguments to [colorRamp()] (and therefore, to
#' [col2rgb()]).
#' @param labels The labels to use for the items (on the Y axis).
#' @param theme The ggplot2 theme to use.
#' @param \dots Additional arguments will be passed to
#' [ggDiamondLayer()]. This can be used to set, for example, the
#' transparency (alpha value) of the diamonds to a lower value using e.g.
#' `alpha=.5`.
#' @return A [ggplot2::ggplot()] plot with several
#' [ggDiamondLayer()]s is returned.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso [psych::fa()]ss, [meansDiamondPlot()],
#' [meanSDtoDiamondPlot()], [diamondPlot()],
#' [ggDiamondLayer()]
#' @keywords hplot
#' @examples
#'
#' \dontrun{
#' ### (Not run during testing because it takes too long and
#' ###  may generate warnings because of the bootstrapping of
#' ###  the confidence intervals)
#'
#' factorLoadingDiamondCIplot(psych::fa(Bechtoldt,
#'                                      nfactors=2,
#'                                      n.iter=50,
#'                                      n.obs=200));
#'
#' ### And using a lower alpha value for the diamonds to
#' ### make them more transparent
#'
#' factorLoadingDiamondCIplot(psych::fa(Bechtoldt,
#'                                      nfactors=2,
#'                                      n.iter=50,
#'                                      n.obs=200),
#'                            alpha=.5,
#'                            size=1);
#' }
#'
#' @export factorLoadingDiamondCIplot
factorLoadingDiamondCIplot <- function(fa,
                                       xlab='Factor Loading',
                                       colors = viridis::viridis_pal()(max(2, fa$factors)),
                                       labels=NULL,
                                       theme=ggplot2::theme_bw(),
                                       ...) {

  ### Create list for CIs per factor
  CIs <- faConfInt(fa);

  dotsList <- as.list(substitute(list(...)));

  if ('alpha' %in% names(dotsList)) {
    alpha <- dotsList$alpha;
  } else {
    alpha <- 1;
  }

  ### Create empty
  res <- ggplot2::ggplot(data.frame(Factor=as.factor(1:length(CIs))),
                         ggplot2::aes_string(x=-Inf, ymin=-Inf, ymax=-Inf,
                                             color='Factor', fill='Factor')) +
    ggplot2::geom_ribbon() +
    ggplot2::geom_vline(xintercept=0) +
    ggplot2::scale_color_manual(values=colors) +
    ggplot2::scale_fill_manual(values=ggplot2::alpha(colors, alpha));

  for (currentFactor in 1:length(CIs)) {
    res <- res + ggDiamondLayer(CIs[[currentFactor]],
                                color = colors[currentFactor],
                                ...);
  }

  if (is.null(labels)) {
    labels <- rownames(unclass(fa$loadings));
  }

  res <- res +
    ggplot2::scale_y_continuous(breaks=1:nrow(unclass(fa$loadings)),
                                labels=labels) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(xlab) +
    theme;

  return(res);
}
