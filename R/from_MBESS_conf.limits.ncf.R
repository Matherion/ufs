from_MBESS_conf.limits.ncf <- function (F.value = NULL, conf.level = 0.95, df.1 = NULL, df.2 = NULL,
                                        alpha.lower = NULL, alpha.upper = NULL, tol = 1e-09, Jumping.Prop = 0.1)
{
  if (Jumping.Prop <= 0 | Jumping.Prop >= 1)
    stop("The Jumping Proportion ('Jumping.Prop') must be between zero and one.")
  if (is.null(F.value))
    stop("Your 'F.value' is not correctly specified.")
  if (F.value < 0)
    stop("Your 'F.value' is not correctly specified.")
  if (is.null(df.1) | is.null(df.2))
    stop("You must specify the degrees of freedom ('df.1' and 'df.2').")
  if (is.null(alpha.lower) & is.null(alpha.upper) & is.null(conf.level))
    stop("You need to specify the confidence interval parameters.")
  if ((!is.null(alpha.lower) | !is.null(alpha.upper)) & !is.null(conf.level))
    stop("You must specify only one method of defining the confidence limits.")
  if (!is.null(conf.level)) {
    if (conf.level >= 1 | conf.level <= 0)
      stop("Your confidence level ('conf.level') must be between 0 and 1.")
    alpha.lower <- alpha.upper <- (1 - conf.level)/2
  }
  if (alpha.lower == 0)
    alpha.lower <- NULL
  if (alpha.upper == 0)
    alpha.upper <- NULL
  FAILED <- NULL
  if (!is.null(alpha.lower)) {
    LL.0 <- stats::qf(p = alpha.lower * 5e-04, df1 = df.1, df2 = df.2)
    Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.0) -
      (1 - alpha.lower)
    if (stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.0) <
        (1 - alpha.lower)) {
      FAILED <- if (stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                       ncp = 0) < 1 - alpha.lower)
        LL.0 <- 1e-08
      if (stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.0) <
          1 - alpha.lower)
        FAILED <- TRUE
    }
    if (is.null(FAILED)) {
      LL.1 <- LL.2 <- LL.0
      while (Diff > tol) {
        LL.2 <- LL.1 * (1 + Jumping.Prop)
        Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                          ncp = LL.2) - (1 - alpha.lower)
        LL.1 <- LL.2
      }
      LL.1 <- LL.2/(1 + Jumping.Prop)
      LL.Bounds <- c(LL.1, (LL.1 + LL.2)/2, LL.2)
      Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.Bounds[2]) -
        (1 - alpha.lower)
      while (abs(Diff) > tol) {
        Diff.1 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                     ncp = LL.Bounds[1]) - (1 - alpha.lower) > tol
        Diff.2 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                     ncp = LL.Bounds[2]) - (1 - alpha.lower) > tol
        Diff.3 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                     ncp = LL.Bounds[3]) - (1 - alpha.lower) > tol
        if (Diff.1 == TRUE & Diff.2 == TRUE & Diff.3 ==
            FALSE) {
          LL.Bounds <- c(LL.Bounds[2], (LL.Bounds[2] +
                                          LL.Bounds[3])/2, LL.Bounds[3])
        }
        if (Diff.1 == TRUE & Diff.2 == FALSE & Diff.3 ==
            FALSE) {
          LL.Bounds <- c(LL.Bounds[1], (LL.Bounds[1] +
                                          LL.Bounds[2])/2, LL.Bounds[2])
        }
        Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                          ncp = LL.Bounds[2]) - (1 - alpha.lower)
      }
      LL <- LL.Bounds[2]
    }
  }
  if (!is.null(FAILED))
    LL <- NA
  if (!is.null(alpha.upper)) {
    FAILED.Up <- NULL
    UL.0 <- stats::qf(p = 1 - alpha.upper * 5e-04, df1 = df.1, df2 = df.2)
    Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.0) -
      alpha.upper
    if (Diff < 0)
      UL.0 <- 1e-08
    Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.0) -
      alpha.upper
    if (Diff < 0) {
      FAILED.Up <- TRUE
    }
    if (is.null(FAILED.Up)) {
      UL.1 <- UL.2 <- UL.0
      while (Diff > tol) {
        UL.2 <- UL.1 * (1 + Jumping.Prop)
        Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                   ncp = UL.2) - alpha.upper
        UL.1 <- UL.2
      }
      UL.1 <- UL.2/(1 + Jumping.Prop)
      UL.Bounds <- c(UL.1, (UL.1 + UL.2)/2, UL.2)
      Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.Bounds[2]) -
        alpha.upper
      while (abs(Diff) > tol) {
        Diff.1 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                     ncp = UL.Bounds[1]) - alpha.upper > tol
        Diff.2 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                     ncp = UL.Bounds[2]) - alpha.upper > tol
        Diff.3 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                     ncp = UL.Bounds[3]) - alpha.upper > tol
        if (Diff.1 == TRUE & Diff.2 == TRUE & Diff.3 ==
            FALSE) {
          UL.Bounds <- c(UL.Bounds[2], (UL.Bounds[2] +
                                          UL.Bounds[3])/2, UL.Bounds[3])
        }
        if (Diff.1 == TRUE & Diff.2 == FALSE & Diff.3 ==
            FALSE) {
          UL.Bounds <- c(UL.Bounds[1], (UL.Bounds[1] +
                                          UL.Bounds[2])/2, UL.Bounds[2])
        }
        Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2,
                          ncp = UL.Bounds[2]) - alpha.upper
      }
      UL <- UL.Bounds[2]
    }
    if (!is.null(FAILED.Up))
      UL <- NA
  }
  if (!is.null(alpha.lower) & !is.null(alpha.upper))
    return(list(Lower.Limit = LL, Prob.Less.Lower = 1 - stats::pf(q = F.value,
                                                           df1 = df.1, df2 = df.2, ncp = LL), Upper.Limit = UL,
                Prob.Greater.Upper = stats::pf(q = F.value, df1 = df.1,
                                        df2 = df.2, ncp = UL)))
  if (is.null(alpha.lower) & !is.null(alpha.upper))
    return(list(Upper.Limit = UL, Prob.Greater.Upper = stats::pf(q = F.value,
                                                          df1 = df.1, df2 = df.2, ncp = UL)))
  if (!is.null(alpha.lower) & is.null(alpha.upper))
    return(list(Lower.Limit = LL, Prob.Less.Lower = 1 - stats::pf(q = F.value,
                                                           df1 = df.1, df2 = df.2, ncp = LL)))
}
