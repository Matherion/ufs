#' @export
safeRequire <- function(packageName, mirrorIndex=1) {
  if (!is.element(packageName, installed.packages()[,1])) {
    if (!is.null(mirrorIndex)) {
      chooseCRANmirror(ind=mirrorIndex);
    }
    install.packages(packageName, dependencies=TRUE);
  }
  suppressPackageStartupMessages(require(package = packageName,
                                         character.only=TRUE,
                                         quietly=TRUE));
}
