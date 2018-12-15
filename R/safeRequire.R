#' @export
safeRequire <- function(packageName, mirrorIndex=NULL) {
  if (!is.element(packageName, installed.packages()[,1])) {
    if (is.null(mirrorIndex)) {
      chooseCRANmirror(ind=1);
    } else {
      chooseCRANmirror(ind=mirrorIndex);
    }
    install.packages(packageName, dependencies=TRUE);
  }
  suppressPackageStartupMessages(require(package = packageName,
                                         character.only=TRUE,
                                         quietly=TRUE));
}
