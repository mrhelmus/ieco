#' @name vif_lmer
#' @title VIF for lmer fit
#' @description calculates VIF for the fixed effects in lmer fits
#' @param fit lmer fit from \code{\link{lmer}}
#' @return a vector of VIF values
#' @author Jon Lefcheck see website
# @examples None None num<
#' @seealso \url{https://jonlefcheck.net/2012/12/28/dealing-with-multicollinearity-using-variance-inflation-factors/}
# @references None None
#' @rdname vif_lmer
#' @export
#' @importFrom lme4 fixef
#' @importFrom stats vcov

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}