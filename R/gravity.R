#' @title Gravity model
#' @description Implements Murphy et al., (2010) gravity model
#' 
#' @param y              Name of dependent variable
#' @param x              Character vector of independent variables
#' @param d              Name of column containing distance
#' @param group          Name of grouping column (from or to)
#' @param data           data.frame object containing model data
#' @param ln             Natural log transform data (TRUE/FALSE) 
#' @param constrained    Specify constrained model, if FALSE a linear model (lm) 
#'                       is run (TRUE/FALSE)
#' @param ...            Additional argument passed to nlme or lm      
#' 
#' @return formula           Model formula  
#' @return gravity           Gravity model
#' @return AIC               AIC value for selected model
#' @return log.likelihood    Restricted log-likelihood at convergence
#' @return x                 data.frame of independent variables
#' @return y                 Vector of dependent variable
#' @return groups            Ordered factor vector of grouping variable
#' @return fit               Model Fitted Values
#' @return constrained       TRUE/FALSE indicating if model is constrained
#'
#' @details 
#' The "group" factor defines the singly constrained direction (from or to) and the 
#' grouping structure for the origins. To specify a null (distance only or IBD) 
#' model just omit the x argument. 
#'
#' By default constrained models are fit by maximizing the restricted log-likelihood 
#' (REML), for maximum likelihood use the type="ML" argument which is passed to the 
#' lme function. If ln=TRUE the input data will be log transformed  
#'
#' @note Depends: nlme, lattice
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and 
#'         Melanie Murphy <melanie.murphy@@uwyo.edu>
#'
#' @references
#' Murphy, M. A. & J.S. Evans. (in prep). GenNetIt: graph theoretical gravity modeling 
#'   for landscape genetics
#' @references
#' Murphy M.A., R. Dezzani, D.S. Pilliod & A.S. Storfer (2010) Landscape genetics of 
#'   high mountain frog metapopulations. Molecular Ecology 19(17):3634-3649 
#'
#' @examples 
#' library(nlme)
#' data(ralu.model)
#' str(ralu.model)
#' 
#' # Gravity model	
#' x = c("DEPTH_F", "HLI_F", "CTI_F", "cti", "ffp")
#' ( gm <- gravity(y = "DPS", x = x, d = "DISTANCE", group = "FROM_SITE", 
#'                 data = ralu.model, ln = FALSE) )
#'
#' # Plot gravity results
#'  par(mfrow=c(2,3))
#'    for (i in 1:6) { plot(gm, type=i) } 
#'
#' # log likelihood of competing models 
#'  x = c("DEPTH_F", "HLI_F", "CTI_F", "cti", "ffp")
#'  for(i in x[-1]) {
#'    x1 = c(x[1], x[-which(x %in% i)])  
#'    ll <- gravity(y = "DPS", x = x1, d = "DISTANCE", group = "FROM_SITE", 
#'                  data = ralu.model, ln = FALSE)$log.likelihood
#'	  cat("log likelihood for parameter set:", "(",x1,")", "=", ll, "\n")				 
#'  }
#'
#' # Distance only (IBD) model
#' gravity(y = "DPS", d = "DISTANCE", group = "FROM_SITE", 
#'         data = ralu.model, ln = FALSE)
#'
#' @seealso \code{\link[nlme]{groupedData}} for how grouping works in constrained model 
#' @seealso \code{\link[nlme]{lme}} for constrained model ... options
#' @seealso \code{\link[stats]{lm}} for linear model ... options
#'
#' @import nlme
#' @exportClass gravity
#' @export
gravity <- function (y, x, d, group, data, ln = TRUE, constrained = TRUE, ...) 
{
    if (missing(d)) 
        stop("Distance must be included")
    if (missing(x)) {
        x = d
    }
    x <- unique(c(x, d))
    gdata <- data[, c(group, y, x)]
    gdata <- nlme::groupedData(stats::as.formula(paste(paste(y, 
                               1, sep = " ~ "), group, sep = " | ")), 
							   data = gdata)
    if (ln == TRUE) {
        gdata[, x] <- log(abs(gdata[, x]))
        gdata[, y] <- log(abs(gdata[, y]))
        gdata[gdata == -Inf] <- 0
        gdata[gdata == Inf] <- 0
    }
    fmla <- stats::as.formula(paste(paste(y, "~", sep = ""), 
                              paste(x, collapse = "+")))
    if (constrained == FALSE) {
        print("Running unconstrained gravity model, defaulting to OLS. Please check assumptions")
        gvlmm <- stats::lm(fmla, data = gdata, ...)
        gvaic <- stats::AIC(gvlmm)
        gm <- list(formula = fmla, gravity = gvlmm, AIC = gvaic, 
                   x = gdata[, x], y = gdata[, y], fit = stats::fitted(gvlmm),
				   constrained = constrained)
    }
    else {
        if (!"groupedData" %in% class(gdata)) 
            stop("Data must be a groupedData object for singly-constrained gravity model")
        print("Running singly-constrained gravity model")
		gvlmm <- nlme::lme(fmla, stats::formula(gdata), data = gdata, ...)
        #gvlmm <- nlme::lme(fmla, stats::as.formula(paste("random = ~1", 
        #                   group, sep = " | ")), data = gdata, ...)
        gvaic <- stats::AIC(gvlmm)
        gm <- list(formula = fmla, gravity = gvlmm, AIC = gvaic, 
            log.likelihood = gvlmm$logLik, x = gdata[, x], y = gdata[, 
                y], groups = gdata[, group], fit = stats::fitted(gvlmm),
				constrained = constrained)
    }
    class(gm) <- "gravity"
    return(gm)
}
