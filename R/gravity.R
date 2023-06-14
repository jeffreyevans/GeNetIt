#' @title Gravity model
#' @description Implements Murphy et al., (2010) gravity model via a 
#'              linear mixed effects model
#' 
#' @param y              Name of dependent variable
#' @param x              Character vector of independent variables
#' @param d              Name of column containing distance
#' @param group          Name of grouping column (from or to)
#' @param data           data.frame object containing model data
#' @param fit.method     Method used to fit model c("REML", "ML")
#' @param ln             Natural log transform data (TRUE/FALSE) 
#' @param constrained    Specify constrained model, if FALSE a linear model (lm) 
#'                       is run (TRUE/FALSE)
#' @param ...            Additional argument passed to nlme or lm      
#' 
#' @return formula           Model formula call
#' @return fixed.formula     Model formula for fixed effects 
#' @return random.formula    Model formula for random (group) effects 
#'                           (only for constrained models)
#' @return gravity           Gravity model
#' @return fit               Model Fitted Values
#' @return AIC               AIC value for selected model
#' @return RMSE              Root Mean Squared Error (based on bias corrected back transform)
#' @return log.likelihood    Restricted log-likelihood at convergence
#' @return group.names       Column name of grouping variable
#' @return groups            Values of grouping variable
#' @return x                 data.frame of x variables
#' @return y                 Vector of y variable
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
#'         Melanie A. Murphy <melanie.murphy@@uwyo.edu>
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
#' 
#' # Gravity model	
#' x = c("DEPTH_F", "HLI_F", "CTI_F", "cti", "ffp")
#' ( gm <- gravity(y = "DPS", x = x, d = "DISTANCE", group = "FROM_SITE", 
#'                 data = ralu.model, ln = FALSE) )
#'
#'#' # Plot gravity results
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
#' @export gravity
#' @export
gravity <- function (y, x, d, group, data, fit.method = c("REML", "ML"),  
                     ln = TRUE, constrained = TRUE, ...) 
{
  fit.method = fit.method[1]
    if (missing(d)) 
      stop("Distance must be included")
    if (missing(x)) {
      x = d
    }
	back.transform <- function(y) exp(y + 0.5 * stats::var(y))
	rmse = function(p, o){ sqrt(mean((p - o)^2)) }
    x <- unique(c(x, d))
    fixed.call <- stats::as.formula(paste(paste(y, "~", sep = ""), 
                              paste(x, collapse = "+")))
	random.call <- stats::as.formula(paste(paste(y, 1, sep = " ~ "), 
	                                 group, sep = " | "))
	gdata <- data[,c(group, y, x)]
    if (ln == TRUE) {
        gdata[, x] <- log(abs(gdata[, x]))
        gdata[, y] <- log(abs(gdata[, y]))
        gdata[gdata == -Inf] <- 0
        gdata[gdata == Inf] <- 0
    }
    if (constrained == TRUE) {
      print("Running singly-constrained gravity model")
	  gvlmm <- nlme::lme(fixed = stats::as.formula(paste(paste(y, "~", sep = ""), 
                         paste(x, collapse = "+"))), data = gdata, 
						 random=stats::as.formula(paste(paste(y, 1, sep = " ~ "), 
	                     group, sep = " | ")))
		if(fit.method == "ML") gvlmm <- stats::update(gvlmm, method="ML") 
	  gm <- list(fixed.formula = fixed.call, 
	             random.formula = random.call, 
		         gravity = gvlmm, 
                 fit = stats::fitted(gvlmm),
                 AIC = stats::AIC(gvlmm),				 
				 RMSE = rmse(back.transform(stats::fitted(gvlmm)), 
				             back.transform(gdata[,y])), 
                 log.likelihood = gvlmm$logLik,  
				 group.names = group, 
				 groups = gdata[,group], 
				 x = data[,x], y = data[,y], 
				 constrained = constrained)
    } else {
     print("Running unconstrained gravity model, defaulting to OLS. Please check assumptions")
      gvlmm <- stats::lm(stats::as.formula(paste(paste(y, "~", sep = ""), 
                              paste(x, collapse = "+"))), data = gdata)
        gm <- list(formula = fixed.call, gravity = gvlmm, 
		           fit = stats::fitted(gvlmm), AIC = stats::AIC(gvlmm), 
				   RMSE = rmse(back.transform(stats::fitted(gvlmm)), 
				   back.transform(gdata[,y])), 
				   x = data[,x], y = data[,y], 
				   constrained = constrained)	
    }
    class(gm) <- "gravity"
  return(gm)
}
