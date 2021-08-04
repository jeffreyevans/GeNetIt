#' @title Predict gravity model
#' @description predict method for class "gravity"
#'
#' @param object            Object of class gravity
#' @param newdata           New data used for obtaining the predictions, can
#'                          be a data.frame or nffGroupedData
#' @param groups            Grouping factor acting as random effect. If used,
#'                          must match levels used in model, otherwise leave it
#'                          null and do not convert to groupedData  
#' @param back.transform    Method to back transform data, default is none and 
#'                          log predictions will be returned. 
#'  
#' @param ...               Arguments passed to predict.lme or predict.lm
#'
#' @return Vector of model predictions
#'
#' @details 
#' The simple back-transform method uses the form exp(y-hat)0.5*variance whereas
#' Miller uses exp(sigma)*05 as the multiplicative bias factor. Naihua regresses 
#' y~exp(y-hat) with no intercept and uses the resulting coefficient 
#' as the multiplicative bias factor. The default to output the log scaled 
#' predictions.   
#'
#' @references
#' Miller, D.M. (1984) Reducing Transformation Bias in Curve Fitting
#'   The American Statistician. 38(2):124-126
#' @references
#' Naihua, D. (1983) Smearing Estimate: A Nonparametric Retransformation Method 
#'   Journal of the American Statistical Association, 78(383):605–610. 
#' 
#' @examples 
#' library(nlme)
#' data(ralu.model)
#' 
#' back.transform <- function(y) exp(y + 0.5 * stats::var(y))
#' rmse = function(p, o){ sqrt(mean((p - o)^2)) } 
#' 
#' x = c("DEPTH_F", "HLI_F", "CTI_F", "cti", "ffp")
#'  
#' sidx <- sample(1:nrow(ralu.model), 100) 
#'   train <- ralu.model[sidx,]
#'   test <- ralu.model[-sidx,]
#'  
#'  # Specify constrained gravity model	
#'  ( gm <- gravity(y = "DPS", x = x, d = "DISTANCE", group = "FROM_SITE", 
#'                  data = train, ln = FALSE) )
#'   
#' ( p <- predict(gm, test[,c(x, "DISTANCE")]) )
#'   rmse(back.transform(p), back.transform(ralu.model[,"DPS"][-sidx]))
#' 
#' # WIth model sigma-based back transformation
#' ( p <- predict(gm, test[,c(x, "DISTANCE")], back.transform = "simple") )
#' ( p <- predict(gm, test[,c(x, "DISTANCE")], back.transform = "Miller") )
#' ( p <- predict(gm, test[,c(x, "DISTANCE")], back.transform = "Naihua") )
#'
#'  
#' # Using grouped data
#' test <- nlme::groupedData(stats::as.formula(paste(paste("DPS", 1, sep = " ~ "), 
#'           "FROM_SITE", sep = " | ")), 
#' 		  data = test[,c("DPS", "FROM_SITE", x, "DISTANCE")])
#' 
#' ( p <- predict(gm, test, groups = "FROM_SITE") )
#'   rmse(back.transform(p), back.transform(ralu.model[,"DPS"][-sidx]))
#'
#' # Specify unconstrained gravity model (generally, not recommended)	
#' ( gm <- gravity(y = "DPS", x = x, d = "DISTANCE", group = "FROM_SITE", 
#'                 data = train, ln = FALSE, constrained=TRUE) )
#' 
#' ( p <- predict(gm, test[,c(x, "DISTANCE")]) )
#'   rmse(back.transform(p), back.transform(ralu.model[,"DPS"][-sidx])) 
#'
#' @method predict gravity
#' @export
predict.gravity <- function (object, newdata, groups = NULL, 
                             back.transform = c("none", "simple", "Miller", "Naihua"), ...) {
  if(class(object$gravity) == "lme") { 					   
  m <- do.call("lme.formula", list(fixed = object$fixed.formula,
            data = object$gravity$data, 
            random = object$random.formula))
    if(!is.null(groups)) {
      if(class(newdata)[1] != "nffGroupedData")
        stop("newdata must be grouped using nlme::groupedData")
		  g <- newdata[,groups]
	      message("Making group level (individual) predictions")
		p <- stats::predict(m, newdata, Q = g, ...)
	  } else {
        message("Making population level predictions")	  
		p <- stats::predict(m, newdata, level = 0, ...)
	  } 
  } else if(class(object$gravity) == "lm") {
    p <- stats::predict(object$gravity, newdata, ...)
  } 
	if(back.transform != "none") {	  
	  if(back.transform == "simple") {
	    message("Back-transforming exp(y-hat)*0.5*variance(y-hat), 
		  assumes normally distributed errors")
	    p <- exp(p + 0.5 * stats::var(p))    
	  } else if(back.transform == "Miller") {
	    message("Miller back-transformation using: 
		  exp(sigma)*0.5*exp(y-hat)0.5*variance(y-hat), 
		  assumes normally distributed errors")	  
        p <- exp((summary(object$gravity)$sigma)*0.5) * exp(p + 0.5 * stats::var(p))	
      } else if(back.transform == "Naihua") {
    message("Naihua back-transformation using: 
	  exp(y-hat) ~ y-hat regression with no intercept,
	  does not assume normally distributed errors")	  
	    p1 <- exp(object$gravity$fitted[,1])
		  y <- stats::coef(lm(object$y-0 ~ p1))[2]
		p <- y * exp(p)	
	  }
	}
  return(p)
}
