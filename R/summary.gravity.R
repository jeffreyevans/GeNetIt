#' @title Summarizing Gravity Model Fits
#' @description Summary method for class "gravity".
#' @param object  Object of class gravity
#' @param ...     Ignored
#' @note Summary of lme or lm gravity model, AIC, log likelihood and Root Mean Square Error (RMSE) of observed verses predicted 
#' @method summary gravity
#' @export
summary.gravity <- function(object, ...) {
  rmse <- function(y, x) { sqrt(mean((y - x)^2)) }
  message("Gravity model summary\n\n") 
  print(object$formula) 
  print(summary(object$gravity))
  message( paste("AIC = ", round(object$AIC,3), sep=""))
  message( paste("log likelihood = ", round(object$log.likelihood,3), sep=""))
  message( paste("RMSE = ", round(rmse(object$y, object$fit),4), sep=""))  
}
