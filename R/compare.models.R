#' @title Compare gravity models
#' @description Prints diagnostic statistics for comparing gravity models 
#' 
#' @param ...  gravity model objects 
#'
#' @return data.frame of competing model statistics
#'
#' @details 
#' Results include model name, AIX, BIC, log likelihood, RMSE and number of parameters
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and 
#'         Melanie A. Murphy <melanie.murphy@@uwyo.edu>
#'
#' @references
#' Murphy M.A., R. Dezzani, D.S. Pilliod & A.S. Storfer (2010) Landscape genetics of 
#'   high mountain frog metapopulations. Molecular Ecology 19(17):3634-3649 
#'
#' @examples  
#' library(nlme)
#'   data(ralu.model)
#' 
#' x = c("DEPTH_F", "HLI_F", "CTI_F", "cti", "ffp")
#' ( null <-  gravity(y = "DPS", x = c("DISTANCE"), d = "DISTANCE",  
#'                    group = "FROM_SITE", data = ralu.model, fit.method = "ML") )
#' ( gm_h1 <- gravity(y = "DPS", x = x, d = "DISTANCE", group = "FROM_SITE", 
#'                    data = ralu.model, ln = FALSE, fit.method="ML") ) 
#' ( gm_h2 <- gravity(y = "DPS", x = x[1:3], d = "DISTANCE", group = "FROM_SITE", 
#'                    data = ralu.model, ln = FALSE, fit.method="ML") ) 
#' ( gm_h3 <- gravity(y = "DPS", x = x[c(4:5)], d = "DISTANCE", group = "FROM_SITE", 
#'                    data = ralu.model, ln = FALSE, fit.method="ML") ) 
#' #( gm_h4 <- gravity(y = "DPS", x = x[c(4:5)], d = "DISTANCE", group = "FROM_SITE", 
#' #                   data = ralu.model, ln = FALSE, fit.method="REML") ) 
#' 
#' compare.models(null, gm_h1, gm_h2, gm_h3)
#'
#' @export compare.models 
compare.models <- function(...) {
  dots <- list(...)
    #mn <- deparse(substitute(list(...)))
	#mn <- regmatches(mn, gregexpr("(?<=\\().*?(?=\\))", mn, perl=TRUE))[[1]]
    #mn <- trimws(unlist(strsplit(mn, ",")))		
    varnames = lapply(substitute(list(...))[-1], deparse)
      mn <- unlist(lapply(varnames, as.character))	
	  for(i in 1:length(dots)){ 
	    if(!inherits(dots[[i]], "gravity")) 
		  stop(paste0(mn[i], " is not a valid gravity model object") ) 
	    dots[[i]]$gravity$y <- dots[[i]]$y
		dots[[i]]$gravity$np <- ncol(dots[[i]]$x) 
        dots[[i]] <- dots[[i]]$gravity 		
	  }
  method <- unique(unlist(lapply(dots, function(x) x$method)))
  if( length(method) > 1 ) {
    cat(paste(mn, unlist(lapply(dots, function(x) x$method)), sep="-"), "\n")
    stop("Models were fit with ML and REML and are not comparable")
  }
  if( any(method == "REML") ) {
    warning("AIC/BIC not valid under REML and will not be reported")
  }
  back.transform <- function(y) exp(y + 0.5 * stats::var(y))
  rmse <- function(y, x) { sqrt(mean((y - x)^2)) }
    mfun <- function(x) {
	  method <- x$method
	  np = ncol(x$data)-1
      if( method == "REML" ) {
        xdf <- data.frame(log.likelihood = x$logLik,
                          RMSE = round(rmse(back.transform(x$y), 
						  back.transform(x$fit[,"fixed"])),4),
                          nparms = np, fit.method=method)  
      } else if( method == "ML" ) {
        xdf <- data.frame(AIC = stats::AIC(x),
		                  BIC = stats::BIC(x),
                          log.likelihood = x$logLik,
                          RMSE = round(rmse(back.transform(x$y), 
						  back.transform(x$fit[,"fixed"])),4),
						  nparms = np, fit.method=method)
      }
	  return(xdf)  
	}
    ldf <- do.call("rbind", lapply(dots, mfun))
	  if(method == "ML") { 
	    ldf$deltaAIC = ldf$AIC - min(ldf$AIC)
        ldf$deltaBIC = ldf$BIC - min(ldf$BIC)		
	  } 
  return( data.frame(model = as.character(mn), ldf) )
}
