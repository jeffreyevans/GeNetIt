#' @title Compare gravity models
#' @description Prints diagnostic statistics for comparing gravity models 
#' 
#' @param ...  gravity model objects 
#'
#' @return data.frame of competing model statistics
#'
#' @details Results include model name, AIX, BIC, log likelihood, RMSE and number of parameters
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and Melanie Murphy <melanie.murphy@@uwyo.edu>
#'
#' @references
#' Murphy M.A., R. Dezzani, D.S. Pilliod & A.S. Storfer (2010) Landscape genetics of high mountain frog metapopulations. Molecular Ecology 19(17):3634-3649 
#'
#' @examples  
#' library(nlme)
#'   data(ralu.model)
#' 
#' x = c("DEPTH_F", "HLI_F", "CTI_F", "cti", "ffp")
#' ( gm_null <-  gravity(y = "DPS", x = c("DISTANCE"), d = "DISTANCE",  
#'                  group = "FROM_SITE", data = ralu.model, method = "ML") )
#'
#' ( gm_h1 <- gravity(y = "DPS", x = x, d = "DISTANCE", group = "FROM_SITE", 
#'                  data = ralu.model, ln = FALSE, method="ML") ) 
#' ( gm_h2 <- gravity(y = "DPS", x = x[1:3], d = "DISTANCE", group = "FROM_SITE", 
#'                 data = ralu.model, ln = FALSE, method="ML") ) 
#' ( gm_h3 <- gravity(y = "DPS", x = x[c(4:5)], d = "DISTANCE", group = "FROM_SITE", 
#'                 data = ralu.model, ln = FALSE, method="ML") ) 
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
  rmse <- function(y, x) { sqrt(mean((y - x)^2)) }
    mfun <- function(x, m = method[1]) {
      if( method == "REML" ) {
        xdf <- data.frame(log.likelihood = x$logLik,
                          RMSE = round(rmse(x$y,x$fit),4),
                          nparms = x$np )  
      } else if( method == "ML" ) {
	    if(is.null(x$np)) { np = 0 } else { np = x$np } 
        xdf <- data.frame(AIC = stats::AIC(x),
		                  BIC = stats::BIC(x),
                          log.likelihood = x$logLik,
                          RMSE = round(rmse(x$y,x$fit),4),
						  nparms = np )
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
