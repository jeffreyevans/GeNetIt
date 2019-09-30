#' @title Effect Size 
#' @description Cohen's D effect size for gravity models
#' 
#' @param x          gravity model object 
#' @param alpha      confidence interval
#' @param actual.n   (FALSE/TRUE) Use actual N or degrees of freedom in calculating Confidence Interval
#'
#' @return data.frame of parameter effect size
#'
#' @details Calculate Cohen's D statistic for each effect in a gravity model object
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and Melanie Murphy <melanie.murphy@@uwyo.edu>
#'
#' @references
#' Murphy M.A., R. Dezzani, D.S. Pilliod & A.S. Storfer (2010) Landscape genetics of high mountain frog metapopulations. Molecular Ecology 19(17):3634-3649 
#' @references
#' Cohen, J. (1988) Statistical power for the behavioral sciences (2nd ed.). Hillsdale, NJ: Erlbaum 
#'
#' @examples  
#' library(nlme)
#'   data(ralu.model)
#' 
#' x = c("DEPTH_F", "HLI_F", "CTI_F", "cti", "ffp")
#' gm_h1 <- gravity(y = "DPS", x = x, d = "DISTANCE", group = "FROM_SITE", 
#'                 data = ralu.model, ln = FALSE, method="ML") 
#' 
#' gravity.es(gm_h1)
#'
#' @export gravity.es
gravity.es <- function(x, actual.n = FALSE, alpha = 0.95) {
  if(!inherits(x, "gravity")) 
    stop(x, " is not a valid gravity model object")	
  eff <- data.frame(t.value = summary(x$gravity)$tTable[,4], 
                    df = summary(x$gravity)$fixDF$terms)
    eff$cohen.d <- (2 * eff$t.value) / sqrt(eff$df)
  eff <- eff[-1,]
    eff <- data.frame(eff, p.value=round(summary(x$gravity)$tTable[,5],6)[-1]) 
    cohen.ci <- function(d, n, conf.level = alpha) {
      deg.f = n + n - 2
      SD <- sqrt(((n + n)/(n * n) + 0.5 * es[,1][i] ^ 2 / deg.f) * 
                ((n + n) / deg.f))
            Z <- -stats::qt((1 - alpha) / 2, deg.f)
          conf.int <- c(d - Z * SD, d + Z * SD)
	    ci <- c(low.ci=conf.int[1], up.ci=conf.int[2])
      return(ci)
    }
    ci <- list()	
	  for(i in 1:nrow(eff)) {
	    if(actual.n) N = length(x$y) else N = eff[,2][i] 
	      ci[[i]] <- cohen.ci(d = eff[,3][i], n = N)
	  }
    ci <- as.data.frame(do.call("rbind", ci))
  return(data.frame(eff, ci))
}
