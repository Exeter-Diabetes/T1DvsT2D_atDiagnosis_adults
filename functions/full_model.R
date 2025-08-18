# DATA <- SR_dd %>%
#   dplyr::select(-famhisnoninsdiab) %>%
#   rename(y = SRoutcome) %>% 
#   as.data.frame()
# 
# B = 100

full_model   <- function(data.d = DATA, 
                         B = B, 
                         data.v = DATA.val, 
                         n.cal.grid = 100){
  source("functions/recalibrate.r")
  source("functions/val.perf.r")
  ### fit maximum likelihood full model (no variable selection)
  fit.full <- lrm(y ~ ., 
                  data = data.d, 
                  x = T, 
                  y = T)
  
  if(!fit.full$fail){
    ### get apparent performance of full model (full.apparent)
    apparent <- as.numeric(val.perf(data = data.d, 
                                    fit = fit.full))
    val      <- as.numeric(val.perf(data = data.v, 
                                    fit = fit.full))
    
    # bootstrap internal validation of the full model to get the optimism-corrected bootstrap
    # calibration slope
    val.full  <- rms::validate(fit.full, 
                               bw = F, 
                               B = B, 
                               pr = F, 
                               estimates = F)
    xx        <- c(as.vector(val.full[c("Dxy",
                                        "R2", 
                                        "Intercept",
                                        "Slope"), 
                                      'index.corrected']))
    boot.opt  <- c((1 + xx[1])/2, xx[-1])
    
    ## apply the bootstrap corrected slope shrinkage factor (and re-estimate the intercept)
    fit.shrunk.full <- recalibrate(shrinkage = boot.opt[4], 
                                   fit = fit.full, 
                                   data = data.d)
    shrunk.apparent <- as.numeric(val.perf(data = data.d, 
                                           fit = fit.shrunk.full))
    shrunk.val      <- as.numeric(val.perf(data = data.v, 
                                           fit = fit.shrunk.full))
    
    pred.val <- predict(fit.full, 
                        newdata = data.v, 
                        type = 'fitted')
    Sm       <- lowess(pred.val, 
                       data.v$y, 
                       iter = 0)
    pp.full  <- seq(min(pred.val), 
                    max(pred.val), 
                    length = n.cal.grid)
    Sm.full  <- approx(Sm, 
                       xout = pp.full, 
                       ties = mean)$y
    
    pred.shrunk <- predict(fit.shrunk.full, 
                           newdata = data.v, 
                           type = 'fitted')
    Sm          <- lowess(pred.shrunk, 
                          data.v$y, 
                          iter = 0)
    pp.shrunk   <- seq(min(pred.shrunk), 
                       max(pred.shrunk), 
                       length = n.cal.grid)
    Sm.shrunk   <- approx(Sm, 
                          xout = pp.shrunk, 
                          ties = mean)$y 
  } else {
    apparent <- NA
    val      <- NA
    boot.opt  <- NA
    shrunk.apparent <- NA
    shrunk.val <- NA
    pp.full <- rep(NA, n.cal.grid)
    Sm.full <- rep(NA, n.cal.grid)
    pp.shrunk <- rep(NA, n.cal.grid)
    Sm.shrunk <- rep(NA, n.cal.grid)
    fit.full <- NA
    fit.shrunk.full <- NA
    boot.opt[4] <- NA
  }
  
  return(list(apparent        = apparent, 
              val             = val, 
              opt.cor         = boot.opt,
              shrunk.apparent = shrunk.apparent,
              shrunk.val      = shrunk.val,
              fit.cal.x       = pp.full,
              fit.cal.y       = Sm.full,
              shrunk.cal.x    = pp.shrunk,
              shrunk.cal.y    = Sm.shrunk,
              fit             = fit.full,
              fit.shrunk      = fit.shrunk.full, 
              shrinkage.f     = boot.opt[4]))
}

