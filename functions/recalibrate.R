recalibrate  <- function(shrinkage, 
                         fit, 
                         data = DATA){
  shrunk.coef   <- shrinkage * coef(fit)
  fit.formula   <- formula(fit)
  fit.shrunk    <- lrm(formula = fit.formula, 
                       data = data, 
                       initial = shrunk.coef, 
                       maxit = 1)
  lrm.offset    <- lrm.fit(y = data$y, 
                           offset = predict(fit.shrunk, 
                                            data, 
                                            type = 'lp'))
  new.intercept <- fit.shrunk$coefficients[1] + lrm.offset$coefficients[1]
  fit.shrunk    <- lrm(formula = fit.formula, 
                       data = data, 
                       initial = c(new.intercept, shrunk.coef[-1]), 
                       maxit = 1)
  return(fit.shrunk)
}