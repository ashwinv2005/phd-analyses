predict.coxme <- function(object, newdata, type=c("lp", "risk"), time, se=TRUE, level=0.95) 
{
  alpha <- 1-level
  # This is an early skeleton of the function
  type <-match.arg(type)
  n <- object$n
  Terms <-  object$terms
 
  if (missing(newdata))
  {
    out <- object$linear.predictor
    if(type == 'risk')
      out <- exp(out)
  }
  
  else
  {

    basehazard <- summary(survfit(object$y~1), time=time)
    lambda <- 1- basehazard$surv
    lambda.se <- basehazard$std.err
    
    form <- object$formulaList$fixed
    form <-  stats::update(form, NULL~.) 
    mm <- model.matrix(form, data=newdata)[, -1]
    
    out <- mm %*% fixef(object)
    
    out.se <- sqrt(diag(mm %*% vcov(object) %*% t(mm)))


    

    if (type=="risk") 
    
    {
      ## this is epxerimental and probably shouldn't be used 
      # out.se <- prod_se(x=lambda, y=exp(out), se.x=lambda.se, se.y=exp(out.se),
      #                   n=object$n[2])
      
      out.lcl <- lambda*exp(out - (qnorm(1-alpha/2))*out.se)
      out.ucl <- lambda*exp(out + (qnorm(1-alpha/2))*out.se)
      
      out <- lambda*exp(out)
      out <- data.frame(pred=out, pred.se=out.se, 
                        pred.lcl=out.lcl, pred.ucl = out.ucl)
    }
    
    else
      out <- data.frame(pred=out, pred.se=out.se)
  }
  return(out)
}

prod_se <- function(x, y, se.x, se.y, n)
{
  se.xy <- (x*y/sqrt(n)) * sqrt( (se.x/x)^2 + (se.y/y)^2 )
  return(se.xy)
}