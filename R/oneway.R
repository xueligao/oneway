
  ## 1. Develop a default method, **oneway.default**,  for the  generic function **oneway**.

  oneway <- function(z, ...) UseMethod("oneway")

  onewayEst <- function(z)
  {


    s=sapply(z,sum)
    Total= sum(unlist(z)^2)
    group_n<-sapply(z,length)
    A=sum(s^2/group_n)

    CF<-(sum(unlist(z)))^2/length(unlist(z))

    SS_between <- A-CF
    SS_within <- Total-A
    df_between <- length(z)-1
    df_within <- length(unlist(z))-length(z)
    MSb<-(A-CF)/(length(z)-1)
    MSw<-(Total-A)/(length(unlist(z))-length(z))
    F<-MSb/MSw
    p<-1-pf(F, df_between,df_within, lower.tail=T)

    anotable=cbind(df=c(df_between, df_within),SS=c(SS_between, SS_within), MS=c(MSb,MSw), F=c(F,NA),"Pr(<F)"=c(p,NA))
    rownames(anotable)<-c("Treatment", "Residuals")
    printCoefmat(anotable, P.values = T,has.Pvalue = T, signif.stars = T, na.print = "")
  }

  oneway.default <- function(z, ...) {
    est <- onewayEst(z)
    est$call <- match.call()
    class(est) <- "oneway"
    est
  }

  ## 2. This method uses the more standard input of a factor representing groups (or samples) and a numeric response.

  oneway.factor <- function(z, y, ...) {
    x<-split(y, z)
    result<-oneway(x)

    result
  }

  ## 3. The model formula is the standard for R models, but do not use **model.matrix** to implement **oneway**.

  oneway.formula <- function(formula, data=list(), ...) {

    mf <- model.frame(formula=formula, data=data)
    x <- mf[,2]
    y <- model.response(mf)

    est <- oneway.factor(x, y, ...)
    est$call <- match.call()
    est$formula <- formula

    est

  }

  ## 4. The default **print** method should be short and provide essential information.

  print.oneway <- function(x, ...) {
    cat("Call:\n")
    print(x$call)
    cat("SS Between group")
    print(x[2])
    cat("\nSS within group")
    print(x[3])
    cat("\ndf Between group")
    print(x[4])
    cat("\ndf within group")
    print(x[5])
  }

  ## 5. The summary method should create a summary object---not print directly.

  summary.oneway <- function(object, ...) {
    MS_between=object[[3]]/object[[1]]
    MS_within=object[[4]]/object[[2]]
    F=MS_between/MS_within
    Pr=1-pf(F, object[[2]],object[[1]], lower.tail=T)
    result=data.frame(Df=c(object[[1]],object[[2]]),
                      SS=c(object[[3]],object[[4]]),
                      MS=c(MS_between, MS_within),
                      Fvalue=c(F,NA),
                      Pr=c(Pr,NA))
    row.names(result)<-c("Between group", "Within group")
    result
  }


  ## 6. The print method for the summary object should provide more detailed information about the summary object.
  print.summary.oneway <- function(x, ...) {
    cat("Call:\n")
    print(x$call)
    cat("\n")
  }


 

