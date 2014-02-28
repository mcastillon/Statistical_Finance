library("stockPortfolio")
library("optimx")
library("BB")
library("ucminf")
library("Rcgmin")
library("Rvmmin")
library("minqa")

##ticker of stocks
ticker <- c("CAT","TIBX","DB","MSFT","CSCO","MSI","HPQ","MRK","IACI","GE","TW","BLK","BAC","OMC","PFE","SI","OPY","CME","C","YHOO","ABT")
## returns for date range for ticker of stocks
r <- getReturns(ticker,start="2005-12-31",end="2010-12-31")
tickers <- c("CAT","TIBX","DB","MSFT","CSCO","MSI","HPQ","MRK","IACI","GE","TW","BLK","BAC","OMC","PFE","SI","OPY","CME","C","YHOO","ABT","^GSPC")
ind <- c("Farm & Construction Machinery","Business Software & Services","Foreign Regional Banks","Application Software","Networking & Communication Devices","Communication Equipment","Diversified Computer Systems","Drug Manufacturers - Major","Internet Information Providers","Diversified Machinery","Management Services","Asset Management","Money Center Banks","Advertising Agencies","Drug Manufacturers - Major","Diversified Machinery","Investment Brokerage - Regional","Investment Brokerage - National","Money Center Banks","Internet Information Providers","Medical Appliances & Equipment")
## industries
ind <- c("Farm & Construction Machinery","Business Software & Services","Foreign Regional Banks","Application Software","Networking & Communication Devices","Communication Equipment","Diversified Computer Systems","Drug Manufacturers - Major","Internet Information Providers","Diversified Machinery","Management Services","Asset Management","Money Center Banks","Advertising Agencies","Drug Manufacturers - Major","Diversified Machinery","Investment Brokerage - Regional","Investment Brokerage - National","Money Center Banks","Internet Information Providers","Medical Appliances & Equipment","Index")
d <- cbind(ticker,ind)
rd <- getReturns(tickers,start="2005-12-31",end="2010-12-31")
covm <- cov(rd$R)
cor <- getCorr(covm,ind)
cor(r$R)
sap <- getReturns("^GSPC",start="2005-12-31",end="2010-12-31")
reg <- lm(sap$R~r$R)
summary(reg)
mean_r <- 0
var_r <- 0
reg$coefficients
for (i in 1:21){
  a <- mean(r$R[,i])
  b <- var(r$R[,i])
  mean_r <- mean_r+a
  var_r <- var_r+b
}
mean_r
var_r

p_m <- stockModel(rd,Rf=0.02/12,drop=22,industry=ind)
p_m
p_m2 <- stockModel(rd,Rf=.01/12,drop=22,industry=ind)
p_m2
op <- optimalPort(p_m)
op
op2 <- optimalPort(p_m2)
op2
portPossCurve(p_m, xlim=c(0,0.25), ylim=c(-0.03,0.04))
portCloud(p_m, add=TRUE)
points(op, pch=19, add=TRUE)
points(op$risk, op$R, pch=19, col="green")
Rf <- 0.0001
segments(0, Rf, op$risk, op$R)
Rf2 <- 0.0002
segments(0, Rf2, op$risk, op$R)
equal_allo <- matrix(NA,1,21)
for (i in 1:21){
  equal_allo[i] <- p_m$R[i]/21
}
equal_allo_sum <- sum(equal_allo)
equal_allo_sum
p_sim <- stockModel(rd,model='SIM',Rf=.01/12,index=22,industry=ind)
p_sim
p_sim$alpha
p_sim$beta
op_sim <- optimalPort(p_sim)
op_sim
p_sim_ns <- stockModel(rd,model='SIM',Rf=.01/12,index=22,shortSelling=FALSE,industry=ind)
p_sim_ns
op_sim_ns <- optimalPort(p_sim_ns)
op_sim_ns
p_ccm <- stockModel(rd,model='CCM',Rf=.01/12,drop=22,industry=ind)
p_ccm
op_ccm <- optimalPort(p_ccm)
op_ccm
p_ccm_ns <- stockModel(rd,model='CCM',Rf=.01/12,shortSelling="no",drop=22,industry=ind)
p_ccm_ns
op_ccm_ns <- optimalPort(p_ccm_ns)
op_ccm_ns
p_ccm <- stockModel(rd,model='CCM',Rf=.01/12,drop=22,industry=ind)
p_ccm
op_ccm <- optimalPort(p_ccm)
op_ccm
p_ccm_ns <- stockModel(rd,model='CCM',Rf=.01/12,shortSelling="no",drop=22,industry=ind)
p_ccm_ns
op_ccm_ns <- optimalPort(p_ccm_ns)
op_ccm_ns
p_mgm <- stockModel(rd,model='MGM',Rf=.01/12,drop=22,industry=ind)
p_mgm
op_mgm <- optimalPort(p_mgm)
op_mgm
p_mgm_ns <- stockModel(rd,model='MGM',Rf=.01/12,shortSelling="no",drop=22,industry=ind)
p_mgm_ns
op_mgm_ns <- optimalPort(p_mgm_ns)
op_mgm_ns
plot(p_m$R,p_m$sigma)
points(p_m, pch=19, add=TRUE)

op_sim
plot(op_sim)
portPossCurve(p_sim, add=TRUE, riskRange=5)
Rfr <- seq(-5,0,0.0005)

rbar_opt <- rep(0,length(Rfr))
risk_opt <- rep(0,length(Rfr))

for (i in 1:length(Rfr))
{
  simn <- stockModel(rd, model='SIM', index=22, Rf=Rfr[i], shortSelling=FALSE)
  opsimn <- optimalPort(simn)
  rbar_opt[i] <- opsimn$R
  risk_opt[i] <- opsimn$risk
}

points(risk_opt,rbar_opt,type="l" )

p_sim_mvp <- stockModel(rd, model='SIM', index=22, Rf=-100, shortSelling=FALSE)
op_sim_mvp <- optimalPort(p_sim_mvp)
op_sim_mvp
points(op_sim_mvp, optPortOnly=TRUE, colOP='#787838', cex=2)
plot(risk_opt,rbar_opt,type="l")
points(op_sim_mvp, optPortOnly=TRUE, colOP='#787838', cex=2)

portPossCurveShort <- function(stockReturns,model,riskRange=2,detail=100,effFrontier=FALSE,add=FALSE,type="l",xlab="Risk",ylab="Expected Return",doNotPlot=FALSE,...)
{
  if (!model$shorts)
  {
    Rfr <- seq(-5,0,0.0005)
    
    rbar_opt <- rep(0,length(Rfr))
    risk_opt <- rep(0,length(Rfr))

    for (i in 1:length(Rfr))
    {
      sm <- stockModel(stockReturns, model=model$model, drop=model$drop, index=model$index, Rf=Rfr[i], shortSelling=FALSE)
      opsm <- optimalPort(sm)
      rbar_opt[i] <- opsm$R
      risk_opt[i] <- opsm$risk
    }
    
    points(risk_opt,rbar_opt,type="l" )
    
    sm_mvp <- stockModel(rd, model=model$model, index=model$index, Rf=-100, shortSelling=FALSE)
    opsm_mvp <- optimalPort(sm_mvp)
    points(opsm_mvp, optPortOnly=TRUE, colOP='#787838', cex=2)
    plot(risk_opt,rbar_opt,type="l")
    points(opsm_mvp, optPortOnly=TRUE, colOP='#787838', cex=2)
  }
}

new_op <- function (model, Rf = NULL, shortSell = NULL, eps = 10^(-4)) 
{
  if (!is.null(Rf)) {
    model$Rf <- Rf
  }
  if (!is.null(shortSell)) {
    model$shorts <- ifelse(shortSell[1] %in% c("y", "yes", 
                                               "Y", "Yes", "YES", TRUE), TRUE, FALSE)
  }
  if (!model$shorts & model$model == "none") {
    warning("Short sales are always permitted when no model is specified.")
    model$shorts <- TRUE
  }

  op <- list()
  class(op) <- "optimalPortfolio"
  op$model <- model
  op$X <- NA
  op$R <- NA
  op$risk <- NA
  if (model$model == "none") {
    optimalPortUt <- function(model) {
      R <- model$R - model$Rf
      Z <- solve(model$COV) %*% R
      X <- as.numeric(Z/sum(Z))
      names(X) <- rownames(Z)
      ps <- portReturn(list(R = model$R, COV = model$COV), 
                       X)
      return(list(X = X, R = ps$R, VAR = ps$VAR))
    }
    minRiskPortUt <- function(model) {
      if (length(model$R) > 2) {
        MRPM <- minRiskPortMultiUt(model)
        return(MRPM)
      }
      temp <- as.numeric(t(c(1, -1)) %*% model$COV %*% 
                           c(1, -1))
      X <- model$COV[2:1, ] %*% c(1, -1) * c(-1, 1)/temp
      port <- portReturn(model, X)
      R <- sum(X * model$R)
      V <- as.numeric(t(X) %*% model$COV %*% X)
      return(list(X = X, R = port$R, VAR = V))
    }
    minRiskPortMultiUt <- function(model, curveInfo = FALSE) {
      maxRf <- optimalPortUt(model, -1000)$R
      Rf <- maxRf - 0.001 * (1:2)
      G1 <- optimalPortUt(model, Rf[1])
      G2 <- optimalPortUt(model, Rf[2])
      R. <- c(G1$R, G2$R)
      V. <- matrix(NA, 2, 2)
      V.[1, 1] <- G1$VAR
      V.[2, 2] <- G2$VAR
      V.[2, 1] <- V.[1, 2] <- as.numeric(t(G1$X) %*% model$COV %*% 
                                           G2$X)
      MRP <- minRiskPortUt(list(R = R., COV = V.))
      X <- G1$X * MRP$X[1] + G2$X * MRP$X[2]
      if (!curveInfo) {
        return(list(R = MRP$R, VAR = MRP$VAR, X = X))
      }
      else {
        return(list(R = MRP$R, VAR = MRP$VAR, X = X, 
                    G1 = G1))
      }
    }
    OP <- optimalPortUt(model)
    op$X <- OP$X
    op$R <- OP$R
    op$risk <- sqrt(OP$VAR)
  }
  else if (model$model == "SIM") {
    ratio <- (model$R - model$Rf)/model$beta
    o <- order(-ratio)
    alpha <- model$alpha[o]
    beta <- model$beta[o]
    R <- model$R[o]
    MSE <- model$MSE[o]
    ratio <- ratio[o]
    c1 <- (R - model$Rf) * beta/MSE
    c2 <- cumsum(c1)
    c3 <- beta^2/MSE
    c4 <- cumsum(c3)
    Ci <- model$VM * c2/(1 + model$VM * c4)
    cStar <- ifelse(model$shorts, rev(Ci)[1], max(Ci))
    z <- (beta/MSE) * (ratio - cStar)
    t <- ifelse(model$shorts, length(Ci), which.max(Ci)[1])
    X <- z[1:t]/sum(z[1:t])
    temp <- list(R = R[1:t], COV = model$COV[o[1:t], o[1:t]])
    ps <- portReturn(temp, X)
    VAR <- sum(beta[1:t] * X)^2 * model$VM + sum(MSE[1:t] * 
                                                   X^2)
    X <- X[match(model$ticker, names(X))]
    names(X) <- model$ticker
    X[is.na(X)] <- 0
    op$X <- X
    op$R <- ps$R
    op$risk <- sqrt(ps$VAR)
  }
  else if (model$model == "CCM") {
    ratio <- (model$R - model$Rf)/model$sigma
    o <- order(-ratio)
    ratio <- ratio[o]
    R <- model$R[o]
    rhoRatio <- model$rho/(1 + (1:length(model$R) - 1) * 
                             model$rho)
    ratioSum <- cumsum(ratio)
    Ci <- rhoRatio * ratioSum
    cStar <- ifelse(model$shorts, rev(Ci)[1], max(Ci))
    z <- (ratio - cStar)/((1 - model$rho) * model$sigma[o])
    t <- ifelse(model$shorts, length(Ci), which.max(Ci)[1])
    X <- z[1:t]/sum(z[1:t])
    temp <- list(R = R[1:t], COV = model$COV[o[1:t], o[1:t]])
    ps <- portReturn(temp, X)
    X <- X[match(model$ticker, names(X))]
    names(X) <- model$ticker
    X[is.na(X)] <- 0
    op$X <- X
    op$R <- ps$R
    op$risk <- sqrt(ps$VAR)
  }
  else if (model$model == "MGM" && model$shorts) {
    ind <- model$industry
    indU <- unique(model$industry)
    N <- rep(NA, length(indU))
    for (i in 1:length(indU)) {
      N[i] <- sum(ind == indU[i])
    }
    I3 <- diag(rep(1, length(indU)))
    A <- I3 + model$rho * N/(1 - diag(model$rho))
    temp <- diag(model$rho) == 1
    A[temp] <- (1 + model$rho * N/(1 - diag(model$rho)))[temp]
    C <- rep(NA, length(indU))
    ratio <- (model$R - model$Rf)/model$sigma
    for (i in 1:length(indU)) {
      theI <- (ind == indU[i])
      C[i] <- sum(ratio[theI]/(1 - model$rho[i, i]))
      if (model$rho[i, i] == 1) {
        C[i] <- sum(ratio[theI])
      }
    }
    PHI <- as.numeric(solve(A) %*% C)
    names(PHI) <- indU
    z <- rep(NA, length(ind))
    for (i in 1:length(ind)) {
      k <- which(indU == ind[i])
      cStar <- sum(model$rho[k, ] * PHI)
      den <- model$sigma[i] * (1 - model$rho[k, k])
      if (model$rho[k, k] == 1) {
        den <- model$sigma[i]
      }
      z[i] <- (ratio[i] - cStar)/den
    }
    X <- z/sum(z)
    names(X) <- names(model$R)
    ps <- portReturn(model, X)
    op$X <- X
    op$R <- ps$R
    op$risk <- sqrt(ps$VAR)
  }
  else if (model$model == "MGM") {
  }
  return(op)
}

new_curve <- function (model, riskRange = 2, detail = 100, effFrontier = FALSE, 
          add = FALSE, type = "l", xlab = "Risk", ylab = "Expected Return", 
          doNotPlot = FALSE, ...) 
{
  if (!model$shorts) {
    stop("Short selling must be permitted.\n")
  }
  if (!model$shorts & model$model %in% "none") {
    model$shorts <- TRUE
    warning("Short sales are always allowed when no model is provided.\n")
  }
  G1 <- new_op(model, Rf = -1000)
  G2 <- new_op(model, Rf = G1$R - 0.01)
  g1X <- G1$X[match(names(model$R), names(G1$X))]
  g2X <- G2$X[match(names(model$R), names(G2$X))]
  R <- c(G1$R, G2$R)
  COV <- diag(c(G1$risk^2, G2$risk^2))
  COV[1, 2] <- as.numeric(t(g1X) %*% model$COV %*% g2X)
  COV[2, 1] <- COV[1, 2]
  meetRRF <- function(R, COV, X, detail, minRisk, RRF) {
    x <- X
    X <- seq(X[1], X[2], length.out = detail)
    r <- X * R[1] + (1 - X) * R[2]
    v <- X^2 * COV[1, 1] + (1 - X)^2 * COV[2, 2] + 2 * X * 
      (1 - X) * COV[1, 2]
    trim <- TRUE
    if (sqrt(v[1]) < RRF * minRisk) {
      x[1] <- 2 * x[1]
      trim <- FALSE
    }
    if (sqrt(rev(v)[1]) < RRF * minRisk) {
      x[2] <- 2 * x[2]
      trim <- FALSE
    }
    if (trim) {
      these <- sqrt(v) < RRF * minRisk
      if (sum(these) > detail/2) {
        out <- list()
        out$X <- X[these]
        out$R <- r[these]
        out$V <- v[these]
      }
      else {
        x[1] <- (x[1] - 1) * 0.75 + 1
        x[2] <- (x[2] - 1) * 0.75 + 1
        out <- meetRRF(R, COV, X = x, detail = detail, 
                       minRisk = minRisk, RRF = RRF)
      }
    }
    else {
      out <- meetRRF(R, COV, X = x, detail = detail, minRisk = minRisk, 
                     RRF = RRF)
    }
    return(list(R = out$R, V = out$V, X = out$X))
  }
  mRRF <- meetRRF(R, COV, X = c(-3, 5), detail = detail, minRisk = G1$risk, 
                  RRF = riskRange)
  if (effFrontier) {
    these <- which(diff(mRRF$V) < 0)
    mRRF$R <- mRRF$R[these]
    mRRF$V <- mRRF$V[these]
    mRRF$X <- mRRF$X[these]
  }
  ports <- mRRF$X %*% t(g1X) + (1 - mRRF$X) %*% t(g2X)
  toReturn <- list(R = mRRF$R, risk = sqrt(mRRF$V), ports = ports)
  if (add & !doNotPlot) {
    lines(toReturn$risk, toReturn$R, type = type, ...)
  }
  else if (!doNotPlot) {
    plot(toReturn$risk, toReturn$R, type = type, xlab = xlab, 
         ylab = ylab, ...)
  }
  invisible(toReturn)
}

p_sim_k <- stockModel(rd,model='SIM',Rf=1,index=22,industry=ind)
op_sim_k <- new_op(p_sim_k)
new_curve(p_sim_k,add=TRUE)
points(op_sim_k, pch=19, add=TRUE)
points(op_sim_k$risk, op_sim_k$R, pch=19, col="green")

library("parma")
p_minrisk <- parmaspec(S=covm,risk="EV",targetType="equality",riskType="minrisk")
parmasolve(p_minrisk)
parmafrontier(p_minrisk)
plot(parmafrontier(p_minrisk))

vari <- var(rd$R)
corr <- cor(rd$R)

sigma <- vari * corr
x_i <- rep(1/22,22)
x_i

xsx <- function (x)
{
  (t(x) %*% sigma %*% x)[1,1]
}

xsx(x_i)

optimx(x_i,fn=xsx)$par