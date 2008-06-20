`csdvsmdd.satter` <-
function(n1, n2, sigma1, sigma2, ate, csd, alpha=.05, alternative="two.sided"){
    alternative <- substring(alternative,1,1)
    mdd <- mdd.satterthwaite(n1,n2,0,sigma1,sigma2,alpha,alternative)
    se <- sqrt(sigma1^2/n1+sigma2^2/n2)
    deg.free <- (sigma1^2/n1+sigma2^2/n2)^2/(sigma1^4/n1^2/(n1-1)+sigma2^4/n2^2/(n2-1))
    prob.sig <- abs(diff(pt((c(mdd,csd)-ate)/se, deg.free)))
    pow <- 1 - pt(ifelse(alternative == "t", qt(1-alpha/2, deg.free), qt(1-alpha, deg.free)) 
        - ate/se, deg.free) 
    if(mdd > csd){
        list(statsig.not.clinsig = 0, clinsig.not.statsig = prob.sig, 
            power = pow)
    } else {
        list(statsig.not.clinsig = prob.sig, clinsig.not.statsig = 0, 
            power = pow)
    }
}

