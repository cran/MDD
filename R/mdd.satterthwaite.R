`mdd.satterthwaite` <-
function(n1, n2, mu1, sigma1, sigma2, alpha=.05, alternative="two.sided", 
        return.cis=FALSE){
    alternative <- substr(alternative,1,1)[1]
    degfree <- (sigma1^2/n1+sigma2^2/n2)^2/(sigma1^4/n1^2/(n1-1)+sigma2^4/n2^2/(n2-1))
    delta <- sqrt(sigma1^2/n1+sigma2^2/n2) * switch(alternative,
        t = qt(1-alpha/2,degfree),
        g = qt(1-alpha,degfree),
        l = qt(alpha,degfree)
    )
    if(!return.cis){
        abs(delta)
    } else {
        wh <- switch(alternative,
            t = c(-1,1),
            l = c(Inf,1),
            g = c(1,Inf)
        )
        mu1 + drop(delta %o% wh)
    }
}

