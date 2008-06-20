`mdd.t.test` <-
function(n1, n2, mu1, sigma, alpha=.05, alternative="two.sided", 
        return.cis=FALSE){
    alternative <- substr(alternative,1,1)[1]
    delta <- sigma * sqrt(1/n1+1/n2) * switch(alternative,
        t = qt(1-alpha/2,n1+n2-2),
        g = qt(1-alpha,n1+n2-2),
        l = qt(alpha,n1+n2-2)
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

