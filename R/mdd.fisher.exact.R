`mdd.fisher.exact` <-
function(placebo.size, treat.size, prob.placebo, alpha=.05, pow=.8, 
        alternative="two.sided"){
    alternative <- substr(alternative,1,1)
    mat.tmp <- matrix(0,nrow=treat.size+1,ncol=placebo.size+1)
    if(alternative=="g"){
        sw <- TRUE
        alternative <- "l"
    } else {
        sw <- FALSE
    }
    x <- treat.size
    y <- placebo.size
    while(x >=0 && y >=0){
        mat.tmp[x+1,y+1] <- (fisher.test(matrix(c(x,treat.size-x,y,placebo.size-y),ncol=2),
            alternative=alternative)$p.value<=alpha)
        if(mat.tmp[x+1,y+1]==1){
            if(x>0){
                mat.tmp[1:x,y+1] <- 1
            }
            y <- y-1
            x <- x+1
        }
        x <- x-1
    }
    if(alternative=="t"){
        mat.tmp <- mat.tmp + mat.tmp[(treat.size+1):1,(placebo.size+1):1]
    }
    if(sw){
        mat.tmp <- mat.tmp[(treat.size+1):1,(placebo.size+1):1]
        alternative <- "g"
    }
    sig.th <- function(theta){sum((dbinom(0:treat.size,treat.size,theta) %o% 
        dbinom(0:placebo.size,placebo.size,prob.placebo)) * mat.tmp)}
    if(alternative!="g"){
        if(sig.th(0) > pow){ 
            low.theta <- uniroot(function(x){sig.th(x)-pow},c(0,prob.placebo), tol=10^-9)$root
        } else {
            low.theta <- NA
        }
    }
    if(alternative!="l"){
        if(sig.th(1) > pow){ 
            up.theta <- uniroot(function(x){sig.th(x)-pow},c(prob.placebo,1), tol=10^-9)$root
        } else {
            up.theta <- NA
        }
    }
    ret <- list()
    ret$actual <- switch(alternative,
        t = c(low.theta,up.theta),
        l = low.theta,,
        g = up.theta
    )
    mdds <- rbind(0:placebo.size,sapply(0:placebo.size,function(x){
       c(.mina(mat.tmp[,x+1])-1, .maxa(mat.tmp[,x+1]))}))
    colnames(mdds) <- rep("",dim(mdds)[2])
    rownames(mdds) <- c("Control","Treatment","Treatment")
    ret$observed <- switch(alternative,
        t = cbind(t(mdds),t(mdds/c(placebo.size,treat.size,treat.size))),
        l = cbind(t(mdds[-3,]),t(mdds[-3,]/c(placebo.size,treat.size))),
        g = cbind(t(mdds[-2,]),t(mdds[-2,]/c(placebo.size,treat.size)))
    )
    ret
}

