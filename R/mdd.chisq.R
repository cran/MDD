`mdd.chisq` <-
function(placebo.size, treat.size, prob.placebo, alpha=.05, pow=.8){
    mat.tmp <- matrix(0,nrow=treat.size+1,ncol=placebo.size+1)
    x <- treat.size
    y <- placebo.size
    while(x >=0 && y >=0){
        mat.tmp[x+1,y+1] <- (chisq.test(matrix(c(x,treat.size-x,y,placebo.size-y),ncol=2))$p.value
            <= alpha)
        if(treat.size==x && placebo.size==y){
            mat.tmp[x+1,y+1] <- 0    
        }
        if(mat.tmp[x+1,y+1]==1){
            if(x>0){
                mat.tmp[1:x,y+1] <- 1
            }
            y <- y-1
            x <- x+1
        }
        x <- x-1
    }
    mat.tmp <- mat.tmp + mat.tmp[(treat.size+1):1,(placebo.size+1):1]
    sig.th <- function(theta){sum((dbinom(0:treat.size,treat.size,theta) %o% 
        dbinom(0:placebo.size,placebo.size,prob.placebo)) * mat.tmp)}
    if(sig.th(0) > pow){ 
        low.theta <- uniroot(function(x){sig.th(x)-pow},c(0,prob.placebo), tol=10^-9)$root
    } else {
        low.theta <- NA
    }
    if(sig.th(1) > pow){ 
        up.theta <- uniroot(function(x){sig.th(x)-pow},c(prob.placebo,1), tol=10^-9)$root
    } else {
        up.theta <- NA
    }
    ret <- list()
    ret$actual <- c(low.theta,up.theta)
    mdds <- rbind(0:placebo.size,sapply(0:placebo.size,function(x){
       c(.mina(mat.tmp[,x+1])-1, .maxa(mat.tmp[,x+1]))}))
    colnames(mdds) <- rep("",dim(mdds)[2])
    rownames(mdds) <- c("Control","Treatment","Treatment")
    ret$observed <- cbind(t(mdds),t(mdds/c(placebo.size,treat.size,treat.size)))
    ret
}

