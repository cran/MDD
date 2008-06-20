`mdd.mantelhaen.pow` <-
function(placebo.size, treat.size, prob.placebo, alpha=.05, pow=.8,
        alternative="two.sided", exact=TRUE, which.strata="all", size.matrix, 
        to.file=character(0), from.file=character(0)){
    if(length(prob.placebo)>1){
        prob.placebo <- prob.placebo[1]
        warning(paste("Assuming all control strata have response rate 'prob.placebo[1] = ",
            prob.placebo[1], "'", sep=""), call.=FALSE)     
    }
    alternative <- substr(alternative,1,1)
    if(identical(from.file,character(0))){
        if(missing(placebo.size)){
            placebo.size <- size.matrix[,1]
            treat.size <- size.matrix[,2]
        }
        if(missing(size.matrix)){
            size.matrix <- cbind(placebo.size,treat.size)
        }
        sig.inds <- .mantelhaen(placebo.size, treat.size, alpha, alternative, exact)
    } else {
        if(nchar(from.file)){
            load(from.file)
        } else {
            load(choose.files(multi=FALSE,filters=Filters["RData",]))
        }
        placebo.size <- dim(sig.inds)[2*(1:(length(dim(sig.inds))/2))] - 1
        treat.size <- dim(sig.inds)[2*(1:(length(dim(sig.inds))/2))-1] - 1
        if(missing(size.matrix)){
            size.matrix <- rbind(treat.size,placebo.size)
        }
    }
    if(!identical(to.file,character(0))){
        if(!length(grep("\\.RData$",to.file))){
            to.file <- paste(to.file,".RData",sep="")
        }
        save(sig.inds,file=to.file)
    }
    if(identical(which.strata,"all")){
        which.strata <- .all.subs(length(placebo.size))
    }
    if(length(prob.placebo)==1){
        prob.placebo <- rep(prob.placebo,length(placebo.size))
    }
    ret <- t(sapply(which.strata, function(i){
        sig.th <- function(theta){
            prob.treat <- prob.placebo
            prob.treat[i] <- theta
            probs <- dbinom(0:treat.size[1],treat.size[1],prob.treat[1]) %o% 
                    dbinom(0:placebo.size[1],placebo.size[1],prob.placebo[1])
            j <- 1
            while((j <- j+1) <= length(placebo.size)){
                probs <- probs %o% (dbinom(0:treat.size[j],treat.size[j],prob.treat[j]) %o% 
                    dbinom(0:placebo.size[j],placebo.size[j],prob.placebo[j]))    
            }
            sum(sig.inds*probs)
        }
        if(alternative!="g"){
            if(sig.th(0) > pow){ 
                low.theta <- uniroot(function(x){sig.th(x)-pow},
                    c(0,max(prob.placebo)), tol=10^-9)$root
            } else {
                low.theta <- NA
            }
        }
        if(alternative!="l"){
            if(sig.th(1) > pow){ 
                up.theta <- uniroot(function(x){sig.th(x)-pow},
                    c(min(prob.placebo),1), tol=10^-9)$root
            } else {
                up.theta <- NA
            }
        }    
        switch(alternative,
            t = c(low.theta,up.theta),
            l = low.theta,
            g = up.theta
        )
    }))
    ret <- matrix(ret, ncol=ifelse(alternative=="t",2,1))
    rownames(ret) <- sapply(which.strata,function(x){paste(x,collapse=",")})
    ret
}

