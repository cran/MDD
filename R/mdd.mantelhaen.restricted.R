`mdd.mantelhaen.restricted` <-
function(placebo.size, treat.size, placebo.vals, alpha=.05, 
        alternative="two.sided", exact=TRUE, print.summary=TRUE){
    require(partitions)
    alternative <- substr(alternative,1,1)
    tmp <- mdd.fisher.exact(sum(placebo.size), sum(treat.size), .5, alpha, .2, 
        alternative)$observed[sum(placebo.vals)+1,]
    ret <- list(le = list(insig = list(), sig = list(), unknown = list()),
                gr = list(insig = list(), sig = list(), unknown = list()))
    if(alternative!="g"){
        ret$le$unknown <- blockparts(treat.size,tmp[2])
        if(alternative=="t"){
            ret$gr$unknown <- blockparts(treat.size,tmp[3])    
        }
    } else {
        ret$gr$unknown <- blockparts(treat.size,tmp[2])
    }
    for(i in c("le","gr")){
        if(length(ret[[i]]$unknown)){
            ret[[i]]$unknown <- lapply(1:dim(ret[[i]]$unknown)[2], 
                function(x){ret[[i]]$unknown[,x]})
        }
    }
    maxes <- as.vector(rbind(treat.size,placebo.size))
    iter.fcn <- list(le = .one.less, gr = function(x){.one.more(x,treat.size)})
    gr.par <- list(le = FALSE, gr = TRUE)
    for(d in 1:2){
        while(length(ret[[d]]$unknown)){
            ind <- (.mhstat(as.vector(rbind(ret[[d]]$unknown[[1]],placebo.vals)), maxes,
                alternative=alternative, exact=exact) <= alpha) + 1
            ret[[c(d,ind)]] <- c(ret[[c(d,ind)]],list(ret[[d]]$unknown[[1]]))
            ret[[d]]$unknown[[1]] <- NULL
            if(!length(ret[[d]]$unknown)){
                ret[[d]]$unknown <- setdiff(union(iter.fcn[[d]](ret[[d]]$insig), 
                    iter.fcn[[3-d]](ret[[d]]$sig)), union(ret[[d]]$sig, ret[[d]]$insig))    
            }
        }
        if(length(ret[[d]]$sig)){
            ret[[d]]$sig <- do.call(rbind,ret[[d]]$sig)
            ret[[d]]$sig <- ret[[d]]$sig[do.call(order, data.frame(ret[[d]]$sig)),,drop=FALSE]
        } else {
            ret[[d]]$sig <- matrix(0,nrow=0,ncol=length(placebo.size))
        }
    }
    ret <- list(le = .compress.list(ret$le$sig), gr = .compress.list(ret$gr$sig, gr=TRUE))
    if(print.summary){
        .explanation.mantelhaen(placebo.size, treat.size, placebo.vals, ret)
        invisible(ret)
    } else {
        ret
    }
}

