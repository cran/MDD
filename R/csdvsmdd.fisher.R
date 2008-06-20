`csdvsmdd.fisher` <-
function(n.placebo, n.treat, prob.placebo, prob.treat, csd, alpha=.05,
        alternative="two.sided"){
    alternative <- substring(alternative,1,1)
    sig.mat <- matrix(0, nrow=n.treat+1, ncol=n.placebo+1)
    sig.inds <- mdd.fisher.exact(n.placebo,n.treat,prob.placebo,alpha,.8,alternative)$observed
    if(alternative != "g"){
        tmp <- sig.inds[!is.na(sig.inds[,2]),1:2]
        for(i in 1:dim(tmp)[1]){
            sig.mat[1:(tmp[i,2]+1),tmp[i,1]+1] <- 1
        }
    } 
    if(alternative != "l"){
        col.num <- ifelse(alternative=="t",3,2)
        tmp <- sig.inds[!is.na(sig.inds[,col.num]),c(1,col.num)]
        for(i in 1:dim(tmp)[1]){
            sig.mat[(tmp[i,2]:n.treat)+1,tmp[i,1]+1] <- 1
        }
    }
    
    csd.mat <- outer((0:n.treat)/n.treat, (0:n.placebo)/n.placebo, 
        switch(alternative,
            t = function(x,y){abs(x-y) >= csd},
            g = function(x,y){x-y >= csd},
            l = function(x,y){y-x >= csd}
        ))
    
    prob.mat <- dbinom(0:n.treat, n.treat, prob.treat) %o% dbinom(0:n.placebo, 
        n.placebo, prob.placebo)

    symm.diff <- sig.mat - csd.mat
    
    list(statsig.not.clinsig = sum((abs(symm.diff)+symm.diff) * prob.mat/2), 
        clinsig.not.statsig = sum((abs(symm.diff)-symm.diff) * prob.mat/2),
        power = sum(sig.mat*prob.mat))
}

