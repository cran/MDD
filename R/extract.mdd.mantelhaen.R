`extract.mdd.mantelhaen` <-
function(placebo.vals, from.file=NULL, print.summary=TRUE, sig.inds){
    if(missing(sig.inds)){
        load(from.file)
    }
    if((n <- length(placebo.vals)) != length(dim(sig.inds))/2){
        stop("Incompatible number of control strata in input values.")
    }

    edges <- list()
    edges$gr <- apply(array(sapply(1:(2*n),function(i){slice.index(sig.inds,i)==ifelse(i %% 2,
        1, dim(sig.inds)[i])}),dim=c(dim(sig.inds),2*n)),
        1:(2*n),sum)
    edges$le <- apply(array(sapply(1:(2*n),function(i){slice.index(sig.inds,i)==ifelse(i %% 2,
        dim(sig.inds)[i],1)}),dim=c(dim(sig.inds),2*n)),
        1:(2*n),sum)
    
    tmp.inds <- list(le = sig.inds, gr = sig.inds)
    for(i in 1:(2*n)){
        d <- dim(sig.inds)[i]
        if(i %% 2){
            tmp.inds$gr[slice.index(sig.inds,i)!=1] <- tmp.inds$gr[slice.index(sig.inds,i)!=1] +
                sig.inds[slice.index(sig.inds,i)!=d]
            tmp.inds$le[slice.index(sig.inds,i)!=d] <- tmp.inds$le[slice.index(sig.inds,i)!=d] +
                sig.inds[slice.index(sig.inds,i)!=1]
        } else {
            tmp.inds$gr[slice.index(sig.inds,i)!=d] <- tmp.inds$gr[slice.index(sig.inds,i)!=d] +
                sig.inds[slice.index(sig.inds,i)!=1]
            tmp.inds$le[slice.index(sig.inds,i)!=1] <- tmp.inds$le[slice.index(sig.inds,i)!=1] +
                sig.inds[slice.index(sig.inds,i)!=d]
        }
    }
    ret.inds <- sapply(c("le","gr"), function(x){
        which(edges[[x]] + tmp.inds[[x]] < 2*n+1 & sig.inds == 1, arr.ind=TRUE)-1}, 
        simplify=FALSE, USE.NAMES=TRUE)
        
    for(i in c("le","gr")){
        for(j in 1:n){
            ret.inds[[i]] <- ret.inds[[i]][ret.inds[[i]][,2*j]==placebo.vals[j],]
        }
        ret.inds[[i]] <- ret.inds[[i]][,2*(1:n)-1]
    }
    if(any(duplicated(data.frame(do.call(rbind,ret.inds))))){
        warning("Duplicate entries in return value")
    }

    ret.inds$gr <- .compress.list(ret.inds$gr, gr=TRUE)
    ret.inds$le <- .compress.list(ret.inds$le)
    colnames(ret.inds$gr) <- NULL
    colnames(ret.inds$le) <- NULL

    if(print.summary){
        .explanation.mantelhaen((dim(sig.inds)-1)[2*(1:n)], (dim(sig.inds)-1)[2*(1:n)-1], 
            placebo.vals, ret.inds)
        invisible(ret.inds)
    } else {
        ret.inds
    }
}

