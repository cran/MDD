`.all.subs` <-
function(n){
    lapply(1:(2^n-1), function(x){
            tmp <- c(x)
            while(length(tmp)<n){
                tmp <- c(tmp, tmp[length(tmp)] %/% 2)
                tmp[length(tmp)-1] <- tmp[length(tmp)-1] %% 2
            }
            tmp <- as.vector(tmp %*% diag(1:n))
            tmp[tmp>0]
        })
}

`.compress.list` <-
function(ends.in, gr = FALSE){
    if(identical(class(ends.in),"list")){
        sw <- TRUE
        ends <- do.call(rbind,ends.in)
    } else {
        sw <- FALSE
        ends <- ends.in
    }
    ends <- unique(as.matrix(ends))
    if(all(dim(ends))){
        if(gr){
            ends <- -ends
        }
        ends <- by(ends,apply(ends,1,sum),as.matrix)
        while(length(ends)>1){
            inds <- sapply(1:dim(ends[[1]])[1], function(x){
                all(apply(t(ends[[2]])-ends[[1]][x,]<0, 2, any))})
            ends[[2]] <- rbind(ends[[1]][inds,],ends[[2]])
            ends[[1]] <- NULL
        }
        ends <- data.frame(do.call(rbind,ends))
        if(gr){
            ends <- -ends
        }
        ends <- as.matrix(ends[do.call(order,ends),])
        dimnames(ends) <- NULL
    }
    if(sw){
        ends <- lapply(1:dim(ends)[1],function(x){ends[x,]})
    }
    ends
}

`.explanation.mantelhaen` <-
function(placebo.size, treat.size, placebo.vals, ret.inds){
    require(partitions)
    n <- length(placebo.size)
    cat(paste("\n   Suppose you have control strata of size ", 
        .meld.strata(placebo.size), ifelse(n<=2,",",";"), 
        " corresponding\ncontrol responses of ", .meld.strata(placebo.vals),  
        " (overall response rate: ", 
        round(100*sum(placebo.vals)/sum(placebo.size),1), 
        "%)", ifelse(n<=2,",",";"), " and\ncorresponding treatment strata of size ",
        .meld.strata(treat.size), ".\n", sep=""))  
    if(all(dim(ret.inds$gr))){
        guar <- t(blockparts(treat.size, max(apply(ret.inds$gr,1,sum))))
        dimnames(guar) <- NULL
        guar <- unlist(apply(.compress.list(guar, gr=TRUE),1,list), recursive=FALSE)
        while(!identical(ret.inds$gr, .compress.list(do.call(rbind,c(list(ret.inds$gr), guar)), 
                gr=TRUE))){
            guar <- .one.more(guar,treat.size)
        }
        guar <- sum(guar[[1]])
        cat(paste("\n   If there are ", guar, ifelse(guar != sum(treat.size), " or more", ""),
            " responses in all treatment strata combined\n(overall response rate: ", 
            round(100*guar/sum(treat.size),1), ifelse(round(round(100*guar/sum(treat.size),1)) ==
            round(100*guar/sum(treat.size),1) && round(100*guar/sum(treat.size),1)<100, ".0", ""), 
            "%), the test will be significant.", sep=""))
        if(guar != min(apply(ret.inds$gr,1,sum))){
            cat(paste("  There are\nscenarios under which the test will be significant with as few as ", 
                min(apply(ret.inds$gr,1,sum)), "\ntotal response", 
                ifelse(min(apply(ret.inds$gr,1,sum)) != 1, "s", ""), 
                " in the treatment strata (overall response rate: ", 
                round(100*min(apply(ret.inds$gr,1,sum))/sum(treat.size),1), 
                ifelse(round(round(100*min(apply(ret.inds$gr,1,sum))/sum(treat.size),1)) ==
                round(100*min(apply(ret.inds$gr,1,sum))/sum(treat.size),1), ".0",""),
                "%).\n", sep=""))
        } else {
            cat("\n")
        }
    }
    if(all(dim(ret.inds$le))){
        guar <- t(blockparts(treat.size, min(apply(ret.inds$le,1,sum))))
        dimnames(guar) <- NULL
        guar <- unlist(apply(.compress.list(guar),1,list), recursive=FALSE)
        while(!identical(ret.inds$le, .compress.list(do.call(rbind,c(list(ret.inds$le), guar))))){
            guar <- .one.less(guar)
        }
        guar <- sum(guar[[1]])
        cat(paste("\n   If there are ", guar, ifelse(guar != 0, " or fewer", ""),
            " responses in all treatment strata combined\n(overall response rate: ", 
            round(100*guar/sum(treat.size),1), ifelse(round(round(100*guar/sum(treat.size),1)) ==
            round(100*guar/sum(treat.size),1) && round(100*guar/sum(treat.size),1) > 0, ".0", ""), 
            "%), the test will be significant.", sep=""))
        if(guar != max(apply(ret.inds$le,1,sum))){
            cat(paste("  There are\nscenarios under which the test will be significant with as many as ", 
                max(apply(ret.inds$le,1,sum)), "\ntotal response", 
                ifelse(max(apply(ret.inds$le,1,sum)) != 1, "s", ""), 
                " in the treatment strata (overall response rate: ", 
                round(100*max(apply(ret.inds$le,1,sum))/sum(treat.size),1), 
                ifelse(round(round(100*max(apply(ret.inds$le,1,sum))/sum(treat.size),1)) ==
                round(100*max(apply(ret.inds$le,1,sum))/sum(treat.size),1), ".0", ""), 
                "%).\n", sep=""))
        } else {
            cat("\n")
        }
    }
    if(!all(dim(ret.inds$le)) && !all(dim(ret.inds$gr))){
        cat("\n   There is no set of responses that will be significant given this data.\n")
    }
    cat("\n")
}

`.mantelhaen` <-
function(placebo.size, treat.size, alpha=.05, alternative="two.sided", exact=TRUE){
    require(partitions)
    alternative <- substr(alternative,1,1)
    maxes <- as.vector(rbind(treat.size,placebo.size))
    ret <- list(treat=list(), plac=list())
    todo <- list(treat = list(treat.size), plac = list(list(placebo.size)))
    if(alternative=="l"){
        sw <- TRUE
        alternative <- "g"
    } else {
        sw <- FALSE
    }
    while(length(todo$treat)){
if(todo$treat[[1]][1] %in% c(0) & todo$treat[[1]][2] %in% c(0) &&
    todo$treat[[1]][3] %in% c(treat.size[3])){browser()}
        loop <- TRUE
        while(loop && !identical(todo$plac[[1]],list())){
            tmp <- sapply(todo$plac[[1]], function(x){
                .mhstat(as.vector(rbind(todo$treat[[1]],x)), maxes,
                alternative=alternative,exact=exact)<=alpha})
            if(!all(tmp)){
                tmp.plac <- list()
                for(i in which(tmp)){
                    tmp.plac <- c(tmp.plac, list(todo$plac[[c(1,i)]]))
                }
                todo$plac[[1]] <- c(tmp.plac,setdiff(.one.less(todo$plac[[1]]),
                    .one.less(tmp.plac)))
            } else { 
                loop <- FALSE
                plac.ret <- list()
                for(i in todo$plac[[1]]){
                    tmp.plac.ret <- as.matrix(blockparts(i))
                    plac.ret <- c(plac.ret, lapply(1:dim(tmp.plac.ret)[2], 
                        function(x){tmp.plac.ret[,x]}))
                    rm(tmp.plac.ret)
                }
                ret$treat <- c(ret$treat,list(todo$treat[[1]]))
                ret$plac <- c(ret$plac, list(do.call(rbind,unique(plac.ret))))
                todo.treat.next <- .one.less(todo$treat[[1]])
                todo.plac.next <- lapply(1:length(todo.treat.next),function(x){todo$plac[[1]]})
                i <- 1
                while(i<=length(todo.treat.next)){
                    treat.loc <- sapply(todo$treat,function(x){identical(x,todo.treat.next[[i]])})
                    if(any(treat.loc)){
                        todo$plac[[which(treat.loc)]] <- unique(c(todo.plac.next[[i]],
                            todo$plac[[which(treat.loc)]]))
                        todo.treat.next[[i]] <- c()
                        todo.plac.next[[i]] <- c()
                        i <- i-1
                    }
                    i <- i+1
                }
                todo$treat <- c(todo$treat, todo.treat.next)
                todo$plac <- c(todo$plac, todo.plac.next)
            } 
        }
        todo$treat[[1]] <- c()
        todo$plac[[1]] <- c()
    }
    for(i in 1:length(ret$treat)){
        ret$treat[[i]] <- matrix(rep(ret$treat[[i]], dim(ret$plac[[i]])[1]), 
            nrow=dim(ret$plac[[i]])[1], byrow=TRUE)
        ret$ret[[i]] <- cbind(ret$treat[[i]],ret$plac[[i]])
        ret$ret[[i]] <- ret$ret[[i]][,as.vector(outer(c(0,length(placebo.size)), 
            1:length(placebo.size), "+"))]
    }
    ret <- do.call(rbind,ret$ret)
    if(sw){
        ret <- t(maxes - t(ret))
    }
    if(alternative=="t"){
        ret <- rbind(ret,t(maxes - t(ret)))
    }
    res <- array(0,dim=maxes+1)
    res[ret+1] <- 1
    res
}

`.maxa` <-
function(x){
    tmp <- which(diff(x)==1)
    ifelse(length(tmp),tmp,NA)
}

`.meld.strata` <-
function(nums){
    if(length(nums)<=2){
        paste(nums,collapse=" and ")
    } else {
        paste(paste(nums[-length(nums)],collapse=", "), ", and ", nums[length(nums)], sep="")
    }
}

`.mhstat` <-
function(nums,maxes,alternative="two.sided",exact=TRUE){
    alternative <- substr(alternative,1,1)
    if(any(nums>maxes)){return(1)}
    ret <- array(rbind(nums,maxes-nums),dim=c(2,2,length(nums)/2))
    ifelse(length(nums)==2, fisher.test(ret[,,1],alternative=alternative)$p.val,
        mantelhaen.test(ret,exact=exact,alternative=alternative)$p.val)
}

`.mina` <-
function(x){
    tmp <- which(diff(x) == -1)
    ifelse(length(tmp),tmp,NA)
}

`.one.less` <-
function(vals, min.val=0){
    if(identical(vals,list())){
        return(list())
    }
    if(class(vals)=="list"){
        return(unique(do.call(c,lapply(vals,function(x){.one.less(x,min.val=min.val)}))))
    }
    do.call(c,apply(matrix(rep(vals,length(vals)),ncol=length(vals),byrow=TRUE) - diag(length(vals)),
        1, function(x){if(all(x>=min.val)){list(c(x))} else {list()}}))
}

`.one.more` <-
function(vals, max.val){
    if(identical(vals,list())){
        return(list())
    }
    if(class(vals)=="list"){
        return(unique(do.call(c,lapply(vals,function(x){.one.more(x,max.val=max.val)}))))
    }
    do.call(c,apply(matrix(rep(vals,length(vals)),ncol=length(vals),byrow=TRUE) + diag(length(vals)),
        1, function(x){if(all(x<=max.val)){list(c(x))} else {list()}}))
}

`.validate.entries` <-
function(fcn,var.list,var.types,var.descrips){
    err.mess <- function(v,tt,d){
        ret <- ""
        vnew <- gsub("[:space:]","",v)
        if(tt %in% c("pos.int.list", "prop.list")){
            vnew <- strsplit(v,",")[[1]]
        }
        valid <- switch(tt,
            pos.int = length(grep("^[1-9][0-9]*$",vnew)),
            pos.real = length(grep("^[0-9]*\\.?[0-9]*$",vnew)) && as.numeric(vnew)>0,
            arb.real = length(grep("^-?[0-9]*\\.?[0-9]*$",vnew)),
            prop = length(grep("^[0-9]*\\.?[0-9]*$",vnew)) && as.numeric(vnew)>0 && 
                as.numeric(vnew)<1,
            pos.int.list = (length(grep("^[1-9][0-9]*$",vnew)) == length(vnew)) && 
                length(vnew),
            prop.list = length(grep("^[0-9]*\\.?[0-9]*$",vnew)) == length(vnew) && 
                length(vnew) && min(as.numeric(vnew)) > 0 && max(as.numeric(vnew)) < 1 
        )
        if(!valid){
            ret <- switch(tt,
                pos.int = paste(d, "must be a positive integer.\n"),
                pos.real = paste(d, "must be a positive real number.\n"),
                arb.real = paste(d, "must be a real number.\n"),
                prop = paste(d, "must be a real number strictly between 0 and 1.\n"),
                pos.int.list = paste(d, "must be a list of positive integers, separated by commas.\n"),
                prop.list = paste(d, "must be either a real number strictly\nbetween 0 and 1, or a list of such numbers separated by commas.\n")
            )
        }
        ret  
    }

    ret <- "val"
    mess <- paste(sapply(which(var.types!=""), function(i){err.mess(var.list[i], 
        var.types[i], var.descrips[i])}), collapse="")
    if(fcn %in% c("mdd.mantelhaen.pow")){
        tmp <- var.list[3]
        if(length(strsplit(var.list[3],",")[[1]]) == 1){
            var.list[3] <- paste(rep(",", length(strsplit(var.list[1],",")[[1]])), collapse="")
        }    
    }
    if(fcn %in% c("mdd.mantelhaen","mdd.mantelhaen.pow")){
        if(length(strsplit(var.list[1],",")[[1]]) != length(strsplit(var.list[2],",")[[1]]) ||
                length(strsplit(var.list[1],",")[[1]]) != length(strsplit(var.list[3],",")[[1]])){
            mess <- paste(mess, paste("Incompatible number of strata: ", 
            length(strsplit(var.list[1],",")), ", ", length(strsplit(var.list[2],",")),
            ", and ",length(strsplit(var.list[3],",")), ".\n", sep=""))    
        }
    }
    if(fcn %in% c("mdd.mantelhaen.pow")){
        var.list[3] <- tmp
    }
    if(mess == ""){
        ret <- "sub"
    } else {
        err.alert <- tktoplevel()
        tkwm.title(err.alert, "Invalid data entered!")
        tkpack(tklabel(err.alert, text=mess, justify="left"), anchor="n")
        qbut <- tkbutton(err.alert, text = " OK ", command=function()tkdestroy(err.alert))
        tkpack(qbut,anchor="n")
    }
    ret
}

