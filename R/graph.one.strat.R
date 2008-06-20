`graph.one.strat` <-
function(res.mat, weighted=FALSE, prob.placebo=NULL, prob.treat=NULL,
        show.grid=FALSE){
    if(class(res.mat)=="list"){
        work.mat <- res.mat$observed
    } else {
        work.mat <- res.mat
    }
    if(all(is.na(work.mat[,2]))){
        stop("No significant scenarios.")
    }
    ind <- which(!is.na(work.mat[,2]) & work.mat[,2]>0)[1]
    n.placebo <- dim(work.mat)[1] - 1
    n.treat <- round(work.mat[ind,2]/work.mat[ind,2+dim(work.mat)[2]/2])
    
    if(dim(work.mat)[2]==4){
        work.mat <- cbind(work.mat[,1:2],NA)
    } else {
        work.mat <- work.mat[,1:3]    
    }
    if(work.mat[ind,2]>work.mat[ind,1]){
        work.mat <- work.mat[,c(1,3,2)]
    }

    if(any(!is.na(work.mat[,3]))){    
        up.x <- as.vector(rbind(work.mat[!is.na(work.mat[,3]),1],
            work.mat[!is.na(work.mat[,3]),1]+1))
        up.y <- rep(work.mat[!is.na(work.mat[,3]),3], each=2)
        up.x <- c(up.x,up.x[length(up.x)],0,up.x[1])
        up.y <- c(up.y,rep(n.treat+1,2),up.y[1])
    } else {
        up.x <- NA
        up.y <- NA
    }
    
    if(any(!is.na(work.mat[,2]))){    
        low.x <- as.vector(rbind(work.mat[!is.na(work.mat[,2]),1],
            work.mat[!is.na(work.mat[,2]),1]+1))
        low.y <- rep(work.mat[!is.na(work.mat[,2]),2], each=2)+1
        low.x <- c(low.x[1],low.x,n.placebo+1,low.x[1])
        low.y <- c(0,low.y,0,0)
    } else {
        low.x <- NA
        low.y <- NA
    }
    
    if(weighted){
        (!is.null(prob.placebo) && !(is.null(prob.treat))) || 
            stop("With 'weighted = TRUE', 'prob.placebo' and 'prob.treat' must be specified.")
        binom.p <- c(0,pbinom(0:n.placebo, n.placebo, prob.placebo))
        up.x <- binom.p[up.x+1]
        low.x <- binom.p[low.x+1]
        binom.t <- c(0,pbinom(0:n.treat, n.treat, prob.treat))
        up.y <- binom.t[up.y+1]
        low.y <- binom.t[low.y+1]
        pow.1 <- 0
        pow.2 <- 0
        text.which <- 0

        plot(c(1,1),c(1,1), type="n", xlab=bquote(paste("Observed control responses for ",
            italic(p) == .(prob.placebo), sep="")), 
            ylab=bquote(paste("Observed treatment responses for  ",
            italic(p) == .(prob.treat), sep="")), xlim=c(0,1), ylim=c(0,1), axes=FALSE,
            main="Significant Scenarios, Weighted by Probability\n(Blue = significant)")
        for(i in 1:4){
            axis(i,at=c(-1,2))
        }
        
        cent.x <- binom.p[-1] - diff(binom.p)/2
        cent.y <- binom.t[-1] - diff(binom.t)/2
        x.loc <- sapply((1:5)/6, function(x){which.min(abs(cent.x-x))})
        y.loc <- sapply((1:5)/6, function(x){which.min(abs(cent.y-x))})
        
        x.t <- cent.x[c(1,x.loc,length(cent.x))]
        y.t <- cent.y[c(1,y.loc,length(cent.y))]
        x.lab <- c(0,x.loc-1,n.placebo)
        y.lab <- c(0,y.loc-1,n.treat)
        
        axis(1,at=x.t,labels=x.lab)
        axis(2,at=y.t,labels=y.lab)
        
        if(any(!is.na(work.mat[,3]))){    
            polygon(up.x, up.y, density=NA, col="blue")
            pow.1 <- abs(sum(up.x[-1]*up.y[-length(up.y)]) - 
                sum(up.x[-length(up.x)]*up.y[-1]))/2
        }
        if(any(!is.na(work.mat[,2]))){    
            polygon(low.x, low.y, density=NA, col="blue")
            pow.2 <- abs(sum(low.x[-1]*low.y[-length(low.y)]) - 
                sum(low.x[-length(low.x)]*low.y[-1]))/2
        }
        
        if(show.grid){
            abline(v=binom.p, lty=3)    
            abline(h=binom.t, lty=3)
        }

        pow <- round(100*(pow.1+pow.2),1)
        
        if(pow < 5){
            text(.5,.5, paste("Power = ", pow, ifelse(round(pow)==pow,".0",""), 
                "%", sep=""), font=2, col="black") 
        } else {
            if(pow.1 >= pow.2){
                text(.01,.99, paste("Power = ", pow, ifelse(round(pow)==pow,".0",""), 
                    "%", sep=""), font=2, adj=c(0,1), col="yellow") 
            } else {
                text(.99,.01, paste("Power = ", pow, ifelse(round(pow)==pow,".0",""), 
                    "%", sep=""), font=2, adj=c(1,0), col="yellow") 
            }
        }
    } else {
        low.x <- low.x-1/2
        low.y <- low.y-1/2
        up.x <- up.x-1/2
        up.y <- up.y-1/2
        plot(c(1,1),c(1,1), type="n", main="Significant Scenarios\n(Blue = significant)", 
            xlab="Observed control responses", ylab="Observed treatment responses", 
            xlim=c(-.5,n.placebo+.5), ylim=c(-.5,n.treat+.5))
        if(any(!is.na(work.mat[,3]))){    
            polygon(up.x, up.y, density=NA, col="blue")
        }
        if(any(!is.na(work.mat[,2]))){    
            polygon(low.x, low.y, density=NA, col="blue")
        }
        if(show.grid){
            abline(v=seq(-1/2,n.placebo+1/2), lty=3)    
            abline(h=seq(-1/2,n.treat+1/2), lty=3)
        }
    }
}

