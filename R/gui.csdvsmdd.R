`gui.csdvsmdd` <-
function(){
    require(tcltk) || stop("TCL/TK support is absent")
    fcn.window <- function(fcn){
        fcn <- as.character(tclObj(fcn))
        submitted <- tclVar("neutral")
        returned <- tclVar(FALSE)
        
        if(fcn != ""){
            convert.tcllist <- function(str){
                as.numeric(strsplit(tclvalue(str),",")[[1]])
            }
        
            submit.data <- function(fcn,var.list,var.fcns){
                inputs <- lapply(1:length(var.list), function(i){
                    var.fcns[[i]](eval(parse(text = paste("tclObj(", 
                    var.list[i], ")", sep=""))))})
                names(inputs) <- var.list
                do.call(fcn,inputs)
            }
            
            user.destroy <- tclVar(FALSE)
            alpha <- tclVar(.05)
            alternative <- tclVar("two.sided")
            alt.labs <- c("Not equal", "Treatment mean less than control mean", 
                "Treatment mean greater than control mean")
            alt.vals <- c("two.sided", "less", "greater")
            n.placebo <- tclVar("")
            n.treat <- tclVar("")
            csd <- tclVar("")
            ate <- tclVar("")
            n1 <- tclVar("")
            n2 <- tclVar("")
            prob.placebo <- tclVar("")
            prob.treat <- tclVar("")
            prev1 <- tclVar()
            prev2 <- tclVar()
            prev3 <- tclVar()
        
            fcn.menu <- tktoplevel()
            help.frame <- tkframe(fcn.menu, borderwidth=2, relief="groove")
            help.button <- tkbutton(help.frame, text=" Help ", 
                command=function(){tclvalue(submitted) <- "help"})
            tkpack(help.button)

            sig.frame <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            alpha.lab <- tklabel(sig.frame, text="Significance level")
            alpha.entry <- tkentry(sig.frame, width=5, textvariable=alpha)
            alt.lab <- tklabel(sig.frame, text="Alternative hypothesis")
            tkgrid(alpha.lab,alpha.entry,alt.lab)
            tkgrid.configure(alpha.lab,sticky="e")
            tkgrid.configure(alpha.entry,alt.lab,sticky="w")
            null.lab <- tklabel(sig.frame, text="")

            csd.lab <- tklabel(sig.frame, text="Clinically Significant Difference")
            csd.entry <- tkentry(sig.frame, width=5, textvariable=csd)
            if(fcn %in% c("csdvsmdd.t", "csdvsmdd.satter")){
                ate.lab <- tklabel(sig.frame, text="Anticipated Treatment Effect")
                ate.entry <- tkentry(sig.frame, width=5, textvariable=ate)
            } else {
                ate.lab <- null.lab
                ate.entry <- null.lab
            }
            alt.1 <- tkradiobutton(sig.frame, text=alt.labs[1], value=alt.vals[1], 
                variable=alternative)
            if(fcn %in% c("csdvsmdd.chisq")){
                alt.2 <- null.lab
                alt.3 <- null.lab
            } else {
                alt.2 <- tkradiobutton(sig.frame, text=alt.labs[2], value=alt.vals[2], 
                    variable=alternative)
                alt.3 <- tkradiobutton(sig.frame, text=alt.labs[3], value=alt.vals[3], 
                    variable=alternative)
            }
            tkgrid(null.lab, null.lab, alt.1)            
            tkgrid.configure(alt.1,sticky="w")
            tkgrid(csd.lab, csd.entry, alt.2)
            tkgrid.configure(csd.lab, sticky="e")
            tkgrid.configure(csd.entry, alt.2, sticky="w")
            tkgrid(ate.lab, ate.entry, alt.3)
            tkgrid.configure(ate.lab, sticky="e")
            tkgrid.configure(ate.entry, alt.3, sticky="w")
                        
            frame.buttons <- tkframe(fcn.menu, borderwidth=2)
      
            frameq <- tkframe(frame.buttons, borderwidth=2, relief="groove")
            frames <- tkframe(frame.buttons, borderwidth=2, relief="groove")
    
            qb <- tkbutton(frameq, text=" Close ", command = function(){
                tclvalue(user.destroy) <- "TRUE"
                tkdestroy(fcn.menu)
            })
            tkpack(qb)
            framer <- tkframe(frame.buttons, borderwidth=2, relief="groove")
            ret.but <- tkbutton(framer, text = " Return ", 
                command = function(){
                    tclvalue(submitted) <- "val"
                    tclvalue(returned) <- TRUE
                })
            tkpack(ret.but)

            submit <- tkbutton(frames, text=" Test ", command = 
                function(){tclvalue(submitted) <- "val"})
            tkpack(submit)
            tkpack(frames, framer, frameq, side="left")

            preview <- tkframe(fcn.menu, borderwidth=2,relief="groove")
            ci.low.lab <- tklabel(preview, text="Prob. of clin. sig.,\nnot stat. sig.")
            mdd.lab <- tklabel(preview, text="Prob. of stat. sig.,\nnot clin. sig.")
            ci.up.lab <- tklabel(preview, text="Power of test")
            mdd.prev <- tkentry(preview, textvariable=prev1)
            ci.low.prev <- tkentry(preview, textvariable=prev2)
            ci.up.prev <- tkentry(preview, textvariable=prev3)
                
            tkgrid(mdd.lab,ci.low.lab,ci.up.lab, sticky="s")
            tkgrid(mdd.prev,ci.low.prev,ci.up.prev)            
        }

        if(fcn == "csdvsmdd.chisq"){
            tkwm.title(fcn.menu, "CSD vs. MDD for chi-squared test")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            frame2 <- tkframe(fcn.menu, relief="groove", borderwidth=2)

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Number")
            rate.lab <- tklabel(frame1, text="Expected\nresponse rate")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=5, textvariable=n.placebo)
            n.t.entry <- tkentry(frame1, width=5, textvariable=n.treat)
            prob.c.entry <- tkentry(frame1, width=8, textvariable=prob.placebo)
            prob.t.entry <- tkentry(frame1, width=8, textvariable=prob.treat)
            
            tkgrid(null.lab,n.lab,null.lab,rate.lab)
            tkgrid(c.lab, n.c.entry, null.lab, prob.c.entry)
            tkgrid(t.lab, n.t.entry, null.lab, prob.t.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")
            tkgrid.configure(n.lab,rate.lab,sticky="s")
            
            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame2, frame.buttons)
            tkgrid(preview, sticky="w", columnspan=2)
            tkgrid.configure(frame2, sticky="w") 
            tkgrid.configure(frame.buttons, sticky="e") 
            tkconfigure(mdd.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.low.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.up.prev, state="readonly", readonlybackground="white")
            
            var.list <- c("n.placebo", "n.treat", "prob.placebo", "prob.treat", "csd", 
                "alpha")
            var.fcns <- rep(c(as.numeric),6)
            var.types <- c("pos.int","pos.int","prop","prop","prop","prop")
            var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
                "Expected control response rate", "Expected treatment response rate", 
                "Clinically significant difference", "Significance level")
         }
        
        if(fcn == "csdvsmdd.fisher"){
            tkwm.title(fcn.menu, "CSD vs. MDD for Fisher's exact test")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            frame2 <- tkframe(fcn.menu, relief="groove", borderwidth=2)

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Number")
            rate.lab <- tklabel(frame1, text="Expected\nresponse rate")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=5, textvariable=n.placebo)
            n.t.entry <- tkentry(frame1, width=5, textvariable=n.treat)
            prob.c.entry <- tkentry(frame1, width=8, textvariable=prob.placebo)
            prob.t.entry <- tkentry(frame1, width=8, textvariable=prob.treat)
            
            tkgrid(null.lab,n.lab,null.lab,rate.lab)
            tkgrid(c.lab, n.c.entry, null.lab, prob.c.entry)
            tkgrid(t.lab, n.t.entry, null.lab, prob.t.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")
            tkgrid.configure(n.lab,rate.lab,sticky="s")
            
            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame2, frame.buttons)
            tkgrid(preview, sticky="w", columnspan=2)
            tkgrid.configure(frame2, sticky="w") 
            tkgrid.configure(frame.buttons, sticky="e") 
            tkconfigure(mdd.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.low.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.up.prev, state="readonly", readonlybackground="white")
            
            var.list <- c("n.placebo", "n.treat", "prob.placebo", "prob.treat", "csd", 
                "alpha", "alternative")
            var.fcns <- rep(c(as.numeric,as.character),c(6,1))
            var.types <- c("pos.int","pos.int","prop","prop","prop","prop","")
            var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
                "Expected control response rate", "Expected treatment response rate", 
                "Clinically significant difference", "Significance level", 
                "Alternative hypothesis")
        }

        if(fcn == "csdvsmdd.satter"){
            sigma1 <- tclVar("")
            sigma2 <- tclVar("")
            tkwm.title(fcn.menu, "CSD vs. MDD for t-test (unequal variances)")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            frame2 <- tkframe(fcn.menu, relief="groove", borderwidth=2)

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Number")
            mu.lab <- tklabel(frame1, text="")
            sigma.lab <- tklabel(frame1, text="sigma")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=5, textvariable=n1)
            n.t.entry <- tkentry(frame1, width=5, textvariable=n2)
            mu.c.entry <- tklabel(frame1, text="")
            sigma1.entry <- tkentry(frame1, width=8, textvariable=sigma1)
            sigma2.entry <- tkentry(frame1, width=8, textvariable=sigma2)
            
            tkgrid(null.lab,n.lab,mu.lab,sigma.lab)
            tkgrid(c.lab, n.c.entry, mu.c.entry, sigma1.entry)
            tkgrid(t.lab, n.t.entry, null.lab, sigma2.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")

            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame2, frame.buttons)
            tkgrid(preview, sticky="w", columnspan=2)
            tkgrid.configure(frame2, sticky="w") 
            tkgrid.configure(frame.buttons, sticky="e") 
            tkconfigure(mdd.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.low.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.up.prev, state="readonly", readonlybackground="white")

            var.list <- c("n1","n2","sigma1","sigma2","ate","csd","alpha","alternative")
            var.fcns <- rep(c(as.numeric,as.character),c(7,1))
            var.types <- c("pos.int", "pos.int", "pos.real", "pos.real", "prop", "prop", "prop", "")
            var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
                "Control standard deviation", "Treatment standard deviation", 
                "Anticipated Treatment Effect", "Clinically Significant Difference", 
                "Significance level", "Alternative hypothesis")
        }
        
        if(fcn == "csdvsmdd.t"){
            sigma <- tclVar("")
            tkwm.title(fcn.menu, "CSD vs. MDD for t-test (equal variances)")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            frame2 <- tkframe(fcn.menu, relief="groove", borderwidth=2)

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Number")
            mu.lab <- tklabel(frame1, text="")
            sigma.lab <- tklabel(frame1, text="sigma")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=5, textvariable=n1)
            n.t.entry <- tkentry(frame1, width=5, textvariable=n2)
            mu.c.entry <- tklabel(frame1, text="")
            sigma.entry <- tkentry(frame1, width=8, textvariable=sigma)
            
            tkgrid(null.lab,n.lab,mu.lab,sigma.lab)
            tkgrid(c.lab, n.c.entry, mu.c.entry, sigma.entry)
            tkgrid(t.lab, n.t.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")
            tkgrid.configure(sigma.entry, rowspan=2)

            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame2, frame.buttons)
            tkgrid(preview, sticky="w", columnspan=2)
            tkgrid.configure(frame2, sticky="w") 
            tkgrid.configure(frame.buttons, sticky="e") 
            tkconfigure(mdd.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.low.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.up.prev, state="readonly", readonlybackground="white")

            var.list <- c("n1","n2","sigma","ate","csd","alpha","alternative")
            var.fcns <- rep(c(as.numeric,as.character),c(6,1))
            var.types <- c("pos.int", "pos.int", "pos.real", "prop", "prop", "prop", "")
            var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
                "Common standard deviation", "Anticipated Treatment Effect", 
                "Clinically Significant Difference", "Significance level", 
                "Alternative hypothesis")
        }

        tkbind(fcn.menu, "<Destroy>", function()tclvalue(submitted) <- "dest")
        while(tclvalue(submitted) != "dest" && !as.logical(tclObj(returned))){
            tkwait.variable(submitted)
            if(tclvalue(submitted) == "help"){
                browseURL(paste(R.home(),"/library/MDD/html/", fcn, ".html", sep=""), 
                    browser = getOption("browser"))
                tclvalue(submitted) <- "neutral"
            }
            if(tclvalue(submitted) == "val"){
                var.vals <- sapply(var.list, function(x){tclvalue(eval(parse(text=x)))})
                tclvalue(submitted) <- .validate.entries(fcn, var.vals, var.types, var.descrips)  
            }
            if(tclvalue(submitted) == "sub"){
                tmp <- submit.data(fcn,var.list,var.fcns)
                tclvalue(prev1) <- as.character(tmp[[1]])
                tclvalue(prev2) <- as.character(tmp[[2]])
                tclvalue(prev3) <- as.character(tmp[[3]])
                tclvalue(submitted) <- "neutral"
            }
        }
        if(tclvalue(submitted)=="dest" && tclvalue(user.destroy)=="0") stop("aborted")

        tkdestroy(fcn.menu)
        if(as.logical(tclObj(returned))){
            tmp
        }
    }

    ret <- tclVar("")
    submitted <- tclVar(0)
    fcn <- tclVar("csdvsmdd.t")
    user.destroy.m <- tclVar(FALSE)

    main.menu <- tktoplevel()
    tkwm.title(main.menu,"CSD vs. MDD GUI")
    
    frame1 <- tkframe(main.menu, relief="groove", borderwidth=2)
    
    tkpack(tklabel(frame1, text="Continuous, two-sample"), anchor="w")
    
    cont.labs <- c("t-test (equal variance)", "t-test (unequal variance)")
    cont.fcns <- c("csdvsmdd.t", "csdvsmdd.satter")
    
    for(i in 1:length(cont.labs)){
        tmp <- tkradiobutton(frame1, text=cont.labs[i], value=cont.fcns[i], variable=fcn)
        tkpack(tmp, anchor="w")
    }
    
    tkpack(tklabel(frame1, text=paste("Binomial response/proportions", paste(rep(" ", 30), 
        collapse="")), anchor="w"))
    
    binom.labs <- c("Chi-squared test", "Fisher's exact test")
    binom.fcns <- c("csdvsmdd.chisq", "csdvsmdd.fisher")
    
    for(i in 1:length(binom.labs)){
        tmp <- tkradiobutton(frame1, text=binom.labs[i], value=binom.fcns[i], variable=fcn)
        tkpack(tmp, anchor="w")
    }
    
    tkpack(frame1)
    
    frame2 <- tkframe(main.menu, borderwidth=2)
    
    frameq <- tkframe(frame2, borderwidth=2, relief="groove")
    frames <- tkframe(frame2, borderwidth=2, relief="groove")
    
    qb <- tkbutton(frameq, text=" Quit ", command = function(){
            tclvalue(user.destroy.m) <- "TRUE"
            tkdestroy(main.menu)        
        })
    tkpack(qb)
    submit <- tkbutton(frames, text=" OK ", command = function(){
            tclvalue(submitted) <- "1"
            tclvalue(user.destroy.m) <- "TRUE"
        })
    tkpack(submit)
    
    tkpack(frames, frameq, side="left")
    tkpack(frame2, anchor="e")
    
    tkbind(main.menu, "<Destroy>", function()tclvalue(submitted) <- 2)
    tkwait.variable(submitted)
    if(tclvalue(submitted)=="2" && tclvalue(user.destroy.m) == "0") stop("aborted")

    if(tclvalue(submitted) == "1"){
        tkdestroy(main.menu)
        fcn.window(fcn)
    }
}

