`gui.mdd` <-
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
            pow <- tclVar(.8)
            exact <- tclVar(TRUE)
            placebo.size <- tclVar("")
            placebo.vals <- tclVar("")
            treat.size <- tclVar("")
            mu1 <- tclVar(0)
            n1 <- tclVar("")
            n2 <- tclVar("")
            return.cis <- tclVar(FALSE)
            prob.placebo <- tclVar("")
            prev1 <- tclVar()
            prev2 <- tclVar()
            prev3 <- tclVar()
            print.summary <- tclVar(TRUE)
        
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

            if(fcn %in% c("mdd.chisq", "mdd.fisher.exact", "mdd.mantelhaen.pow")){
                pow.lab <- tklabel(sig.frame, text="Power")
                pow.entry <- tkentry(sig.frame, width=5, textvariable=pow)
            } else {
                pow.lab <- tklabel(sig.frame, text="")
                pow.entry <- tklabel(sig.frame, text="")
            }
            alt.1 <- tkradiobutton(sig.frame, text=alt.labs[1], value=alt.vals[1], 
                variable=alternative)
            tkgrid(pow.lab, pow.entry, alt.1)
            tkgrid.configure(pow.lab,sticky="e")
            tkgrid.configure(pow.entry,alt.1,sticky="w")

            if(!(fcn %in% c("mdd.chisq"))){
                if(fcn %in% c("mdd.mantelhaen", "mdd.mantelhaen.pow")){
                    exact.but <- tkcheckbutton(sig.frame, text="Use exact test", variable=exact)
                    tkgrid(exact.but, tkradiobutton(sig.frame, text=alt.labs[2], 
                        value=alt.vals[2], variable=alternative), sticky="w", columnspan=2)
                    if(fcn %in% c("mdd.mantelhaen")){
                        sum.but <- tkcheckbutton(sig.frame, text="Print summary of results", 
                            variable=print.summary)
                        tkgrid(sum.but, tkradiobutton(sig.frame, text=alt.labs[3], 
                            value=alt.vals[3], variable=alternative), sticky="w", 
                            columnspan=2)
                    } else {
                        tkgrid(tklabel(sig.frame, text=""), tkradiobutton(sig.frame, 
                            text=alt.labs[3], value=alt.vals[3], variable=alternative), 
                            sticky="w", columnspan=2)
                    }
                } else {
                    for(i in 2:length(alt.labs)){
                        tmp <- tkradiobutton(sig.frame, text=alt.labs[i], value=alt.vals[i], 
                            variable=alternative)
                        tkgrid(tklabel(sig.frame, text=""), tklabel(sig.frame, text=""), tmp, 
                            sticky="w")
                    }
                }
            }
            frame.buttons <- tkframe(fcn.menu, borderwidth=2)
      
            frameq <- tkframe(frame.buttons, borderwidth=2, relief="groove")
            frames <- tkframe(frame.buttons, borderwidth=2, relief="groove")
    
            qb <- tkbutton(frameq, text=" Close ", command = function(){
                tclvalue(user.destroy) <- "TRUE"
                tkdestroy(fcn.menu)
            })
            tkpack(qb)
            framer <- tkframe(frame.buttons, borderwidth=2, relief="groove")
            ret.but <- tkbutton(framer, text = ifelse(fcn %in% c("mdd.mantelhaen",
                "mdd.satterthwaite", "mdd.t.test"), " Return ", "  OK  "), 
                command = function(){
                    tclvalue(submitted) <- "val"
                    tclvalue(returned) <- TRUE
                })
            tkpack(ret.but)

            if(fcn %in% c("mdd.mantelhaen", "mdd.satterthwaite", "mdd.t.test")){            
                submit <- tkbutton(frames, text=" Test ", command = 
                    function(){tclvalue(submitted) <- "val"})
                tkpack(submit)
                tkpack(frames, framer, frameq, side="left")
             } else {
                tkpack(framer, frameq, side="left")    
            }
        }

        if(fcn == "mdd.chisq"){
            tkwm.title(fcn.menu, "MDD for chi-squared test")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            frame2 <- tkframe(fcn.menu, relief="groove", borderwidth=2)

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Number")
            rate.lab <- tklabel(frame1, text="Expected\nresponse rate")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=5, textvariable=placebo.size)
            n.t.entry <- tkentry(frame1, width=5, textvariable=treat.size)
            prob.c.entry <- tkentry(frame1, width=8, textvariable=prob.placebo)
            prob.t.entry <- tklabel(frame1, text="")
            
            tkgrid(null.lab,n.lab,null.lab,rate.lab)
            tkgrid(c.lab, n.c.entry, null.lab, prob.c.entry)
            tkgrid(t.lab, n.t.entry, null.lab, prob.t.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")
            tkgrid.configure(n.lab,rate.lab,sticky="s")
            
            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame.buttons, sticky="e", columnspan=2)
            
            var.list <- c("placebo.size", "treat.size", "prob.placebo", "alpha", "pow")
            var.fcns <- rep(c(as.numeric),5)
            var.types <- c("pos.int","pos.int","prop","prop","prop")
            var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
                "Expected control response rate", "Significance level", "Power")
        }
        
        if(fcn == "mdd.fisher.exact"){
            tkwm.title(fcn.menu, "MDD for Fisher's exact test")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Number")
            rate.lab <- tklabel(frame1, text="Expected\nresponse rate")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=5, textvariable=placebo.size)
            n.t.entry <- tkentry(frame1, width=5, textvariable=treat.size)
            prob.c.entry <- tkentry(frame1, width=8, textvariable=prob.placebo)
            prob.t.entry <- tklabel(frame1, text="")
            
            tkgrid(null.lab,n.lab,null.lab,rate.lab)
            tkgrid(c.lab, n.c.entry, null.lab, prob.c.entry)
            tkgrid(t.lab, n.t.entry, null.lab, prob.t.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")
            tkgrid.configure(n.lab,rate.lab,sticky="s")
            
            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame.buttons, sticky="e", columnspan=2)
            
            var.list <- c("placebo.size", "treat.size", "prob.placebo", "alpha", "pow", 
                "alternative")
            var.fcns <- rep(c(as.numeric,as.character),c(5,1))
            var.types <- c("pos.int","pos.int","prop","prop","prop","")
            var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
                "Expected control response rate", "Significance level", "Power", 
                "Alternative hypothesis")
        }

        if(fcn == "mdd.mantelhaen"){
            tkwm.title(fcn.menu, "MDD for Cochran-Mantel-Haenszel test")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Strata sizes,\nseparated by commas")
            resp.lab <- tklabel(frame1, text="Observed responses,\nseparated by commas")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=20, textvariable=placebo.size)
            n.t.entry <- tkentry(frame1, width=20, textvariable=treat.size)
            obs.c.entry <- tkentry(frame1, width=20, textvariable=placebo.vals)
            obs.t.entry <- tklabel(frame1, text="")
            
            tkgrid(null.lab,n.lab,null.lab,resp.lab)
            tkgrid(c.lab, n.c.entry, null.lab, obs.c.entry)
            tkgrid(t.lab, n.t.entry, null.lab, obs.t.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")
            tkgrid.configure(n.lab,resp.lab,sticky="s")
            
            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame.buttons, sticky="e", columnspan=2)
            
            var.list <- c("placebo.size", "treat.size", "placebo.vals", "alpha", 
                "alternative", "exact", "print.summary")
            var.fcns <- rep(c(convert.tcllist,as.numeric,as.character,as.logical),c(3,1,1,2))
            var.types <- c("pos.int.list","pos.int.list","pos.int.list","prop","","","")
            var.descrips <- c("Number of subjects in each control stratum", 
                "Number of subjects in each treatment stratum", 
                "Number of observed responses in each control stratum", 
                "Significance level", "Alternative hypothesis", "Whether to use an exact test",
                "Whether to print a summary")
        }
        
        if(fcn == "mdd.mantelhaen.pow"){
            to.file <- tclVar("") 
            from.file <- tclVar("")

            tkwm.title(fcn.menu, "Power for Cochran-Mantel-Haenszel test")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            frame2 <- tkframe(fcn.menu, relief="groove", borderwidth=2)

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Strata sizes,\nseparated by commas")
            resp.lab <- tklabel(frame1, text="Expected response rates,\nseparated by commas")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=20, textvariable=placebo.size)
            n.t.entry <- tkentry(frame1, width=20, textvariable=treat.size)
            obs.c.entry <- tkentry(frame1, width=25, textvariable=prob.placebo)
            obs.t.entry <- tklabel(frame1, text="")
            
            tkgrid(null.lab,n.lab,null.lab,resp.lab)
            tkgrid(c.lab, n.c.entry, null.lab, obs.c.entry)
            tkgrid(t.lab, n.t.entry, null.lab, obs.t.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")
            tkgrid.configure(n.lab,resp.lab,sticky="s")
            
            file.frame <- tkframe(fcn.menu, borderwidth=2, relief="groove")
            from.file.label <- tklabel(file.frame, text="Load from:")
            to.file.label <- tklabel(file.frame, text="Save to:")
            from.file.entry <- tkentry(file.frame, width=50, textvariable=from.file)
            to.file.entry <- tkentry(file.frame, width=50, textvariable=to.file)
            from.file.browse <- tkbutton(file.frame, text=" Browse ", 
                command=function(){tclvalue(from.file) <- tkgetOpenFile()})
            to.file.browse <- tkbutton(file.frame, text=" Browse ", 
                command=function(){tclvalue(to.file) <- tkgetSaveFile()})
                
            tkgrid(tklabel(file.frame, justify="left",
                text="   (Optional) File(s) to use for storing/retrieving array of significant scenarios"),
                columnspan=2)
            tkgrid(from.file.label, from.file.entry, from.file.browse)
            tkgrid(to.file.label, to.file.entry, to.file.browse)
            tkgrid.configure(from.file.label,to.file.label,sticky="e")
            tkgrid.configure(from.file.entry,to.file.entry,from.file.browse,to.file.browse,
                sticky="w")
            
            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(file.frame, columnspan=2)
            tkgrid(frame.buttons, columnspan=2)
            tkgrid.configure(frame.buttons, sticky="e") 
            
            var.list <- c("placebo.size", "treat.size", "prob.placebo", "alpha", 
                "pow", "alternative", "exact", "to.file", "from.file")
            var.fcns <- rep(c(convert.tcllist, as.numeric, as.character, as.logical,
                as.character), c(3,2,1,1,2))
            var.types <- c("pos.int.list","pos.int.list","prop.list","prop","prop",rep("",4))
            var.descrips <- c("Number of subjects in each control stratum", 
                "Number of subjects in each treatment stratum", 
                "Expected response rates in each control stratum", "Significance level", 
                "Power", "Alternative hypothesis", "Whether to use an exact test",
                "Output file", "Input file")
        }

        if(fcn == "mdd.satterthwaite"){
            sigma1 <- tclVar("")
            sigma2 <- tclVar("")
            tkwm.title(fcn.menu, "MDD for t-test (unequal variances)")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            frame2 <- tkframe(fcn.menu, relief="groove", borderwidth=2)

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Number")
            mu.lab <- tklabel(frame1, text="mu")
            sigma.lab <- tklabel(frame1, text="sigma")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=5, textvariable=n1)
            n.t.entry <- tkentry(frame1, width=5, textvariable=n2)
            mu.c.entry <- tkentry(frame1, width=5, textvariable=mu1)
            sigma1.entry <- tkentry(frame1, width=8, textvariable=sigma1)
            sigma2.entry <- tkentry(frame1, width=8, textvariable=sigma2)
            
            tkgrid(null.lab,n.lab,mu.lab,sigma.lab)
            tkgrid(c.lab, n.c.entry, mu.c.entry, sigma1.entry)
            tkgrid(t.lab, n.t.entry, null.lab, sigma2.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")

            ret.1 <- tkradiobutton(frame2, text="Return MDD", value=0, variable=return.cis)
            ret.2 <- tkradiobutton(frame2, text="Return confidence interval", value=1, 
                variable=return.cis)
            tkgrid(ret.1,ret.2)
            tkgrid.configure(ret.1, sticky="w")
            tkgrid.configure(ret.2, sticky="e")

            preview <- tkframe(fcn.menu, borderwidth=2,relief="groove")
            mdd.lab <- tklabel(preview, text="MDD")
            ci.low.lab <- tklabel(preview, text="Lower Confidence Bound")
            ci.up.lab <- tklabel(preview, text="Upper Confidence Bound")
            mdd.prev <- tkentry(preview, textvariable=prev1)
            ci.low.prev <- tkentry(preview, textvariable=prev2)
            ci.up.prev <- tkentry(preview, textvariable=prev3)
                
            tkgrid(mdd.lab,ci.low.lab,ci.up.lab)
            tkgrid(mdd.prev,ci.low.prev,ci.up.prev)

            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame2, frame.buttons)
            tkgrid(preview, sticky="w", columnspan=2)
            tkgrid.configure(frame2, sticky="w") 
            tkgrid.configure(frame.buttons, sticky="e") 
            tkconfigure(mdd.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.low.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.up.prev, state="readonly", readonlybackground="white")
            
            var.list <- c("n1","n2","mu1","sigma1","sigma2","alpha","alternative","return.cis")
            var.fcns <- rep(c(as.numeric,as.character,as.logical),c(6,1,1))
            var.types <- c("pos.int","pos.int","arb.real","pos.real","pos.real","prop","","")
            var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
                "Mean for control group", "Control standard deviation", 
                "Treatment standard deviation", "Significance level", "Alternative hypothesis", 
                "Return CI")
        }
        
        if(fcn == "mdd.t.test"){
            sigma <- tclVar("")
            tkwm.title(fcn.menu, "MDD for t-test (equal variances)")
            frame1 <- tkframe(fcn.menu, relief="groove", borderwidth=2)             
            frame2 <- tkframe(fcn.menu, relief="groove", borderwidth=2)

            null.lab <- tklabel(frame1, text="")           
            n.lab <- tklabel(frame1, text="Number")
            mu.lab <- tklabel(frame1, text="mu")
            sigma.lab <- tklabel(frame1, text="sigma")
            c.lab <- tklabel(frame1, text="Control")
            t.lab <- tklabel(frame1, text="Treatment")
            
            n.c.entry <- tkentry(frame1, width=5, textvariable=n1)
            n.t.entry <- tkentry(frame1, width=5, textvariable=n2)
            mu.c.entry <- tkentry(frame1, width=5, textvariable=mu1)
            sigma.entry <- tkentry(frame1, width=8, textvariable=sigma)
            
            tkgrid(null.lab,n.lab,mu.lab,sigma.lab)
            tkgrid(c.lab, n.c.entry, mu.c.entry, sigma.entry)
            tkgrid(t.lab, n.t.entry)
            tkgrid.configure(c.lab,t.lab,sticky="e")
            tkgrid.configure(sigma.entry, rowspan=2)

            ret.1 <- tkradiobutton(frame2, text="Return MDD", value=0, variable=return.cis)
            ret.2 <- tkradiobutton(frame2, text="Return confidence interval", value=1, 
                variable=return.cis)
            tkgrid(ret.1,ret.2)
            tkgrid.configure(ret.1, sticky="w")
            tkgrid.configure(ret.2, sticky="e")

            preview <- tkframe(fcn.menu, borderwidth=2,relief="groove")
            mdd.lab <- tklabel(preview, text="MDD")
            ci.low.lab <- tklabel(preview, text="Lower Confidence Bound")
            ci.up.lab <- tklabel(preview, text="Upper Confidence Bound")
            mdd.prev <- tkentry(preview, textvariable=prev1)
            ci.low.prev <- tkentry(preview, textvariable=prev2)
            ci.up.prev <- tkentry(preview, textvariable=prev3)
                
            tkgrid(mdd.lab,ci.low.lab,ci.up.lab)
            tkgrid(mdd.prev,ci.low.prev,ci.up.prev)

            tkgrid(frame1, help.frame, sticky="w")
            tkgrid(sig.frame, sticky="ew", columnspan=2)
            tkgrid(frame2, frame.buttons)
            tkgrid(preview, sticky="w", columnspan=2)
            tkgrid.configure(frame2, sticky="w") 
            tkgrid.configure(frame.buttons, sticky="e") 
            tkconfigure(mdd.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.low.prev, state="readonly", readonlybackground="white")
            tkconfigure(ci.up.prev, state="readonly", readonlybackground="white")
            
            var.list <- c("n1","n2","mu1","sigma","alpha","alternative","return.cis")
            var.fcns <- rep(c(as.numeric,as.character,as.logical),c(5,1,1))
            var.types <- c("pos.int","pos.int","arb.real","pos.real","prop","","")
            var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
                "Mean for control group", "Common standard deviation", "Significance level",
                "Alternative hypothesis", "Return CI")
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
                if(fcn %in% c("mdd.mantelhaen") && as.logical(tclObj(print.summary))
                        && !as.logical(tclObj(returned))){
                    tmp.file <- tempfile()
                    sink(tmp.file)
                    tmp <- submit.data(fcn,var.list,var.fcns)
                    sink()
                    read.mess <- paste(readLines(tmp.file),collapse="\n")
                    file.remove(tmp.file)
                    mantelhaen.mess <- tktoplevel()
                    tkwm.title(mantelhaen.mess, "Summary of results")
                    tkpack(tklabel(mantelhaen.mess, text=read.mess, justify="left"), 
                        anchor="w")
                } else {
                    tmp <- submit.data(fcn,var.list,var.fcns)
                }
                if(fcn %in% c("mdd.satterthwaite", "mdd.t.test")){
                    if(length(tmp) == 1){
                        tclvalue(prev2) <- ifelse(tclvalue(alternative) == "less", "-Inf", 
                            as.numeric(tclvalue(mu1))-tmp)
                        tclvalue(prev3) <- ifelse(tclvalue(alternative) == "greater", "Inf", 
                            as.numeric(tclvalue(mu1))+tmp)
                    } else {
                        tclvalue(prev2) <- as.character(tmp[1])
                        tclvalue(prev3) <- as.character(tmp[2])
                    }
                    tclvalue(prev1) <- min(abs(c(as.numeric(tclvalue(prev2)), 
                        as.numeric(tclvalue(prev3))) - as.numeric(tclvalue(mu1))))
                }
                tclvalue(submitted) <- "neutral"
            }
        }
        if(tclvalue(submitted)=="dest" && tclvalue(user.destroy)=="0") stop("aborted")

        tkdestroy(fcn.menu)
        if(as.logical(tclObj(returned))){
            if(fcn %in% c("mdd.mantelhaen") && as.logical(tclObj(print.summary))){
                invisible(tmp)
            } else {
                tmp
            }
        }
    }

    ret <- tclVar("")
    submitted <- tclVar(0)
    fcn <- tclVar("mdd.t.test")
    user.destroy.m <- tclVar(FALSE)

    main.menu <- tktoplevel()
    tkwm.title(main.menu,"Minimum Detectable Difference GUI")
    
    frame1 <- tkframe(main.menu, relief="groove", borderwidth=2)
    
    tkpack(tklabel(frame1, text="Continuous, two-sample"), anchor="w")
    
    cont.labs <- c("t-test (equal variance)", "t-test (unequal variance)")
    cont.fcns <- c("mdd.t.test", "mdd.satterthwaite")
    
    for(i in 1:length(cont.labs)){
        tmp <- tkradiobutton(frame1, text=cont.labs[i], value=cont.fcns[i], variable=fcn)
        tkpack(tmp, anchor="w")
    }
    
    tkpack(tklabel(frame1, text="Binomial response/proportions"), anchor="w")
    
    binom.labs <- c("Chi-squared test", "Fisher's exact test", 
        "Cochran-Mantel-Haenszel test (no power calculations)",
        "Cochran-Mantel-Haenszel test (with power calculations)")
    binom.fcns <- c("mdd.chisq", "mdd.fisher.exact", "mdd.mantelhaen",
        "mdd.mantelhaen.pow")
    
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

