`gui.graph.one.strat` <-
function(){
    require(tcltk) || stop("TCL/TK support is absent")
    fcn.window <- function(){
        fcn <- "graph.one.strat"
        submitted <- tclVar("neutral")
        returned <- tclVar(FALSE)
        user.destroy <- tclVar(FALSE)
  
        alpha <- tclVar(.05)
        alpha.old <- tclVar("")
        alternative <- tclVar("two.sided")
        alternative.old <- tclVar("")
        alt.labs <- c("Not equal", "Treatment mean less than control mean", 
            "Treatment mean greater than control mean")
        alt.vals <- c("two.sided", "less", "greater")
        placebo.size <- tclVar("")
        placebo.size.old <- tclVar("")
        treat.size <- tclVar("")
        treat.size.old <- tclVar("")
        prob.placebo <- tclVar("")
        prob.treat <- tclVar("")
        weighted <- tclVar(FALSE)
        show.grid <- tclVar(FALSE)
                    
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

        grid.entry <- tkcheckbutton(sig.frame, text="Show grid", variable=show.grid)

        for(i in 1:length(alt.labs)){
            tmp <- tkradiobutton(sig.frame, text=alt.labs[i], value=alt.vals[i], 
                variable=alternative)
            if(i == 3){
                tkgrid(grid.entry, tmp, sticky="w", columnspan=2)
            } else {
                tkgrid(tklabel(sig.frame, text=""), tklabel(sig.frame, text=""), 
                    tmp, sticky="w")
            }
        }
        frame.buttons <- tkframe(fcn.menu, borderwidth=2)
      
        frameq <- tkframe(frame.buttons, borderwidth=2, relief="groove")
        frames <- tkframe(frame.buttons, borderwidth=2, relief="groove")
        framen <- tkframe(frame.buttons, borderwidth=2, relief="groove")
    
        qb <- tkbutton(frameq, text=" Quit ", command = function(){
                tclvalue(user.destroy) <- "TRUE"
                tkdestroy(fcn.menu)
            })
        tkpack(qb)
        submit <- tkbutton(frames, text=" Graph ", command = function(){tclvalue(submitted) <- "val"})
        tkpack(submit)
        new.graph <- tkbutton(framen, text=" New ", command = function()x11())
        tkpack(new.graph)

        tkpack(framen, frames, frameq, side="left")    
 
        tkwm.title(fcn.menu, "Significant scenarios for Fisher's exact test")
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
        prob.t.entry <- tkentry(frame1, width=8, textvariable=prob.treat)
            
        tkgrid(null.lab,n.lab,null.lab,rate.lab)
        tkgrid(c.lab, n.c.entry, null.lab, prob.c.entry)
        tkgrid(t.lab, n.t.entry, null.lab, prob.t.entry)
        tkgrid.configure(c.lab,t.lab,sticky="e")
        tkgrid.configure(n.lab,rate.lab,sticky="s")

        ret.1 <- tkradiobutton(frame2, text="No weighting", value=0, variable=weighted)
        ret.2 <- tkradiobutton(frame2, text="Weight by probability", value=1, 
            variable=weighted)
        tkgrid(ret.1,ret.2)
        tkgrid.configure(ret.1, sticky="w")
        tkgrid.configure(ret.2, sticky="e")

        tkgrid(frame1, help.frame, sticky="w")
        tkgrid(sig.frame, sticky="ew", columnspan=2)
        tkgrid(frame2, frame.buttons)
        tkgrid.configure(frame2, sticky="w") 
        tkgrid.configure(frame.buttons, sticky="e") 
            
        var.list <- c("placebo.size","treat.size","alpha","alternative","weighted",
            "prob.placebo","prob.treat","show.grid")
        var.fcns <- rep(c(as.numeric,as.character,as.logical,as.numeric,as.logical),c(3,1,1,2,1))
        var.types <- c("pos.int","pos.int","prop","","","prop","prop","")
        var.descrips <- c("Number of control subjects", "Number of treatment subjects", 
            "Significance level", "Alternative hypothesis", "Weight by probability",
            "Control response rate", "Treatment response rate", "Whether to show grid")

        tkbind(fcn.menu, "<Destroy>", function()tclvalue(submitted) <- "dest")
        while(tclvalue(submitted) != "dest"){
            tkwait.variable(submitted)
            if(tclvalue(submitted) == "help"){
                browseURL(paste(R.home(),"/library/MDD/html/gui.graph.one.strat.html", sep=""), 
                    browser = getOption("browser"))
                tclvalue(submitted) <- "neutral"
            }
            if(tclvalue(submitted) == "val"){
                var.vals <- sapply(var.list, function(x){tclvalue(eval(parse(text=x)))})
                tclvalue(submitted) <- .validate.entries(fcn, var.vals, var.types, 
                    var.descrips)  
            }
            if(tclvalue(submitted) == "sub"){
               if(tclvalue(alpha.old) != tclvalue(alpha) || 
                        tclvalue(placebo.size.old) != tclvalue(placebo.size) || 
                        tclvalue(treat.size.old) != tclvalue(treat.size) ||
                        tclvalue(alternative.old) != tclvalue(alternative)){
                    tmp <- mdd.fisher.exact(as.numeric(tclvalue(placebo.size)),
                        as.numeric(tclvalue(treat.size)), as.numeric(tclvalue(prob.placebo)), 
                        as.numeric(tclvalue(alpha)), .2, 
                        as.character(tclvalue(alternative)))$observed
                    tclvalue(alpha.old) <- tclvalue(alpha)
                    tclvalue(placebo.size.old) <- tclvalue(placebo.size)
                    tclvalue(treat.size.old) <- tclvalue(treat.size)
                    tclvalue(alternative.old) <- tclvalue(alternative)
                }
                graph.one.strat(tmp, as.logical(tclObj(weighted)), 
                    as.numeric(tclvalue(prob.placebo)), as.numeric(tclvalue(prob.treat)),
                    as.logical(tclObj(show.grid)))
                tclvalue(submitted) <- "neutral"
            }
        }
        if(tclvalue(submitted)=="dest" && tclvalue(user.destroy)=="0") stop("aborted")
        if(tclvalue(submitted)=="dest"){
            return(invisible(tmp))
        }
    }
    fcn.window()
}

