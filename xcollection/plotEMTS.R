library(reshape2)
library(ggplot2)
#library(xtable)

# Visualizes table (from maketable.awk), writes out a latex table and
# the plot in a PDF. 'grpfile' is the output file name.
# usage: grptable("TABLE-META", "TEST")

plotEMTS <- function(tabfile, grpfile) {
    dlong = read.table(tabfile, header=TRUE)
    dwide = dcast(dlong, sys + model ~ doc, value.var="score")
    dmelt = melt(dwide,
		 id.vars=c("sys", "model"),
		 variable.name="doc",
		 value.name="score")
    #print(xtable(dwide, digits=4), type="latex",
	  #file=paste(grpfile, ".tex", sep=""))
    #pdf(paste(grpfile, ".pdf", sep=""), width=11, height=8)
    p = ggplot(dmelt, aes(doc, score, fill=doc)) +
	geom_bar(width=.4, stat="identity") +
	facet_grid(sys ~ model) +
	theme(strip.text.x=element_text(size=8, angle=90),
	      axis.text.x=element_text(angle=90,vjust=0.5,size=6))
    #print(p)
    #dev.off()
    return(p)
}

