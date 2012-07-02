#' Creating hammock plots and parallel sets
#' 
#' quick description
#' 
#' more detailed description: we need to reference Kosara's paper on parallel sets, we should also point to 
#' Matt Schonlau's paper for the hammocks
#' 
#' @param vars list of variable names to be included in the plotting. order of the variables is preserved in the display
#' @param data data frame 
#' @param weight weighting variable - use character string
#' @param alpha level of $\alpha$ blending for ribbons, value between 0 and 1, defaults to 0.5.
#' @param width width of variables 
#' @param order flag variable with three levels -1, 0, 1 for levels in decreasing order, levels in increasing order and levels unchanged. This variable can be either a scalar or a vector
#' @param color colors for levels. if not specified, grey is used
#' @param label binary variable (vector), whether labels should be shown.
#' @param angle by which text for labelling is rotated. Ignored if label = FALSE
#' @param text.offset (vector) of values for offset the labels
#' @param ... passed on directly to all of the ggplot2 commands
#' @usage returns a  ggplot2 object that can be plotted directly or used as base layer for additional modifications.
#' @export
#' @examples
#' data(mtcars)
#' gghammock(list("gear", "cyl"), data=mtcars)
#' # flip plot and rotate text
#' gghammock(list("gear", "cyl"), data=mtcars, angle=0) + coord_flip()
#' # change colour scheme
#' gghammock(list("gear", "cyl"), data=mtcars, angle=0) + coord_flip() + 
#'   scale_fill_brewer(palette="Set1")
#' # example with more than two variables:
#' titanic <- as.data.frame(Titanic)
#' gghammock(list("Class", "Sex", "Survived"), data=titanic, weight="Freq", angle=0) + 
#'   coord_flip() + scale_fill_brewer(palette=6, guide="none")
#' # biological examples: genes and pathways
#' data(genes)
#' require(RColorbrewer)
#' gghammock(list("chrom", "path"), data = genes, color = "white", 
#'   factorlevels =  c(sapply(unique(genes$chrom), as.character), 
#'   unique(genes$path))) + 
#'   scale_fill_manual(values = c(rep("grey80", 24), brewer.pal("YlOrRd", n = 9)), guide="none") + 
#'   coord_flip() 


gghammock <- function(vars=list(), data, weight=NULL, alpha=0.5, width = 0.25, order = 1, color = NA, label = TRUE, angle=90, text.offset=NULL, ...) {
  ### error checking
  k = length(vars)
  if (k < 2) message("Error: gghammock needs at least two variables. Use vars=list('X', 'Y')")
  
  ### if user doesn't specify the weight, assign value of 1. 
  data$weight <- data[,weight]
  if (is.null(weight)) data$weight <- 1
  
  ## if ordering is selected, organize x and y axis by weight
  # make order a vector of length length(vars)
  order <- rep(order, length=length(vars))
  for (i in 1:length(vars)){
	if (! is.factor(data[,vars[[i]]])) 
  		data[,vars[[i]]] <- factor(data[,vars[[i]]])

    if (order[i] != 0)
      data[,vars[[i]]] <- reorder(data[,vars[[i]]], data$weight, 
                             function(x) if (order[i] > 0) sum(x)
                             			 else -sum(x)
                             )
  }
  
#browser()  
  for (i in 1:length(vars)) {
  	
  	levels(data[,vars[[i]]]) <- paste(vars[[i]], levels(data[,vars[[i]]]), sep=":")
  }
  
  ## helper function
  getRibbons <- function(xid,yid) {    
    ## get the names of the x and y variables
    x <- vars[[xid]]
    y <- vars[[yid]]
    
    xname <- x
    yname <- y
    
    ## create the data table, x, y, and weight
    ## correction: order the table by x, y according to gghammocks ordering
    ##	dfxy <- as.data.frame(xtabs(data$weight~data[,y] + data[,x]))
    dfxy <- as.data.frame(xtabs(data$weight~data[,x] + data[,y]))
    dfxy <- subset(dfxy, Freq > 0)
     
    #names(dfxy)[1:2] <- c(yname, xname)
    ## correction: maintain x, y order as for variable entry
    names(dfxy)[1:2] <- c(xname, yname)
    
    ## get the ordering for data according to x-axis categories
    ## correction: order in the same direction as x, y axis
    ## idx <- order(dfxy[,y], dfxy[,x], decreasing = TRUE)
    idx <- order(dfxy[,x], dfxy[,y], decreasing = FALSE)
    
    ## find the position of X-axis connector
    dfxy$X[idx] <- cumsum(dfxy$Freq[idx])    
    
    ## get the ordering for data according to y-axis categories
    ## correction: order in the same direction as x, y axis
    ##   idx <- order(dfxy[,x], dfxy[,y])
    idx <- order(dfxy[,y], dfxy[,x], decreasing = FALSE)
    ## find the position of the Y-axis connector
    dfxy$Y[idx] <- cumsum(dfxy$Freq[idx])
    
    ## assign row number as id
    dfxy$id <- 1:nrow(dfxy)    
    
    dfm <- melt(dfxy, measure.var=c("X", "Y"))
    levels(dfm$variable) <- c(x,y)    
    
    dfxy$XX <- dfxy[,xname]
    dfxy$YY <- dfxy[,yname]
    
    dfm$Nodeset <- dfm[,yname]    
    
    dfm$offset <- c(width/2,-width/2)[as.numeric(dfm$variable)]
    dfm$xid <- xid - 1
    dfm$yid <- yid
    
    geom_ribbon(aes(x=as.numeric(variable)+offset+xid,ymin=value -Freq, ymax= value, group=id, 
                    fill=Nodeset),	alpha=alpha, data=dfm)
  }
  ## end helper function
  
  
  gr <- list()
  for (i in 1:(length(vars)-1))
    gr[[i]] <- getRibbons(i,i+1)

  	
  dfm <- melt(data[,c("weight", unlist(vars))], id.var="weight")
  names(dfm)[3] <- "Nodeset"

  llabels <- NULL
  if (label) {
	  label.stats <- ddply(dfm, .(variable, Nodeset), summarize,
	                       n = length(weight),
	                       weight=sum(weight)
	                       )
	  maxWeight <- sum(label.stats$weight)/length(unique(label.stats$variable))
	  label.stats$ypos <- cumsum(label.stats$weight)-(as.numeric(label.stats$variable)-1)*maxWeight
	  label.stats$ypos <- label.stats$ypos-label.stats$weight/2
#browser()
  	 if (is.null(text.offset)) text.offset <- 0
  	 label.stats$text.offset <- rep(text.offset, length=nrow(label.stats))
  	 
	  varnames <- paste(unlist(vars), sep="|", collapse="|")
	  label.stats$Nodeset <- gsub(sprintf("(%s):(.*)",varnames),"\\2", as.character(label.stats$Nodeset))
	llabels <- list(geom_text(aes(x=as.numeric(variable)+text.offset, y=ypos, label=Nodeset),
	                      colour = "grey20", data=label.stats, angle=angle, size=4), 
	                geom_text(aes(x=as.numeric(variable)+0.01+text.offset, y=ypos-0.01, label=Nodeset),
	                      colour = "grey90", data=label.stats, angle=angle, size=4)) 
  }
  ggplot() + geom_bar(aes(weight=weight, x=variable, fill=Nodeset), 
                      colour = color, width=width,data=dfm) + 
             llabels + xlab("")  + gr 
}

