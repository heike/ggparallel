#' Data linking genes and pathways.
#'
#' Marie, could you try to describe how you got this data?. 
#' 
#'
#' @docType data
#' @name genes
#' @author Marie Vendettuoli \email{mariecv26@@gmail.com}
#' @references \url{url to the repositories}
#' @keywords data
#' @examples
#' data(genes)
#' require(RColorBrewer)
#' gghammock(list("chrom", "path"), data = genes, color = "white", 
#'   factorlevels =  c(sapply(unique(genes$chrom), as.character), 
#'   unique(genes$path))) + 
#'   scale_fill_manual(values = c(rep("grey80", 24), brewer.pal("YlOrRd", n = 9)), guide="none") + 
#'   coord_flip() 
NULL