vcontext("ggparallel")
data(mtcars)
ggparallel(list("gear", "cyl"), data=mtcars)
save_vtest("Basic common angle plot with mtcars data")

ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)
save_vtest("Basic hammock plot with mtcars data")

require(RColorBrewer)
require(ggplot2)
cols <- c(brewer.pal(4, "Reds")[-1], brewer.pal(4, "Blues")[-1])
ggparallel(list("gear", "cyl"), ratio=0.2, data=mtcars, method="hammock", text.angle=0) + scale_fill_manual(values=cols) + scale_colour_manual(values=cols) + theme_bw()
save_vtest("Basic hammock plot with mtcars data with ColorBrewer colors")

## combination of common angle plot and hammock adjustment:
ggparallel(list("gear", "cyl"), data=mtcars, method="adj.angle", ratio=2)
save_vtest("Combination of common angle plot and hammock adjustment with ColorBrewer colors")



## compare with method='parset'
ggparallel(list("gear", "cyl"), data=mtcars, method='parset')
save_vtest("Basic parallel set plot")

## flip plot and rotate text
ggparallel(list("gear", "cyl"), data=mtcars, text.angle=0) + coord_flip()
save_vtest("Basic parallel set plot, flipped plot, rotated text")

## change colour scheme
ggparallel(list("gear", "cyl"), data=mtcars, text.angle=0) + coord_flip() +
  scale_fill_brewer(palette="Set1") +
  scale_colour_brewer(palette="Set1")
save_vtest("Basic parallel set plot, flipped plot, rotated text with ColorBrewer colors")

## example with more than two variables:
titanic <- as.data.frame(Titanic)
ggparallel(names(titanic)[c(1,4,2,1)], order=0, titanic, weight="Freq") +
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")

cols <- c(brewer.pal(5,"Blues")[-1], brewer.pal(3, "Oranges")[-1], brewer.pal(3, "Greens")[-1])
ggparallel(names(titanic)[c(1,4,2,1)], order=0, titanic, weight="Freq") +
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + theme_bw()
save_vtest("Common Angle plot with Titanic data")

## hammock plot with same width lines
ggparallel(names(titanic)[c(1,4,2,3)], titanic, weight=1, asp=0.5, method="hammock", ratio=0.2, order=c(0,0)) +
  theme( legend.position="none") +
  scale_fill_brewer(palette="Paired") +
  scale_colour_brewer(palette="Paired")
save_vtest("Hammock plot with Titanic data")

## hammock plot with line widths adjusted by frequency
ggparallel(names(titanic)[c(1,4,2,3)], titanic, weight="Freq", ratio = 0.2, asp=0.5, method="hammock", order=c(0,0), text.angle=0, width=0.45) +
  theme( legend.position="none") +
  scale_fill_brewer(palette="Paired") +
  scale_colour_brewer(palette="Paired")
save_vtest("Hammock plot with Titanic data, line widths adjusted by frequency")

  ## biological examples: genes and pathways
  data(genes)
  cols <- c(rep("grey80", 24), brewer.pal("YlOrRd", n = 9))
  genes$chrom <- factor(genes$chrom, levels=c(paste("chr", 1:22, sep=""), "chrX", "chrY"))
  ggparallel(list("path", "chrom"), text.offset=c(0.03, 0,-0.03), data = genes,  width=0.1, order=c(1,0), text.angle=0, color="white",
             factorlevels =  c(sapply(unique(genes$chrom), as.character),
                               unique(genes$path))) +
    scale_fill_manual(values = cols, guide="none") +
    scale_colour_manual(values = cols, guide="none") +
    coord_flip()
  save_vtest("Common angle plot with gene data")

end_vcontext()
