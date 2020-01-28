insertion_correlation_clust <- function(input, dest) {
  d <- read.table(input,sep="\t",head=T)
  d$chrompos <- paste0(d$chrom, d$pos)
  tab <- t(sapply(unique(d$chrompos), function(x) levels(as.factor(d$TE)) %in% d$TE[which(d$chrompos==x)]))
  colnames(tab) <- levels(as.factor(d$TE))
  
  m <- cor(tab)
  col<- colorRampPalette(c("blue", "white", "red"))(20)
  pdf(width=10,heigth=10)
  heatmap(m, col=col, symm=TRUE)
  dev.off()
}
