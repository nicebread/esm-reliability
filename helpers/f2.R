# simple wrapper: formats a number in f.2 format
# if a number is, say -0.0001, trim it to exactly zero (use trimToZero = -0.0001)
# (otherwise, it would be displayed as -.00 in the correlation matrix)

f2 <- function(x, digits=0, skipZero=FALSE, prepoint=0, trimToZero=0) {
	
	if (trimToZero != 0) {
		x[abs(x)<trimToZero] <- 0
	}
	
	if (skipZero == TRUE) {zero <- "."} else {zero <- "0."}
	
	if (length(dim(x)) == 2) {
		apply(x, 2, function(x2) {gsub("0\\.", zero, sprintf(paste0("%",prepoint,".",digits,"f"), x2))})
	} else {
		gsub("0\\.", zero, sprintf(paste("%.",digits,"f",sep=""), x))
	}
}
