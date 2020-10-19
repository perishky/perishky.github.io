## Question 2
## Make the script robust so that any error
## is either identified by assertion or 
## caught by `tryCatch()`.

pheno.filename <- readline(prompt="Enter phenotype file: ") ## pheno-dat.csv
gene.filename <- readline(prompt="Enter gene data file: ")  ## gene-dat.csv
top.n <- readline(prompt="Enter # top genes to show: ")     ## 5

## handle user input errors
stopifnot(file.exists(pheno.filename))
stopifnot(file.exists(gene.filename))
stopifnot(!is.na(as.integer(top.n)))
stopifnot(as.integer(top.n) > 0)

## handle file reading errors, e.g. incorrect format
read.csv.safe <- function(filename) {
	dat <- tryCatch(read.csv(filename, row.names=1), 
		   		    error=function(msg) {
				    	cat("There was a problem with CSV input file ", filename, ".\n")
						stop(msg)
			 		})
	return(dat)
}

top.n <- as.integer(top.n)
pheno.dat <- read.csv.safe(pheno.filename)
gene.dat <- read.csv.safe(gene.filename)
gene.dat <- as.matrix(gene.dat)
stats <- sapply(rownames(gene.dat), function(gene) {
	tryCatch({
		fit <- lm(gene.dat[gene,] ~ bmi+diet+sex, pheno.dat)
		return(coef(summary(fit))["bmi",]) 	
	}, error=function(msg) {
		## if the model fit fails, just replace summary statistics
		## with missing values
		return(rep(NA, 4))
	})
})
stats <- t(stats)
colnames(stats) <- c("estimate","se","t","p.value")
cat("The strongest association is:\n")
idx <- which.min(stats[,"p.value"])
cat("gene:", rownames(stats)[idx], "\n")
cat("effect=", round(stats[idx,"estimate"],2), "\n")
cat("p=", stats[idx,"p.value"], "\n")
plot(pheno.dat$bmi, gene.dat[idx,], pch=19, main=rownames(stats)[idx])
abline(lm(gene.dat[idx,]~pheno.dat$bmi), col="red")
top.idx <- order(stats[,"p.value"])[1:top.n]
cat("The top 5 genes:", paste(rownames(stats)[top.idx], collapse=","), "\n")