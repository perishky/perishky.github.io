

## How to make an R script robust: practical

In this practical, we will improve the robustness
of the script shown below. 

The script tests associations of BMI with gene expression, 
generates a plot of the top association and a user-specified
number of top gene names. 

You can use the provided files "pheno-dat.csv" and "gene-dat.csv"
for actually running and testing the script.


```r
pheno.filename <- readline(prompt="Enter phenotype file: ") ## pheno-dat.csv
gene.filename <- readline(prompt="Enter gene data file: ")  ## gene-dat.csv
top.n <- readline(prompt="Enter # top genes to show: ")     ## 5
top.n <- as.integer(top.n)
pheno.dat <- read.csv(pheno.filename,row.names=1)
gene.dat <- read.csv(gene.filename,row.names=1)
gene.dat <- as.matrix(gene.dat)
stats <- sapply(rownames(gene.dat), function(gene) {
	fit <- lm(gene.dat[gene,] ~ bmi+diet+sex, pheno.dat)
	coef(summary(fit))["bmi",] 	
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
```

Before running the script use `setwd()` to 
the directory containing the files 
"pheno-dat.csv" and "gene-dat.csv".

### Question 1

The script will actually fail when 
data files "pheno-dat.csv" and "gene-dat.csv"
are used as input. Can you discover why?

### Question 2

Make the script robust so that any error
is either identified by assertion or 
caught by `tryCatch()`.

You might find the function `file.exists()` useful
for verifying that a file name entered by the user 
actually exists. 

