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
top.idx <- order(stats[idx,"p.value"])[1:top.n]
cat("The top 5 genes:", paste(rownames(stats)[top.idx], collapse=","), "\n")
```

### Problem 1
Add comments to the script to explain:

1. What the script does

2. What inputs are expected

3. What outputs are expected

4. What errors could be generated

5. What different sections of the script are doing

### Problem 2
Add assertions to the script so that any error 
will cause an assertion to fail.

You might find the function `file.exists()` useful.

### Problem 3
Use `tryCatch()` to the script so that any errors
will be "caught".

