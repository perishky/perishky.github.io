pheno.dat <- read.csv("pheno-dat.csv")
n <- nrow(pheno.dat)
gene.dat <- t(sapply(1:100, function(i) {
	with(pheno.dat, rnorm(1,sd=0.5)*bmi + rnorm(1,sd=0.5)*diet + rnorm(1,sd=5)*sign(sex=="M") + rnorm(n,sd=1))
}))
rownames(gene.dat) <- paste0("gene", 1:nrow(gene.dat))
colnames(gene.dat) <- pheno.dat$id
gene.dat[5,] <- NA
gene.dat[10,c(1,10)] <- 2.003
write.csv(gene.dat, file="gene-dat.csv")
