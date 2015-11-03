affy <- read.delim("genotypes_Affymetrix.map",header=F, as.is=T)
affy[1:10,]
omni <- read.delim("genotypes_OmniExpress.map",header=F, as.is=T)
table(affy[,2]%in%omni[,2])

#unique SNP either omni and affy file
NROW(unique(c(omni$V2,affy$V2)))

##distance between each SNP
d <- affy$V4[2:NROW(affy)]-affy$V4[1:(NROW(affy)-1)]

hwe.all <-read.table("AffyHWEall.hwe", header=T, as.is=T)
##extract p-value only after removing missing values
p.hwe <- hwe.all$P[!is.na(hwe.all$P)]

## drawing qq plot
x <- sort(-log10(ppoints(p.hwe)))
y <- sort (-log10(p.hwe))
plot(x,y,ylab="observed", xlab="expected", main="ALL")
abline (a=0,b=1,lty=2)


hwe.eur <-read.table("AffyHWEeur.hwe", header=T, as.is=T)
p.hwe.eur <- hwe.eur$P[!is.na(hwe.eur$P)]

hwe.afr <-read.table("AffyHWEafr.hwe", header=T, as.is=T)
p.hwe.afr <- hwe.afr$P[!is.na(hwe.afr$P)]

par(mfrow=c(1,2))
x <- sort(-log10(ppoints(p.hwe.eur)))
y <- sort (-log10(p.hwe.eur))
plot(x,y,ylab="observed", xlab="expected", main="Europeans")
abline (a=0,b=1,lty=2)


x <- sort(-log10(ppoints(p.hwe.afr)))
y <- sort (-log10(p.hwe.afr))
plot(x,y,ylab="observed", xlab="expected", main="Africans")
abline (a=0,b=1,lty=2)

##compare the MAF between EUR and AFR
## making files
maf.afr <- read.table ("AffyMAFafr.frq", header=T, as.is=T)
maf.eur <- read.table ("AffyMAFeur.frq", header =T, as.is=T)
## merge
mafs <-merge(maf.afr, maf.eur, by="SNP", all=F)

A1freq.afr <- mafs$MAF.x
A1freq.eur <- mafs$MAF.y
A1freq.eur[mafs$A1.x !=mafs$A1.y] <- 1- mafs$MAF.y[mafs$A1.x != mafs$A1.y]
table(mafs$A1.x !=mafs$A1.y)

plot(A1freq.afr, A1freq.eur)

sample <- sample(1:NROW(A1freq.afr), 1000)
plot(A1freq.afr[sample], A1freq.eur[sample])

## PC
affy.pheno <-read.table("phenotypes_Affymetrix.txt", header=T, as.is=T)
plot(affy.pheno$PC1,affy.pheno$PC2,xlab="PC1", ylab="PC2",type="n")
ACB <-affy.pheno$Population =="ACB"
ASW <-affy.pheno$Population =="ASW"
ESN <-affy.pheno$Population =="ESN"
GBR <-affy.pheno$Population =="GBR"
IBS <-affy.pheno$Population =="IBS"

points(affy.pheno$PC1[ASW], affy.pheno$PC2[ASW], col="red", pch=2)
points(affy.pheno$PC1[ACB], affy.pheno$PC2[ACB], col="red", pch=1)
points(affy.pheno$PC1[ESN], affy.pheno$PC2[ESN], col="red", pch=3)
points(affy.pheno$PC1[IBS], affy.pheno$PC2[IBS], col="black", pch=1)
points(affy.pheno$PC1[GBR], affy.pheno$PC2[GBR], col="black", pch=2)

## association files
affy.nocovars <- read.table("Y1_nocovar_Affymetrix.assoc.linear", header=T, as.is=T)
affy.covars <- read.table("Y1_covar_Affymetrix.assoc.linear", header=T, as.is=T)

p.nocovar <- affy.nocovars$P[!is.na(affy.nocovars$P)]
p.covar <- affy.covars$P[!is.na(affy.covars$P)]

## QQ plot

par(mfrow=c(1,2))
plot(-log10(ppoints(p.nocovar)),-log10(sort(p.nocovar)),xlab="expected", ylab="observed", ylim=c(0,5), xlim=c(0,5),main="Without Covariates")
abline(a=0, b=1, lty=2)

plot(-log10(ppoints(p.covar)), -log10(sort(p.covar)), xlab="expected", ylab="observed", ylim=c(0,5), xlim=c(0,5), main="With Covariates")
abline(a=0,b=1, lty=2)

##calculating lambda
chi2.nocovar <-affy.nocovars$STAT^2
chi2.covar <- affy.covars$STAT^2

median(chi2.nocovar, na.rm=T)/qchisq(0.5,1)
median(chi2.covar,na.rm=T)/qchisq(0.5,1)

##Manhattan plots

plot(affy.nocovars$BP, -log10(affy.nocovars$P), xlab="BP", ylab="-log10 P", ylim=c(0,8) )
abline(h=-log10(0.05/NROW(affy.nocovars)),lty=2)
abline(h=-log10(0.00000005), lty=2)
table(p.nocovar<0.05)

plot(affy.covars$BP, -log10(affy.covars$P), xlab="BP", ylab="-log10 P", ylim=c(0,8))
abline (h=-log10(0.00000005), lty=2)
abline (h=-log10(0.05/NROW(affy.covars)), lty=2)

##check what SNPs were significant
affy.covars <- affy.covars[!is.na(affy.covars$P),]
affy.covars[affy.covars$P<0.000005,]




     




