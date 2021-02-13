###################################
#
# R source code for data analysis in
# "RELIABILITY AND VALIDITY OF THE COMPULSORY CITIZENSHIP BEHAVIORS SCALE:
#  6-STEP R-BASED PSYCHOMETRICS PROTOCOL AMONG NURSES IN TURKEY"
#
###################################

# Packages
for (n in c('readxl','haven','GPArotation', 'corrplot', 'ggpubr', 'SparseM', 'foreign', 'utils', 'relimp', 
            'ggplot2', 'ggdendro', 'psych', 'Hmisc', 'ltm', 'mirt', 'eRm', 'mokken', 'lavaan','OpenMx', 'writexl',
            'semTools','semPlot', 'qgraph','sem','CTT','pwr','PerformanceAnalytics','RColorBrewer','ggcorrplot',
            'GGally','corrr','RColorBrewer','MBESS','cluster')) {if(!require(n,character.only=TRUE)){install.packages(n)}}
library(n,character.only=TRUE)

# Reading the data
mydata <- read_sav("data(ccb_530_noout).sav")
names(mydata)
dim(mydata)

#####################  STEP 1 - Descriptive statistics ##################### 
# Sum of all WEMWBS items
attach(mydata)
CCB_total <- ccb_1 + ccb_2 + ccb_3 + ccb_4 + ccb_5
mydata$CCB_total <- CCB_total
detach(mydata)
summary(mydata$CCB_total)
CCB_table <- mydata[,6:10]
lowerCor(CCB_table, method = "spearman")

pairs.panels(CCB_table, scale=TRUE, stars=T, ci=TRUE, alpha=0.05, digits = 3, # Correlation chart (pearson)
             method = "pearson", hist.col = "green",cex.cor=1)
pairs.panels(CCB_table, scale=TRUE, stars=T, ci=TRUE, alpha=0.05, digits = 3, # Correlation chart (Spearman)
             method = "spearman",hist.col = "green", cex.cor=1)

#Power analysis
correlation <- c(.849,.770, .814,.699,.740,.767,.600,.634,.654,.827)
mean(correlation)
pwr.r.test(n =530, r = .735, sig.level = 0.05, power =)

##################### STEP 2 - IRT analyses (the mokken, ltm, and mirt packages) ##################### 
moscales.for.lowerbounds <- function( x, lowerbounds=seq(from=0.05,to=0.60,by=0.05) )
{
  ret.value <- NULL;
  for( lowerbound in lowerbounds )
  {
    tmp <- aisp( x,  lowerbound=lowerbound );
    if( is.null(ret.value) )
    {
      ret.value <- data.frame( "Item"=rownames(tmp), "Scales."=tmp[,1] );
    }
    else
    {
      ret.value <- cbind( ret.value, "Scales."=tmp[,1] );
    }
    names(ret.value)[ncol(ret.value)] <- paste("c=",sprintf("%.2f",lowerbound),sep="");
  }
  rownames(ret.value) <- NULL;
  ret.value;
}

# Compute scalability coefficients
CCB_table <- as.data.frame(CCB_table[complete.cases(CCB_table),])
coefH(CCB_table)$H

# examine aisp for increasing c levels (run the function you defined above and give it a name)
motable.CCB_table <- moscales.for.lowerbounds( CCB_table )

# see the results
motable.CCB_table

# save it as a data frame
CCB_table2 <- as.data.frame(motable.CCB_table)

##################### STEP 3 - Parametric IRT #####################
# Rating Scale model (equivalent of Rasch for ordinal items)

fit1.CCB_table2 <- PCM(CCB_table) #, constrained = FALSE, Hessian=TRUE

# separation reliability (proportion of item variance not due to error - similar to C-alpha)
ppr1 <- person.parameter(fit1.CCB_table2)

# item fit (between 0.6 and 1.4 acc to Wright BD, Linacre JM. Reasonable mean-square fit values. Rasch Meas Trans. 1994;8(2):370.)
itemfit.fit1.CCB_table2 <- itemfit(ppr1)

# check min and max infit and outfit
min(itemfit.fit1.CCB_table2$i.infitMSQ)
max(itemfit.fit1.CCB_table2$i.infitMSQ)

min(itemfit.fit1.CCB_table2$i.outfitMSQ)
max(itemfit.fit1.CCB_table2$i.outfitMSQ)

##################### STEP 4 - Explanatory and Confirmatory factor analysis ##################### 
### Exploratory factor analysis (EFA)
##correlation adequacy Bartlett's test
cortest.bartlett(CCB_table, n = nrow(CCB_table))

##sampling adequacy KMO test
KMO(CCB_table)

##how many factors?
#Parallel analysis
p1 <- fa.parallel(CCB_table, fm="ml", fa="both") #both principal components and principal factors
png(filename="figure3.png", type="cairo", height = 6, width = 6, units = 'in', res=500)
plot(p1)
dev.off()
sum(p1$fa.values > 1.0) ##old kaiser criterion
sum(p1$fa.values > .7) ##new kaiser criterion

# very simple structure analysis
vss(CCB_table, 3)

# default FA - 1 factor, min residual & principal axis
fa(CCB_table,  nfactors=1, fm="minres", n.iter=10)
fa(CCB_table,  nfactors=1, fm="pa")

# plot the fa solution
plot(fa(CCB_table,  nfactors=1, fm="pa"))

# hierarchical cluster analysis using ICLUST (groups items)
iclust(CCB_table, title="CCB_table using Pearson correlations")
summary(iclust(CCB_table))
iclust.diagram(iclust(CCB_table, title="CCB_table using Pearson correlations"))

# hierarchical factor solution to find omega coefficient
omega(CCB_table, nfactors=2, sl=FALSE)

# omega with polychoric matrix
CCB_table.poly <- polychoric(CCB_table)
omega(CCB_table.poly$rho, nfactors=2,  sl=FALSE)

## Descriptive  statistics of CCB factor
attach(mydata)
factor1 <- c("ccb_1", "ccb_2", "ccb_3", "ccb_4", "ccb_5")
mydata$factor1 = apply(mydata[ , factor1], 1, mean) ##creates average scores
names(mydata)
summary(mydata)
sd(mydata$factor1)
CCB_Factor <- mydata$factor1
hist(CCB_Factor)
boxplot(CCB_Factor)

?hist
### Confirmatory factor analysis (CFA)
#specify the model
#Model1
Model1 <- "CCBs =~ ccb_1 + ccb_2 + ccb_3 + ccb_4 + ccb_5"

# fit the model
fit1 <- lavaan::cfa(Model1, data=mydata)

# model summary
summary(fit1, standardized=TRUE, fit.measures = TRUE)

#Modification indices
modindices(fit1, minimum.value = 10, sort = TRUE)

#fit indices
fitmeasures(fit1, c("gfi", "agfi", "nfi", "cfi", "tli", "rmsea", "srmr","aic","bic"))

#Model2
Model2 <- "CCBs =~ ccb_1 + ccb_2 + ccb_3 + ccb_4 + ccb_5
ccb_4~~ccb_5 + ccb_3"

# fit the model
fit2 <- lavaan::cfa(Model2, data=mydata)

# model summary
summary(fit2, standardized=TRUE, fit.measures = TRUE)

#Modification indices
modindices(fit2, minimum.value = 10, sort = TRUE)
fitmeasures(fit2, c("gfi", "agfi", "nfi", "cfi", "tli", "rmsea", "srmr","aic","bic"))

# coefficients only
coef(fit2)

# R square
inspect(fit2, 'r2')
fitmeasures(fit2)

# CFA diagram from psych package
semPaths(fit2,what="paths",whatLabels="stand", layout = "tree", color = "green", rotation = 4)

##################### STEP 5 - CCT ##################### 
# CTT for a single scale

# Alpha by bootstrapping
ci.reliability(data=CCB_table, type="alpha", conf.level = 0.95, interval.type="perc", B=100)

# Guttman lambda 6 (G6) and Beta values
splitHalf(CCB_table) 

# Omega
ci.reliability(data=CCB_table, type="omega", conf.level = 0.95, interval.type="perc", B=100)

##################### STEP 6 ##################### 
# check everything about your scores

# check descriptives
summary(CCB_table)

# Histograms
png(filename="figure1.png", type="cairo", height = 6, width = 6, units = 'in', res=300)
hist(CCB_total, breaks=40 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="distribution of WEMWBS_total" , main="")
dev.off()

# Plot png image
png(filename="figure2.png", type="cairo", height = 8, width = 8, units = 'in', res=300)
cor.plot(lowerCor(CCB_table, method = "spearman"), numbers=TRUE, main="Correlations between CCB items", 
         cex=0.5, cex.axis=0.7, xlas = 2)
dev.off()





