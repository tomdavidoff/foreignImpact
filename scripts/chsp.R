library(data.table)
library(ggplot2)
library(fixest)

df <- fread("data/raw/chsp2022.csv")
print(summary(df))
for (p in df[,`Period of construction`]) {
	print(p)
	for (l in df[,GEO]) {
		for (k in unique(df[,`Residency participation`])) {
			print(c(substring(p,1,4),substring(l,1,8),k,df[`Period of construction`==p & GEO==l & `Residency participation`==k,VALUE]))
		}
	}
}
vd <- mean(df[GEO=="Vancouver, Census metropolitan area (CMA)" & `Residency participation`=="Resident owners only",log(VALUE)])
vf <- mean(df[GEO=="Vancouver, Census metropolitan area (CMA)" & `Residency participation`=="Non-resident participation",log(VALUE)])
print(c(vd,vf))
print(vd-vf)
print(mean(df[`Residency participation`=="Resident owners only",log(VALUE),by=c("Period of construction","GEO")],na.rm=TRUE) - mean(df[`Residency participation`=="Non-resident participation",log(VALUE),by=c("Period of construction","GEO")],na.rm=TRUE))
