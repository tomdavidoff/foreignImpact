library(data.table)
library(ggplot2)
library(fixest)

df <- fread("data/raw/chsp2022.csv")
print(summary(df))

vv <- df[GEO=="Vancouver, Census metropolitan area (CMA)",log(VALUE),by=c("Residency participation","Period of construction")]
vvg <- df[, log(VALUE),by=c("Residency participation","Period of construction","GEO")]
# all condos: 830K/710K
print(log(830)-log(710))
print(summary(vv))
print(vv)
print(vvg)
print(summary(vvg))
vvg <- dcast(vvg, GEO+ `Period of construction` ~ `Residency participation` , value.var="V1")
print(summary(vvg[,`Resident owners only`-`Non-resident participation`]))
print(vvg)

if (2<1) {
	for (p in df[,`Period of construction`]) {
		print(p)
		for (l in df[,GEO]) {
			for (k in unique(df[,`Residency participation`])) {
				print(c(substring(p,1,4),substring(l,1,8),k,df[`Period of construction`==p & GEO==l & `Residency participation`==k,VALUE]))
			}
		}
	}
}
