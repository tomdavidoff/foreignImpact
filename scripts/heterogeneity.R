#heterogeneity.R
# R to explore heterogeneity in Vancouver condos
# Tom Davidoff
# 04/23/25

library(data.table)
library(ggplot2)
library(fixest)

MADESALES <- 1
if (MADESALES == 0) {
	# read in mls.R from source

	df <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_sales_20240331_REVD24.csv",select=c("JURISDICTION_CODE","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION","FOLIO_ID","ROLL_NUMBER"))
	print(summary(df))

	da <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_addresses_20240331_REVD24.csv",select=c("FOLIO_ID","UNIT_NUMBER","STREET_NUMBER","STREET_DIRECTION_PREFIX","STREET_NAME","STREET_TYPE","STREET_DIRECTION_SUFFIX","CITY","POSTAL_CODE","PROVINCE","JURISDICTION_CODE"))
	da[,building:=paste(STREET_NUMBER,STREET_DIRECTION_PREFIX,STREET_NAME,JURISDICTION_CODE)]
	da[,buildingN:=.N,by=c("building")]
	df <- merge(df,da,by=c("FOLIO_ID","JURISDICTION_CODE"))

	dd <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_descriptions_20240331_REVD24.csv",select=c("FOLIO_ID","ACTUAL_USE_DESCRIPTION","JURISDICTION_CODE","REGIONAL_DISTRICT"))

	df <- merge(df,dd,by=c("FOLIO_ID","JURISDICTION_CODE"))
	fwrite(df,"data/derived/sales.csv")
}
df <- fread("data/derived/sales.csv")

print(summary(df))
print(table(df[,REGIONAL_DISTRICT]))
df <- df[REGIONAL_DISTRICT=="Metro Vancouver"]
print(table(df[,ACTUAL_USE_DESCRIPTION]))
df <- df[ACTUAL_USE_DESCRIPTION=="Strata-Lot Residence (Condominium)"]
print(table(df[,CONVEYANCE_TYPE_DESCRIPTION]))
df <- df[CONVEYANCE_TYPE_DESCRIPTION=="Improved Single Property Transaction"]
print(df[1:10,CONVEYANCE_DATE])
df[,year:=as.numeric(substring(CONVEYANCE_DATE,1,4))]
df[,month:=as.numeric(substring(CONVEYANCE_DATE,5,6))]
df[,yearQuarter:=year+(ceiling((month)/3)/4)]
df <- df[year>=2010]
df <- df[!is.na(CONVEYANCE_PRICE)]
df <- df[CONVEYANCE_PRICE>quantile(CONVEYANCE_PRICE,0.0001) & CONVEYANCE_PRICE<quantile(CONVEYANCE_PRICE,0.9999)]
df[,bMean:=mean(log(CONVEYANCE_PRICE),na.rm=TRUE),by=c("building","yearQuarter")]
dv <- df[, .(all=var(log(CONVEYANCE_PRICE)),between=var(bMean),within=var(log(CONVEYANCE_PRICE)-bMean)),by=yearQuarter]
dv <- melt(dv, id.vars="yearQuarter", variable.name="type", value.name="var")
print(dv)
ggplot(dv, aes(x=yearQuarter,y=var,group=type,color=type)) +
	geom_line() +
	geom_point() +
	ggtitle("Variance Decomposition of Log Price") +
	xlab("Year") +
	ylab("Variance") +
	geom_vline(xintercept=2016.75,linetype="dashed",color="red") +
	theme(legend.position="bottom")
ggsave("text/variance_decomposition.png")

q("no")

