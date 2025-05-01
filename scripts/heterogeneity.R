#heterogeneity.R
# R to explore heterogeneity in Vancouver condos
# Tom Davidoff
# 04/23/25

library(data.table)
library(ggplot2)
library(fixest)

# read in mls.R from foreignTrend
dsales <- fread("~/onedrive/foreignTrend/data/derived/mls.csv")
dsales[, date := as.Date(`Sold Date`, format = "%m/%d/%Y")]
dsales <- dsales[!is.na(date)]
dsales[, price := as.numeric(gsub("\\$", "", gsub(",", "", `Price`)))]
# convert to 1/30/2022 style to date 

# convert price from $1,000,482 to numeric
for (t in c("Apartment/Condo", "HOUSE")) {
	dst <- dsales[TypeDwel==t]
	print(quantile(dst[date>as.Date("2015-01-01") & date<as.Date("2015-12-31"),price],probs=c(.1,.25,.5,.75,.9,.95,.975,.99)))
	print(quantile(dst[date>as.Date("2016-02-01") & date<as.Date("2016-04-01"),price],probs=c(.1,.25,.5,.75,.9,.95,.975,.99)))
	print(quantile(dst[date>as.Date("2016-04-01") & date<as.Date("2016-08-01"),price],probs=c(.1,.25,.5,.75,.9,.95,.975,.99)))
	print(quantile(dst[date>as.Date("2016-08-15") & date<as.Date("2016-11-15"),price],probs=c(.1,.25,.5,.75,.9,.95,.975,.99)))
	print(quantile(dst[date>as.Date("2017-01-01") & date<as.Date("2017-12-31"),price],probs=c(.1,.25,.5,.75,.9,.95,.975,.99)))
}
