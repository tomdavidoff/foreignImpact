# cmhcStarts.R
# use CMHC package to get starts for Vancouver and Toronto
# Tom Davidoff
# 05/04/25

library(cmhc)
library(ggplot2)
library(data.table)
library(openxlsx)

DOSTART <- 1
if (DOSTART==1) {
	starts <- data.table(year=numeric(),month=numeric(),cma=numeric,starts=numeric())
	for (y in 2000:2019) {
		for (met in c(933,535)) {
			for (m in 1:12) {
				# 933 is Vancouver, 535 is Toronto
				s <- data.table(get_cmhc(survey = "Scss", series = "Starts", dimension = "Dwelling Type", breakdown = c("Centres"), geoFilter = "Metro", geo_uid = met,year=y,month=m,frequency="monthly",))
				print(s)
				v <- data.table(year=y,month=m,cma=met,starts=s[`Dwelling Type`=="Apartment",Value][1])
				starts <- rbind(starts,v)
				print(starts)
			}
		}
	}
	fwrite(starts,file="data/derived/starts.csv")
}
starts <- fread("data/derived/starts.csv")

# prices
name_cma <- data.table(cma=c(933,535),name=c("GREATER_VANCOUVER","GREATER_TORONTO"))
prices <- data.table(year=numeric(),month=numeric(),Apartment_HPI=numeric(),cma=numeric())
for (m in c(933,535)) {
	dp <- data.table(read.xlsx("data/raw/crea.xlsx",sheet=name_cma[cma==m,name][1],detectDates=TRUE))
	dp[,month:=month(Date)]
	dp[,year:=year(Date)]
	dp <- dp[,.(month,year,Apartment_HPI,cma=m)]
	print(dp)
	prices <- rbind(prices,dp)
	print(prices)
}

print(starts)
print(prices)

starts <- merge(starts,prices,by=c("year","month","cma"),all.x=TRUE)
starts[,date:=year+(month-1)/12]
starts <- starts[,.(date,cma,starts,Apartment_HPI)]
starts <- melt(starts,id.vars=c("date","cma"))
starts[,value:=log(value)]
print(starts)
for (k in c(933,535)) {
	ggplot(starts[cma==k],aes(x=date,y=value,color=paste(variable))) + geom_line() + ylim(4,10) + xlim(2010,2020)+ geom_point()
	ggsave(paste0("text/startsPrices",k,".png"))
}

