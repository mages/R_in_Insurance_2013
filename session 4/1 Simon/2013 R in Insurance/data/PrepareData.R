# Author: Simon Brickman
#         Adam L. Rich
# Date:   July 10, 2013
# Description:
#
#   To prepare data for the R in Insurance presentation
#



require(reshape2)
require(data.table)



setwd('w:/sl/actuarial/R-Software/2013R Conference/')
load('./data/Claims.RData')
load('./data/Claims2.RData')



claims <- melt(
  sClaimCat.anon,
  value.name = "val",
  measure.vars = c(7:10),
  variable.name = "Type"
)



# CLAIMS is claims aggregated by YOA, dev, and Type
CLAIMS <- data.table(claims)
setkey(CLAIMS, YOA, dev, Type)
CLAIMS.AGG <- CLAIMS[, list(valsum = sum(val), valnum = length(val)), key(CLAIMS)]
claims.agg <- as.data.frame(CLAIMS.AGG)



# Held data
HELD.STR <- subset(CLAIMS.AGG, Type == "BlendUSD")
HELD.STR <- HELD.STR[, valsum:=NULL]             
HELD.STR$Type <- as.character(HELD.STR$Type)     
HELD.STR$Type <- "Held"                      
HELD.VALS <- data.table(
  YOA = 2003:2012,
  valsum = c(25e6,30e6,45e6,20e6,50e6,50e6,50e6,35e6,35e6,30e6)
)
setkey(HELD.STR, YOA)
setkey(HELD.VALS, YOA)
HELD <- HELD.STR[HELD.VALS]



# Tack HELD onto the end of CLAIMS.AGG
CLAIMS.WH <- rbind(CLAIMS.AGG, HELD, use.names = TRUE)
CLAIMS.WH$Type <- factor(
  x = as.character(CLAIMS.WH$Type),
  levels = c("Held", "IncurredUSD", "BlendUSD", "MostlikelyUSD", "PessimisticUSD")
)
CLAIMS.WH$YOA <- factor(
  x = CLAIMS.WH$YOA,
  levels = 2003:2012
)



# Prepare for moving average examples
ca$Duration <- as.numeric(ca$ClaimClosedDt - ca$ClaimOpenedDt)
ca$ClosedYYMM <- format(ca$ClaimClosedDt, '%Y%m')
ca$Size <- 5 - findInterval(ca$IncurredUSD, c(0,.1,10000,250000,20e6))

CA <- data.table(ca)
setkey(CA, ClosedYYMM, ClaimSeverityInd)
CASUM <- CA[, list(num = length(Duration), mn = mean(Duration)), by = key(CA)]

v <- monthsteps(d1 = as.Date("2006-01-01"), as.Date("2012-12-31"))
df <- data.frame(v, ClosedYYMM = format(v,'%Y%m'))

sev <- data.frame(ClaimSeverityInd = 1:4)
DF <- data.table(merge(df, sev, all=T))
setkey(DF, ClosedYYMM, ClaimSeverityInd)

DTC.IND <- CASUM[DF]
DTC.IND[is.na(DTC.IND)] <- 0
DTC.IND <- DTC.IND[, totdays:=mn*num]

setkey(CA, ClosedYYMM)
CASUM <- CA[, list(num = length(Duration), mn = mean(Duration)), by = key(CA)]

# create full range from start date to end date in monthly steps
DF <- data.table(df)
setkey(DF, ClosedYYMM) 

DTC <- CASUM[DF]               
DTC[is.na(DTC)] <- 0          
DTC <- DTC[, totdays:=mn*num] 



# Save!
save(file = './data/PreparedData.RData',
  claims, CLAIMS, claims.agg, CLAIMS.AGG, HELD, CLAIMS.WH, DTC, DTC.IND)
