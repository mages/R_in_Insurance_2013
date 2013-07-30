\name{MAChart}
\alias{MAChart}
\title{
Moving Average Chart to deal with claim duration
}
\description{
quick way to plot moving average chart
}
\usage{
MAChart<-function(MAP=12,
                  df,
                  xcol=5,
                  ycol=4,
                  ywt=3,
                  xstep=1,
                  xlab="Date",
                  ylab="Duration",
                  main="",
                  leg="",
                  fac="",
                  xRotate=FALSE) 
}
\arguments{
  \item{MAP}{
moving average period to use (default to 12)
}
  \item{df}{
dataframe containing data
}
  \item{xcol}{
column containing complete period of months for graphing in format as.Date (yyyy-mm-dd)
}
  \item{ycol}{
column containing mean duration (in days) of claims closed in closure month. Duration is measured from open date to closed date on ClaimCentre
}
  \item{ywt}{
column containing number of claims closed in month
}
  \item{xstep}{
step for x-axis labels
}
  \item{xlab}{
x-axis title
}
  \item{ylab}{
y-axis title
}
  \item{main}{
heading for chart
}
  \item{leg}{
column name containing factor for legend. Note this column need not be a factor but must be capable of being turned into a factor (i.e. be categorical) 
}
  \item{fac}{
Used to creates series of charts, for each element of fac. Enter name of 
column to use. Note this column need not be a factor but must be capable of being turned into a factor (i.e. be categorical) 
}
  \item{xRotate}{
default in FALSE. Setto true to rotate x-axis
}

}

\details{
Makes use of ggplot2,directlabels, data.table and scales packages
}
\value{
returns a chart
}
\author{
Simon Brickman
}
\examples{
#load data
load("~/2013RConference/data/claims2.RData")
ca<-claims2

ca$Duration<-as.numeric(ca$ClaimClosedDt-ca$ClaimOpenedDt)
ca$ClosedYYMM<-format(ca$ClaimClosedDt,'%Y%m')

#run by split
CA<-data.table(ca)
setkey(CA,ClosedYYMM,ClaimSeverityInd)  #add factor ClaimSeverityInd
CASUM<-CA[,list(num=length(Duration),mn=mean(Duration)),by=key(CA)]

#create full range from start date to end date in monthly steps
v<-monthsteps(d1=as.Date("2006-01-01"),as.Date("2012-12-31"))
df<-data.frame(v,ClosedYYMM=format(v,'%Y%m'))
DF<-data.table(df)
setkey(DF,ClosedYYMM) 

#run overall
setkey(CA,ClosedYYMM)
CASUM<-CA[,list(num=length(Duration),mn=mean(Duration)),by=key(CA)]

#join
CAS1<-CASUM[DF]               #outer join DF to CASUM
CAS1[is.na(CAS1)]<-0          #replace NAs with 0
CAS1<-CAS1[,totdays:=mn*num] #add column for total open period per closed month

library(scales)

MAChart(df=as.data.frame(CAS1),ywt=2,ycol=3,xcol=4,
        main="Duration to close (12m moving average)",
        xlab="Closed Year+Month",ylab="Average Duration (days)",xstep=3,xRotate=T)
