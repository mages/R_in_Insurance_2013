\name{plotchart}
\alias{plotchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function to show chart
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Shows series of charts with labelling of legends based on dataframe df. The series is defined by fac and the legend by leg which should be defined in quotes from names in df.
}
\usage{
plotchart <- function(df, fac = "ClaimSeverityInd", 
                          vals = "FREQ_EPI_1m", 
                          leg = "YOA",
                          x = "dev", 
                          xl = "Development quarters", 
                          yl = "Claims per ?m of premiums", 
                          HeadTitle = "",
                          scale.by = NULL,
                          xscale.by=NULL,
                          yScaleMin=NULL,
                          usedirectlabel=TRUE,
                          UseZeroYaxis=TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Dataframe
}
  \item{fac}{
%%     ~~Describe \code{fac} here~~
Used to creates series of charts, for each element of fac. Need not be factor as this is created automatically if it isn't already a factor.
}
  \item{vals}{
vector of values to plot. Must be numeric
}
  \item{leg}{
separate lines are drawn for each element of leg. leg is also added using directlabels package to chart. Need not be factor as this is created automatically if it isn't already a factor.
}
  \item{x}{
x-axis variable
}
  \item{xl}{
x-axis title
}
  \item{yl}{
y-axis title
}
  \item{HeadTitle}{
Header title. The fac value for each chart is added automatically to the title.
}
  \item{scale.by}{
An optional scale factor can be applied to the values. Useful for say \% by setting to 100. If yl is started with LOSS RATIO then the scale.by defaults to 100.
}
  \item{xscale.by}{
An optional scale factor can be applied to the values on the x axis. Useful for say \% by setting to 100.
}

  \item{yScaleMin}{
An optional value for the y axis to specify the minimum limit to show. Useful when showing development ratios for early years of account which should reach 1 ultimately (if the ultimate target is to be reached). Enter with the scale factor applied (i.e. 75 for 75\% if scale.by=100)
}
  \item{UseZeroYaxis}{
An optional value to supress use of zero on y axis, by setting to FALSE. Useful if the range of y values is very different from 0. Default value is TRUE, to show 0 on y axis.
}
  \item{usedirectplot}{
An optional value, defaulted to TRUE, to allow direct labels to be added to charts.Sometimes worth overring with usedirectplot=FALSE where the chart output is too complex.
}



}

\details{
Makes use of ggplot2 and directlabels packages
}

\value{
returns a ggplot graph object
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Simon Brickman
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

res<-data.frame(YOA=rep.int(c(2006,2007),c(8,4)),dev=c(1:8,1:4),
                Loss_Ratio=c(5,10,20,40,60,70,75,78,7,12,30,50,20,40,70,80,78,75,75,73,39,60,70,75),
                Type=rep.int(c("Incurred","Blend"),c(12,12)))
plotchart(res,fac="Type",vals="Loss_Ratio",x="dev",yl="Loss Ratio",
          leg="YOA",HeadTitle="Example Loss Ratios")


