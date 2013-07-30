#' Automated Reporting
#' ====================
#' 
#' R in Insurance
#' 
#' July 15, 2013
#' 
#' 
#' * Simon Brickman
#'     - [simon.brickman@beazley.com][simon]
#' * Adam L. Rich
#'     - [adam.rich@beazley.com][adam]
#'     - [http://adamleerich.com][adam-www]
#' 
#'   [adam]: emailto:adam.rich@beazley.com
#'   [simon]: emailto:simon.brickman@beazley.com
#'   [adam-www]: http://adamleerich.com
#'     



#+ setup, include = FALSE
require(knitr)
require(directlabels)
require(ggplot2)
require(reshape2)
require(data.table)
require(scales)


# The following tells knitr to NOT reformat our R code
opts_chunk$set(tidy = FALSE)



# tools::Rd2HTML is the internal function 
# used to build HTML files from Rd files
 
tools::Rd2HTML(
  './help-files/RL2013_MAChart.Rd', 
  './help-files/RL2013_MAChart.html',
  package = 'R in Insurance: Automated Reporting',
  no_links = TRUE
)

tools::Rd2HTML(
  './help-files/RL2013_plotchart.Rd', 
  './help-files/RL2013_plotchart.html',
  package = 'R in Insurance: Automated Reporting',
  no_links = TRUE
)



#' # Cooking with Computers
#' 
#' ![](./Cheesecake.jpg)
#' 
#' 
#' 
#' # Code > Report
#' 
#' >- We like to analyze data
#' >- We are required to share those analyses with people who don't read code
#' >- We don't like re-writing stuff
#' >- What if we could write R scripts and automagically print reports?
#' >- No LaTeX! No Sweave! No code chunks!
#' 
#' 
#' 
#' # Generalized Charts
#' 
#' - We produce a lot of charts
#' - They must be consistently formatted
#' - We write wrappers around `ggplot2` functions to standardize our plots and encapsulate repeated code
#' - `directlabels`
#' 
#' 
#' 
#' # I Am an R-Script
#' 
#' - Prove it ([R script][this-script])
#' - `knitr`
#' - `spin`
#' - A lot more to come in this area
#'     * Shameless self promotion
#'     * http://adamleerich.com
#' 
#'   [this-script]: ./Automated_Reporting.R
#'   
#' 
#' 
#' # Enough Fluff! More Charts!
#' 
#' - Our best advances have been in chart automation/standardization
#' - Our code is not all-purpose (on purpose)
#' - But, it is general enough for our most time-sensitive work
#' - We don't always have time to fiddle with all the details
#' - And, people (management, underwriters) want to see the same thing over and over again anyway
#'
#'
#'
#' # `BZLYCharts`
#' 
#' The functions we are sharing are part of a 
#' not yet fully developed, internal use only package
#' called `BZLYCharts`.
#' 
#' Not all code needs to be on CRAN.
#' We encourage everyone to have their own packages.
#' 
#' For now we will just load the functions.
source("RLondon2013ChartFunctions.r")
ls(1)



#' # Contents
#' 
#' The two functions we want to share are
#' 
#' 1. plotchart ([help file][plotchart-Rd])
#' 2. MAChart ([help file][MAChart-Rd])
#'   
#'   [plotchart-Rd]: ./help-files/RL2013_plotchart.html
#'   [MAChart-Rd]: ./help-files/RL2013_MAChart.html
#'



#' # Simple Example
#' 
#' We start with simple example with a small data frame.
simple <- data.frame(
  YOA = rep.int(c(2006, 2007), c(8, 4)),
  dev = c(1:8, 1:4),
  Loss_Ratio = c( 5, 10, 20, 40, 60, 70, 75, 78,
                  7, 12, 30, 50, 20, 40, 70, 80,
                 78, 75, 75, 73, 39, 60, 70, 75),
  Type = rep.int(c("Paid","Incurred"), c(12, 12))
)

simple


#' # "Tabular" Triangle
#' We can see this as a triangle using `tapply`.
with(simple, tapply(Loss_Ratio, list(YOA, dev, Type), I))



#' # "Graphical" Triangle
#' Now use this one-line statement to see this information graphically
plotchart(
  df = simple, fac = "Type", vals = "Loss_Ratio",
  x = "dev", yl = "LR(%)", leg = "YOA",
  HeadTitle = "Example Loss Ratios"
)



#' # What is `plotchart` Doing?
#' 
#' It takes a dataframe and creates a bunch of charts
#' 
#' Argument     | Purpose
#' -------------|---------------------------------
#' df           | The dataframe (not a data.table!)
#' fac          | Factor to split charts
#' vals         | The y-axis value
#' x            | The x-axis value
#' yl           | Label for y-axis
#' leg          | Further split charts into lines
#' HeadTitle    | Title stub
#' `...`        | Of course, there are others
#' 
#' [`plotchart` help file!][plotchart-Rd]
#' 



#' # Switch It Up
#' 
#' `plotchart` was built to make it easy to make a lot of charts
#' and to be able to quickly change how the data is laid out
#' without having to copy chunks of code or re-formatting.
#' For example, it is very easy to change the x- and y-axes.
#'
plotchart(
  simple, fac = "YOA", vals = "Loss_Ratio",
  x = "dev", yl = "LR(%)", leg = "Type",
  HeadTitle = "Example Loss Ratios"
)



#' # More Complicated Example
#' 
#' Let's try a more involved dataset of individual claims.
#+ claims-rdata
load("./data/PreparedData.RData")
head(claims.agg)
str(claims.agg)
levels(claims.agg$Type)



#' # YOAs for each Measure
#' 
plotchart(
  claims.agg, fac = "Type", vals = "valsum",
  x = "dev", yl = "Cost", leg = "YOA",
  HeadTitle = "Claim Development"
)



#' # YOAs for each Measure: `usedirectlabel = FALSE`
#' 
#' The labelling looks too busy 
#' as there are too many lines, 
#' so use of standard key is preferred
plotchart(
  claims.agg, fac = "Type", vals = "valsum",
  x = "dev", yl = "Cost", leg = "YOA",
  HeadTitle = "Claim Development",
  usedirectlabel = FALSE
)



#' # Measures for each YOA
#' 
#' Now let's switch legend and factors to see how measures compare
plotchart(
  claims.agg, fac="YOA", vals = "valsum",
  x = "dev", yl = "Cost", leg = "Type",
  HeadTitle = "Claim Development"
)



#' # Output to PDF
#' 
#' If we want to send these charts out we can simply specify pdf output.
#' (We plan to offer HTML/PNG indexed and individual output in the future.)
pdfOn <- TRUE
if(pdfOn == TRUE) pdf(file = "Example1.pdf")
plotchart(
  claims.agg, fac = "YOA", vals = "valsum",
  x = "dev", yl = "Cost", leg = "Type",
  HeadTitle = "Claim Development"
)
if(pdfOn == TRUE) dev.off()



#' # Frequency Graphs
#' 
#' We can also look at claim numbers - 
#' and if we had premium or an exposure measure
#' the numbers could be turned into a frequency measure.
#' 
#' It doesn't make any sense to show numbers for all measures 
#' as these will be identical, so run without factor.
#'
#'
#'



#' # Frequency Graphs (cont'd)
plotchart(
  claims.agg, fac = "", vals = "valnum", 
  x = "dev", yl = "Number", leg = "YOA",
  HeadTitle = "Claim Development",
  usedirectlabel = FALSE
)



#+ include = FALSE
# # It may be interesting to see development of average claim separated
# # between closed and open cases for incurred only.
# setkey(RES, YOA, dev, Type, ClaimStatus)
# RES3 <- RES[, list(valsum = sum(val), valnum = length(val)), key(RES)]
# RES3 <- RES3[, AvgClaim:=valsum/valnum]  # add new column
# res3 <- as.data.frame(RES3[Type == "IncurredUSD" & YOA < 2011 & ClaimStatus == "C"])  
# 
# plotchart(
#   res3, fac = "", vals = "AvgClaim",
#   x = "dev", yl = "Average Claim", leg = "YOA",
#   HeadTitle = "Average Claim Amount for Closed Cases",
#   usedirectlabel = FALSE
# )



#+ include = FALSE
# # If we limit years we can effectively zoom in years of interest
# res3 <- as.data.frame(
#   RES3[Type == "IncurredUSD" & YOA < 2011 & YOA > 2006 & ClaimStatus == "C"]
# )  
# 
# plotchart(
#   res3, fac="", vals = "AvgClaim",
#   x = "dev", yl = "Average Claim", leg = "YOA",
#   HeadTitle = "Average Claim Amount for Closed Cases",
#   usedirectlabel = FALSE
# )



#' # Comparisons to Ultimate
#' Let's compare the measures 
#' against the proposed ultimate values (called HELD).
plotchart(
  as.data.frame(CLAIMS.WH), fac = "YOA", vals = "valsum",
  x = "dev", yl = "Claim Cost", leg = "Type",
  HeadTitle = "Comparison of Measures",
  usedirectlabel = FALSE
)



#+ include = FALSE
# #' # Ultimate Loss Ratios
# #' Excuse us while we get the data ready
# #' 
# setnames(HELD, "valsum", "ult")
# setkey(CLAIMS.WH, YOA)
# CLAIMS.WH <- HELD[CLAIMS.WH]
# CLAIMS.WH <- CLAIMS.WH[, Development:=valsum/ult]
# CLAIMS.WH
# 
# 
# 
# #' # Ultimate Loss Ratios (cont'd)
# plotchart(
#   as.data.frame(CLAIMS.WH[!Type == 'Held']), fac = "Type", 
#   vals = "Development", yl = "Ratio to Ultimate", 
#   leg = "YOA", HeadTitle = "Loss Development",
#   yScaleMin = 100, scale.by = 100,
#   usedirectlabel = FALSE
# )
# 
# 
# 
# #' # Ultimate Loss Ratios (cont'd)
# plotchart(
#   as.data.frame(CLAIMS.WH), fac = "YOA", vals = "Development",
#   yl = "Ratio to Ultimate", leg = "Type",
#   HeadTitle = "Loss Development",
#   yScaleMin = 100, scale.by = 100,
#   usedirectlabel = FALSE
# )



#' # Moving Average Charts
#' Another function like `plotchart` is `MAChart`.
#' The following examples show duration to close
#' and its change over time.
head(DTC)
head(DTC.IND)



#' # Moving Average Charts (Cont'd)
MAChart(
  df = as.data.frame(DTC), ywt = 2, ycol = 3, xcol = 4,
  main = "Duration to close (12m moving average)",
  xlab = "Closed Year+Month",
  ylab = "Average Duration (days)",
  xstep = 3, xRotate = T
)



#' # Moving Average Charts (cont'd)
MAChart(
  df = as.data.frame(DTC.IND), ywt = 3, ycol = 4, xcol = 5,
  main = "Duration to close (12m MA) by SeverityIndicator",
  xlab = "Closed Year+Month",
  ylab = "Average Duration (days)",
  xstep = 3,
  leg="ClaimSeverityInd",
  xRotate = T
)



#' # The End
#' 
#' The data and all this code will be made available to conference participants.
#' 
#' Thanks!
#' 
#' * Simon Brickman
#'     - [simon.brickman@beazley.com][simon]
#' * Adam L. Rich
#'     - [adam.rich@beazley.com][adam]
#'     - [http://adamleerich.com][adam-www]
#'     



#+ include = FALSE
# Pandoc markdown
# http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html
