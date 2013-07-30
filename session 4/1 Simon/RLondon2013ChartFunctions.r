
  
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
                          UseZeroYaxis=TRUE) {
  
  
  # Author: Simon Brickman
  # Date:   July 24, 2012
  # Description:
  #
  #   Functions for smart plotting 
  #     triangle-type Pre Peer-like charts
  #
  
  # set up function to treat this as factor only and loop through it
  # if you want to use column names as looping then trick is to use melt function
  # melt(df,measure.vars=c("ML","Pes"))
  #
   #
  # Examples
  # plot development to ultimate  
  #   pdf(file = "EPL OpenMarket NonBlock RAG Report 2.pdf")
  #   plotchart(res,HeadTitle="EPL Premium Frequency")
  #   plotchart(res,vals="FREQ_FTE_100K",yl="Claims per 000 FTE",,HeadTitle="EPL HeadCount Frequency")
  #   dev.off()
  
  
  #plotchart(temp,fac="YOA",vals="Development",x="Development_Quarter",yl="Development in %",leg="Type",
  #          HeadTitle=paste("COB: ", CB, " - Loss Ratio Development", sep = ""),yScaleMin=100)
  
  # plotchart(res,fac="Type",vals="ClosedPC",x="dev",yl="Closed",
  #           leg="yoa",HeadTitle="EPL Closed%")
  
  #  plotchart(temp,fac="",vals="CRelease",x="NStep",yl="Release from Initial Blend",
  #             leg="OQ",HeadTitle="Release from Initial Blend",scale.by=100)
   
  
  # [October 10, 2012 ALR]
  #
  #   Plots chart(s) for df
  #   Will return one or many, depending on fac.  
  #
  #   Arguments:
  #     df          The data
  #     fac         The name of the factor column in df
  #                 Each factor level will give a different chart
  #                 If you don't pass anything (NULL or blank)
  #                   you only get one chart
  #     vals        The values
  #     leg         The row in the triangle,
  #                 or the line on the chart
  #                 "YOA" for Pre-Peer charts
  #     x           The x-axis coordinate
  #                 "dev" for Pre-Peer charts
  #     xl          x-axis label
  #     yl          y-axis label
  #     HeadTitle   Chart title
  #
  
   #
  #amended 7/Apr/2013 
  # to deal with scaling with no factors (before scaling was applied after setting maximum)
  #
  #Note function requires legend to be defined (can deal with no factors but cannot yet
  #cope with no legend)
  
  #amended 29/Apr/2013
  # to allow non zero as mimimum yaxis value (if  above zero)
  # added xscale.by to operate as per scale.by
  
  library(ggplot2)
  library(directlabels)
  
  
  # Check scale.by 
  if (is.null(scale.by)) {
    if (left(toupper(yl), 10) == "LOSS RATIO") {
      scale.by <- 100
    } else {
      scale.by <- 1
    }
  }
  
  # Check xscale.by 
  if (is.null(xscale.by)) xscale.by <- 1
  
  
  if (fac == "" || is.null(fac)) {
    df <- df[, c(x, vals, leg)]
    names(df) <- c("x", "vals", "leg")
    
    if (!is.factor(df$leg)) df$leg <- factor(df$leg)
    
    # Apply scale.by
    df$vals <- df$vals * scale.by
    df$x <- df$x * xscale.by
    
    ylmax<-max(df$vals,na.rm=TRUE)
    if (is.null(yScaleMin)==FALSE) ylmax<-max(ylmax,yScaleMin)
    #fixed 7/apr/2013 to allow for scaling with no factors
    ylmin<-min(df$vals,na.rm=TRUE)
    if(UseZeroYaxis==TRUE) ylmin=min(0,ylmin)
    
      if (usedirectlabel==TRUE) {
        print(direct.label(qplot(
          x,
          vals, 
          data = df, 
          colour = leg,
          main = paste(HeadTitle),
          xlab = xl,
          ylab = yl,
          geom = c("point", "line"),
          ylim=c(ylmin,ylmax)  
        )))   
      }else{
        print(qplot(
          x,
          vals, 
          data = df, 
          colour = leg,
          main = paste(HeadTitle),
          xlab = xl,
          ylab = yl,
          geom = c("point", "line"),
          ylim=c(ylmin,ylmax)  
        ))               
        
      }
    
  } else {
    df <- df[, c(x, vals, leg, fac)]
    names(df) <- c("x", "vals", "leg", "fac")
    
    if (!is.factor(df$fac)) df$fac <- factor(df$fac)
    if (!is.factor(df$leg)) df$leg <- factor(df$leg)
    
    # Apply scale.by
    df$vals <- df$vals * scale.by
    df$x <- df$x * xscale.by  
    
    for (j in as.character(levels(df$fac))) { 
      
      tempC <- df[df$fac == j, ] 
      
      ylmax<-max(tempC$vals,na.rm=TRUE)
      if (is.null(yScaleMin)==FALSE) ylmax<-max(ylmax,yScaleMin)
      ylmin<-min(tempC$vals,na.rm=TRUE)
      if(UseZeroYaxis==TRUE) ylmin=min(0,ylmin)
      
      if(!dim(tempC)[1]==0) {
        if (usedirectlabel==TRUE) {
          print(direct.label(qplot(
            x,
            vals, 
            data = tempC, 
            colour = leg,
            main = paste(HeadTitle, "  ", fac, ": ", j, sep=""),
            xlab = xl,
            ylab = yl,
            geom = c("point", "line"),
            ylim=c(ylmin,ylmax)                                                       
          )))
        }else{
          print(qplot(
            x,
            vals, 
            data = tempC, 
            colour = leg,
            main = paste(HeadTitle, "  ", fac, ": ", j, sep=""),
            xlab = xl,
            ylab = yl,
            geom = c("point", "line"),
            ylim=c(ylmin,ylmax)                                                       
          ))
        }
        
      } #end if for printing plot (trapping for zero rows)
    } # end for
  } # end if
} # end function


toDate<-function(colnum,df) {
  # convert posixct date to date and handles GMT/BST
  as.Date(df[,colnum]+3600) }

monthsteps<-function(d1,d2) {
  #date:    21/feb/2013
  #purpose: create monthly vector from start to end month inclusive
  #version: 1.0 
  #author:  Simon Brickman
  
  stopifnot(is.Date(d1) & is.Date(d2))             #check we have dates
  n<-12*(year(d2)-year(d1))+(month(d2)-month(d1))  #calculate length inclusive of opening and closing months
  s<-(month(d1)+0:n)                               #create vector of this length
  i<-(s-1)%%12 +1                                  #months for each element
  j<-(s-1)%/%12                                    #years for each element
  v<-as.Date(paste(year(d1)+j,i,1,sep="-"))        #turn into dates
  return(v)
  
  #example
  #   d1<-as.Date("2007-03-31")
  #   d2<-as.Date("2009-02-01")
  #   monthsteps(d1,d2)
  #   
  
}



MAChart<-function(MAP=12,df,xcol=5,ycol=4,ywt=3,xstep=1,
                  xlab="Date",ylab="Duration",main="",leg="",fac="",xRotate=FALSE) {
  
  # Author: Simon Brickman
  # Date:   February 21 2013
  # Description:
  #
  #   creates moving average of ycol with term MAP
  #   against xcol
  #   option of using weights ywt
  #   assumes columns are contained in dataframe df, and that xcol is complete
  #   can use function monthsteps to create monthly steps
  #   Add option to see by factor eg size or category
  #
  # Amended 7th May 2013
  # added xRotate to allow axis to be rotated (default false)
  
    
  library(ggplot2)
  library(directlabels)
  library(scales)

  #set up xstep skip
  xstp<-paste(xstep," months",sep="")
  
  fn <- rep(1/MAP, MAP)
  
  #message in heading on claims involved
  if (is.na(ywt)) {
    totmsg<-""
  }else{
    totwt<-sum(df[,ywt])
    totmsg<-paste(" with total ",totwt,sep="")
  }
  
  
  if (fac == "" || is.null(fac)) {
    
      if (leg==""|| is.null(leg)){
        
        if (is.na(ywt)) {
          y1_lag <- filter(df[,ycol], fn, sides=1)
        }else{
          y1_lag <- filter(df[,ycol]*df[,ywt], fn, sides=1)/
            filter(df[,ywt], fn, sides=1)
        }
        
        y1_lag[is.na(y1_lag)]<-0
        tempC<-data.frame(x=df[-(1:(MAP-1)),xcol],y=y1_lag[-(1:(MAP-1))])
        
        m<-ggplot(tempC, aes(x, y))
        m<-m +geom_line() +geom_point()
        m<-m +scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks(xstp))
        if(xRotate==TRUE) m<-m+ theme(axis.text.x=element_text(angle=90, hjust=1))
        m<-m+xlab(xlab)+ylab(ylab)
        m<-m+ggtitle(paste(main,totmsg,sep=""))
        print(m)   #cannot do direct label as no legend to separate out
        
      }else{
        if (!is.factor(df[,leg])) df[,leg] <- factor(df[,leg]) 
        dfc<-NULL
        for (j in as.character(levels(df[,leg]))) { 
          tempC <- df[df[,leg] == j, ] 
          if (is.na(ywt)) {
            y1_lag <- filter(tempC[,ycol], fn, sides=1)
          }else{
            y1_lag <- filter(tempC[,ycol]*tempC[,ywt], fn, sides=1)/
              filter(tempC[,ywt], fn, sides=1)
          }
          
          y1_lag[is.na(y1_lag)]<-0
          
          tempC<-data.frame(x=tempC[-(1:(MAP-1)),xcol],y=y1_lag[-(1:(MAP-1))],leg=j)
          if (is.null(dfc)) {
            dfc<-tempC
          }else{  
            dfc<-rbind(dfc,tempC)
          }  
        }
        
        m<-ggplot(dfc, aes(x, y,col=leg,group=leg))
        m<-m +geom_line() +geom_point()
        m<-m +scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks(xstp))  
        if(xRotate==TRUE) m<-m+ theme(axis.text.x=element_text(angle=90, hjust=1))
        #brillaint way to skip on xaxis
        m<-m+xlab(xlab)+ylab(ylab)
        m<-m+ggtitle(paste(main,totmsg,sep=""))
        print(direct.label(m))
        
      }  #end if 
  
  } else {
        
    if (!is.factor(df$fac)) df$fac <- factor(df[,fac])
     
    for (j in as.character(levels(df$fac))) { 
      
      dfsub <- df[df$fac == j, ]   
      
      
      if (leg==""|| is.null(leg)){
        
        if (is.na(ywt)) {
          y1_lag <- filter(dfsub[,ycol], fn, sides=1)
        }else{
          y1_lag <- filter(dfsub[,ycol]*dfsub[,ywt], fn, sides=1)/
            filter(dfsub[,ywt], fn, sides=1)
        }
        
        y1_lag[is.na(y1_lag)]<-0
        tempC<-data.frame(x=dfsub[-(1:(MAP-1)),xcol],y=y1_lag[-(1:(MAP-1))])
        
        m<-ggplot(tempC, aes(x, y))
        m<-m +geom_line() +geom_point()
        m<-m +scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks(xstp))
        if(xRotate==TRUE) m<-m+ theme(axis.text.x=element_text(angle=90, hjust=1)) 
        m<-m+xlab(xlab)+ylab(ylab)  
        m<-m+ggtitle(paste(main, "  ", fac, ": ", j,totmsg,sep=""))
        print(m)   #cannot do direct label as no legend to separate out
        
      }else{
        if (!is.factor(dfsub[,leg])) dfsub[,leg] <- factor(dfsub[,leg]) 
        dfc<-NULL
        for (j in as.character(levels(dfsub[,leg]))) { 
          tempC <- dfsub[dfsub[,leg] == j, ] 
          if (is.na(ywt)) {
            y1_lag <- filter(tempC[,ycol], fn, sides=1)
          }else{
            y1_lag <- filter(tempC[,ycol]*tempC[,ywt], fn, sides=1)/
              filter(tempC[,ywt], fn, sides=1)
          }
          
          y1_lag[is.na(y1_lag)]<-0
          
          tempC<-data.frame(x=tempC[-(1:(MAP-1)),xcol],y=y1_lag[-(1:(MAP-1))],leg=j)
          if (is.null(dfsubc)) {
            dfc<-tempC
          }else{  
            dfc<-rbind(dfc,tempC)
          }  
        }
        
        m<-ggplot(dfc, aes(x, y,col=leg,group=leg))
        m<-m +geom_line() +geom_point()
        m<-m +scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks(xstp))  
        if(xRotate==TRUE) m<-m+ theme(axis.text.x=element_text(angle=90, hjust=1))
        #brillaint way to skip on xaxis
        m<-m+xlab(xlab)+ylab(ylab)
        m<-m+ggtitle(paste(main,totmsg,sep=""))
        print(direct.label(m))
        
      }  #end if 
    }
  }
  
}    #end function

right = function(string_input , no) {
  substr(string_input,nchar(string_input)+1-no,nchar(string_input))
}

left = function(string_input , no) {
  substr(string_input,1,no)
}

is.Date <- function (x) {
  if (class(x)[1] == "Date") 
    TRUE
  else FALSE
}

# Found in data.table!
# year <- function(d){
#   as.integer(format(d, '%Y'))
# }
# 
# month <- function(d) {
#   as.integer(format(d, '%m'))
# }
# 
