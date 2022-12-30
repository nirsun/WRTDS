interpolatedQ<-function(Daily) {
# save this file as fun_interpolatedQ.R

  #create a sequence of dates from the Daily File Directly
  Date <- seq(as.Date(head(Daily$Date,n=1)), as.Date(tail(Daily$Date,n=1)), by="days")
  DateVector <-data.frame(Date)
  # use the Vlookup function similar to excel via merge function
  # creating a subset of Daily dataframe: Daily[,c("Date","Q")]
  #keeping all the dates of DailyFilled_Q via: all.x = TRUE
  DailyFilledQ<-merge(DateVector,Daily[,c("Date","Q")],by="Date",all.x = TRUE)
  #locate the missing values: NA
  locate_nonNA<-which(!is.na(DailyFilledQ[,"Q"]))
  InterpolatedQ <- DailyFilledQ
  d<-0
  
  for (j in 1:(length(locate_nonNA)-1)){
    subs <- locate_nonNA[j+1] - locate_nonNA[j];
    if (subs ==1) {
      d<-d+1
    }
    else if (subs>1) {
      extent <- seq(locate_nonNA[j],locate_nonNA[j+1])
      InterpolatedQ[seq(min(extent),max(extent)),2] <- linspace(DailyFilledQ[(locate_nonNA[j]),2],DailyFilledQ[(locate_nonNA[j+1]),2],length(extent))
    }
  }
  return(InterpolatedQ)
}
