
#author narendra
#date 02/25/16
#description time series pridication of balance 



#install.packages("RWeka")
#install.packages("xts")
#install.packages("forecast")
#install.packages("xlsx")
#install.packages("date")
#install.packages("readxl")
library(xts)
library(RWeka)
library(forecast)
#WPM("refresh-cache") # refresing chace 
#WPM("list-packages", "installed") # list of installed packages 
#WPM("list-packages", "available") # list of available packages  
#WPM("install-package", "timeseriesForecasting") # installing time series forcasting package 


library(xlsx)
library(RWeka)
library(readxl)
setwd("/Users/naru/Documents/RWorkshop/personalFinanceSaving/data") # setting path to data directory
data <- read.xlsx2("Sample data set 5 Final.xlsx", sheetName = 2,header=TRUE) # reading xlsx file sheet 2 as the fromat defined in file 

#head(data)
#data <- read.table("Sample data set 4 Final.xlsx",2)
#data <- read_excel("Sample data set 6 Final.xlsx", 2) # reading xlsx file sheet 2 as the fromat defined 


###### applying all three assumptions 
newdata = data[!apply(is.na(data) | data == "", 1, all), ]  # removing empty rows from excell sheet 
newdata
#head(newdata)
newdata = aggregate(newdata$Balance,by=list(date=newdata$Post.Date), FUN = tail, n = 1)  # getting end balance of each day 
newdata
#head(newdata)
newdata$date <- as.Date(as.numeric(as.character(newdata$date)), origin="1899-12-30") # converting into real date 
#class(newdata$date)
newdata
#head(newdata)

alldates = seq(min(newdata$date), max(newdata$date), 1) # seq of dates from start to end with one day gape



dates0 = alldates[!(alldates %in% newdata$date)]   # filter out timestamps that are already present
data0 = data.frame(date = dates0, x = NA_real_)  # construct a `data.frame` to append with missing dates


newdata = rbind(newdata, data0) # append this `data.frame` and resort in time:
newdata = newdata[order(newdata$date),]

newdata$x <-as.character(newdata$x)


# fill the values 
current = NA_real_
newdata$x = sapply(newdata$x, function(x) { 
  current <<- ifelse(is.na(x), current, x); current }) # assiging blace to newlay added dates from the last day available balance 


#######end

#######getting modified data into output into CSV file # not useful 
write.csv(newdata, file="balance.csv") ## not useful #these parts (tagged as not useful) are only to understand the procedure 
newdata

######end

####### ploting data to see patterns 
plot(newdata$date, newdata$x, type = "l")   ## not useful 

#######end


########Normalized Data BAlance columun between 0 to 1 
normdata<-newdata
x <-as.numeric(newdata$x)
normalized = (x-min(x))/(max(x)-min(x))

normdata$x <- normalized  # data with normlized balace 0 to 1

########end


#######xts dataframe (data in time series dataframe )

xtsdata <- xts(as.numeric(newdata$x), newdata$date)  # converting data frame into time series format ( inbuilt in R ) 
xtsdata    # timeseries without normlized balance 

#normlized data
normxtsdata <- xts(as.numeric(normdata$x), normdata$date)
normxtsdata  # time series with normlized balance 


#######end

########pridiction using Neural network autoregression  ## not useful 
par(mfrow=c(1,2))  # not useful 


xtsdata
fit <- nnetar(xtsdata)
f <- forecast(fit,h=30)    # not useful 
plot(f)
summary(f)

#normlized normxtsdata
fit <- nnetar(normxtsdata)
f <- forecast(fit,h=30)  #not useful 
plot(f)
summary(f)

#####end


#######foriour transformation of balance data into frequecy index # to do analysis with knowan stastistics used for Fourious transformation 
library(ggplot2)

f <- data.frame(coef = fft(normdata$x), freqindex = c(1:91))   # norm data # use this one if you want to go with normlized balance 
#f <- data.frame(coef = fft(as.numeric(newdata$x)), freqindex = c(1:91))   # original data # use this one if you want to without normlizing balance 
#plot(f)
qplot(freqindex, Mod(coef), data = f[1:91,], geom = "line") # plot line graph # 


#f[Mod(f$coef) > 3 & f$freqindex < 53, "freqindex"] - 1 ## not useful 


peaks <- Mod(f$coef) > 3  #  condtion if the frequecy is grater then the mode 
ffilt <- f
ffilt[!peaks, "coef"] <- 0  # put value zero all those  that have peak value false

ffilt <- data.frame(index=ffilt$freqindex, value=Re(fft(ffilt$coef, inverse=TRUE))/91, type=rep("filtered", times=91)) # getting the real value of coeficients 
ffilt <- rbind(ffilt, data.frame(index=seq(1:91), value=newdata$x, type=rep("original", times=91)))


##########end 
midindex <- ceiling((length(f$coef)-1)/ 2) + 1  # getting half of data points 
peakind <- f[abs(f$coef) > 3 & f$freqindex > 1 & f$freqindex < midindex,] # selecting data points  with given conditions 

lindex <- length(f$coef)

lowerind <- 1  # define lower bind 

subsignals <- lapply(c(peakind$freqindex, midindex+1), function(x){  # getting standalon singlnals or patterns from data or balance 
  upperind <- x # define upperbound 
  fsub <- f  # fouriour transformation data 
  notnullind <- ((fsub$freqindex >= lowerind
                  & fsub$freqindex < upperind)
                 |
                   (fsub$freqindex >  (lindex - upperind + 2)
                    & fsub$freqindex <= (lindex - lowerind + 2)))   # applying condition 
  fsub[!notnullind,"coef"] <- 0  # 0 to false values 
  lowerind <<- upperind
  Re(fft(fsub$coef, inverse=TRUE)/length(fsub$coef))  # getting real values from complex numbers 
})




########
library(grid)  # not useful , use for plotting diffrent signals 

grid.newpage()
pushViewport(viewport(layout=grid.layout(4,2)))

vplayout <- function(x,y)
  viewport(layout.pos.row = x, layout.pos.col = y)

psig <- function(x, y, z){  # giving each plot a diffrent part 
  h <- data.frame(index = c(1:length(subsignals[[x]])),
                  orders = subsignals[[x]])
  lab <- paste("Subseries ", as.character(x), sep="")
  print(qplot(index, orders, data = h, geom = "line", main=lab), vp = vplayout(y,z))
  TRUE
}

par(mfrow=c(1,6))
psig(1,1,1); 
psig(2,1,2);
psig(3,2,1); 
psig(4,2,2); 
psig(5,3,1); 
psig(6,3,2)


#number of hidden neurons

nn.sizes <- c(4,2,3,3,3,2,2,2)
#nn.sizes <- c(5,3,4,4,4,3,3,3)  # its neuron layer, you define it based on  
######
numofsubs <- length(subsignals)
twindow <- 30 # number of time window # altentive to define 

offsettedsubdfs <- lapply(1:numofsubs, function(x){ # itrate over signals 
  singleoffsets <- lapply(0:(twindow-1), function(y){   # apply on each signal 
    subsignals[[x]][(twindow-y):(length(subsignals[[x]])-y-1)]
  })
  a <- Reduce(cbind, singleoffsets)   # adding each dataframe by column from signals 
  names <- lapply(1:twindow, function(y){paste("TS", as.character(x), "_", as.character(y), sep = "")}) # givings column names 
  b <- as.data.frame(a)
  colnames(b) <- names
  b
})

####
sample.number <- length(offsettedsubdfs[[1]][,1])

######
#install.packages("nnet")
library(nnet)
#the neural networks

nns <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  nn <- nnet(offsettedsubdfs[[i]][1:(sample.number),], #the training samples
             
             subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], #the output
             
             #corresponding to the training samples
             
             size=nn.sizes[i], #number of neurons
             
             maxit = 1000, #number of maximum iteration
             
             linout = TRUE) #the neuron in the output layer should be linear
  
  #the result of the trained networks should be plotted
  
  plot(subsignals[[i]][(twindow+1):(length(subsignals[[i]]))], type="l")  # not useful can be removed
  
  lines(nn$fitted.values,type="l",col="red")
  
  nn
  
})


#pridiction 

number.of.predict <- 30



#long term prediction

long.predictions <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  prediction <- vector(length=number.of.predict, mode="numeric")
  
  #initial input
  
  input <- offsettedsubdfs[[i]][sample.number,]
  
  for (j in 1 : number.of.predict)
    
  {
    
    prediction[j] <- predict(nns[[i]], input)
    
    input <- c(prediction[j],input[1:(length(input)-1)])
    
  }
  
  #we want to plot the prediction
  
  plot(c(nns[[i]]$fitted.values,prediction), type="l",col="red")
  
  lines(subsignals[[i]][(twindow+1):length(subsignals[[i]])])
  
  prediction

})

long.predictions


normPridiction<- rowSums(data.frame(long.predictions))


finalpridiction <-  (normPridiction *(max(x)-min(x)) ) +min(x)
plot(finalpridiction)

#########end 




###########average method for forcasting 


#data by months 
library(dplyr) 
library(reshape2)
newdata$month <- factor(format(newdata$date, "%B"),  levels = month.name) # getting moths name from data
newdata$month <- as.character(newdata$month)
 # geting each months
months <- unique(newdata$month)
month1 <- as.numeric(subset(newdata$x,newdata$month == as.character(months[1])))  # first month 
month2 <- as.numeric(subset(newdata$x,newdata$month == as.character(months[2])))  #second month
month3 <- as.numeric(subset(newdata$x,newdata$month == as.character(months[3])))  # third month

#list all months 
monthdata <- list(month1,month2,month3)
                    
#maximum number of rows
max.rows <- max(length(month1), length(month2), length(month3))
# Stack shorter columns with NA at the end
monthdata <- sapply( monthdata , function(x) c( x , rep( NA , max.rows - length(x) ) ) ) 

#setting a column name at run time 
monthdata<- setNames( do.call( data.frame , list(monthdata) ) , paste0("month" , 1:3 ) )


monthdata$average <- rowMeans(monthdata)


############end 

####### average of diffrence of change


monthdata$change1 <- monthdata$month2 - monthdata$month1 
monthdata$change2 <- monthdata$month3 - monthdata$month2 


monthdata$changeAverage <- (monthdata$change1+monthdata$change2)/2


##########end 


########pridiction using average method 

monthdata$pridiction <- monthdata$changeAverage + monthdata$average

plot(monthdata)
#######end  


######final results 
finalaveragedata<- monthdata[complete.cases(monthdata),]

balancePridiction <- data.frame(averageMethod=finalaveragedata$pridiction,mlpPridiction = finalpridiction )


balancePridiction$average <- rowMeans(balancePridiction)


pridictedValues <-balancePridiction$average #result pridiction 

