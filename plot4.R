##find skip
skip_find<-function(inputFile,pattern,header=T){
  con  <- file(inputFile, open = "r")
  if(header==T)
    readLines(con, n = 1, warn = FALSE)#header
  sk<-0
  while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    sk<-sk+1
    if(length(grep(pattern,oneLine))==1)
      break
  } 
  close(con)
  sk
}
##read data
df<-read.table(file="household_power_consumption.txt",header = F,sep=";",
               na.strings = "?",stringsAsFactors = F,
               skip=skip_find("household_power_consumption.txt","^1/2/2007"),
               nrows = 2*24*60)

colnames(df)<-colnames(read.table(file="household_power_consumption.txt",header = T,sep=";",na.strings = "?",stringsAsFactors = F,nrows = 1))

df$Date<-as.Date(df$Date,"%d/%m/%Y")
df$Time<-strptime(paste(df$Date,df$Time),"%Y-%m-%d %H:%M:%S")

par(mfcol=(c(2,2)),mar=c(4,4,2,2))
plot(df$Time,df$Global_active_power,type="l",xlab="",ylab = "Global Active Power (kilowatts)")
with(df,{
  plot(Time,Sub_metering_1,type="n",xlab = "",ylab="Energy sub metering")
  lines(Time,Sub_metering_1,col="grey")
  lines(Time,Sub_metering_2,col="red")
  lines(Time,Sub_metering_3,col="blue")
  legend("topright",lty=1,col=c("grey","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         bty="n",cex=0.40)
})
plot(df$Time,df$Voltage,type="l",xlab = "datetime",ylab="Voltage")
plot(df$Time,df$Global_reactive_power,type="l",xlab = "datetime",ylab="Global_reactive_power")

#dev.copy(png,"plot4.png")
#dev.off()

png(file="plot4.png")
par(mfcol=(c(2,2)))
hist(df$Global_active_power,main="Global Active Power",col = "red",xlab = "Global Active Power (kilowatts)")
with(df,{
  plot(Time,Sub_metering_1,type="n",xlab = "",ylab="Energy sub metering")
  lines(Time,Sub_metering_1,col="grey")
  lines(Time,Sub_metering_2,col="red")
  lines(Time,Sub_metering_3,col="blue")
legend("topright",lty=1,col=c("grey","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),bty="n")#,cex=0.40)
})
plot(df$Time,df$Voltage,type="l",xlab = "datetime",ylab="Voltage")
plot(df$Time,df$Global_reactive_power,type="l",xlab = "datetime",ylab="Global_reactive_power")
dev.off()