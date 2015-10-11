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

hist(df$Global_active_power,main="Global Active Power",col = "red",xlab = "Global Active Power (kilowatts)")
#dev.copy(png,"plot1.png")
#dev.off()

png(file="plot1.png")
hist(df$Global_active_power,main="Global Active Power",col = "red",xlab = "Global Active Power (kilowatts)")
dev.off()
