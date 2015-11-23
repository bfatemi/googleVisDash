library(data.table)
library(googleVis)
library(RODBC)

#######################################################################
#   Read in Data and call it procs. Then make it a data.table because
#   data.table is much more efficient and easy to use
#######################################################################

dt <- as.data.table(procs) 

#######################################################################
#   Make col names user friendly and construct a column of class Date
#######################################################################

setnames(dt,
         old = names(dt),
         new = c("Category","Day","Month","Qtr","Year","Count"))

dt[,Date:=as.Date(paste0(Month," ",Day," ",Year),format = "%B %d %Y")]

setkey(dt,Date) #general best practice is to "key" your data.table

#######################################################################
#   replace text values in Qtr with Dates corresponding to the end of 
#   the quarter
#######################################################################

#create dictionary to match text with dates
dict <- data.table(Qtr=c("Q1","Q2","Q3","Q4"),
                   Date=c("3/31","6/30","9/30","12/31"))

qtrDates <- paste0(dict[match(dt[,Qtr],dict[,Qtr]), Date],"/",dt[,Year])
qtrDates <- as.Date(qtrDates,format="%m/%d/%Y")
dt[,Qtr:=qtrDates]

#######################################################################
#   Create the CALENDAR CHARTS - timeline heat map showing procedure 
#   counts (sum accross all procedure types)
#######################################################################

#all procs by date
all <- dt[,.(ProcedureCount = sum(Count)),by=Date]

Cal <- gvisCalendar(all,
                    datevar="Date",
                    numvar = "ProcedureCount",
                    options=list(width=1300,height=3000))

plot(Cal)

#clean up the google default footer and captions
Cal$html$caption=NULL
Cal$html$footer="</body></html>"

#######################################################################
#   Create the MOTION CHART - visualizing the trend in procedure counts
#   over 2014 (incomplete data for 2015 so trends would be off), 
#   shown per procedure (not aggregated like the cal chart)
#######################################################################

#sum proc counts by category, quarter, and for 2014 only
motion.dt <- dt[Year==2014,.(ProcedureCount = sum(Count)), by=.(Category,Qtr)]

#clean up
motion.dt[,Year:=NULL]

#save chart
Motion <- gvisMotionChart(motion.dt,
                          idvar="Category", 
                          timevar="Qtr",
                          options = list(title="By Procedure and Quarter",
                                         gvis.editor="Customize"))


#######################################################################
#   Create the MOTION CHART - another motion chart
#######################################################################

View(dt)

mot.dt <- dt[,.(ProcedureCount = sum(Count)),by=.(Category,Year)]

#save chart
Motion <- gvisMotionChart(mot.dt,
                          idvar="Category", 
                          timevar="Year",
                          options = list(title="By Procedure and Year",
                                         gvis.editor="Customize"))
plot(Motion)

#######################################################################
#   Complement the Motion chart with a googleVis table showing the data
#######################################################################

table <- gvisTable(mot.dt,
                   options = list(page='enable',height=500))


#######################################################################
#   Create the html page that will display the three charts we made here
#######################################################################

a <- gvisMerge(Motion,table,horizontal=T)
b <- gvisMerge(Cal,a,horizontal=F,tableOptions = "border = \"1\"")
plot(b)


writeLines(code,con = "../Documents/3proccount.html")
print(b,file = "../Documents/check.html")

