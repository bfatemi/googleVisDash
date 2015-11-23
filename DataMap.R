library(googleVis)
library(data.table)
library(boot)

######################################
#   Define function to build branch
######################################

get_branch <- function(levPar,levChild){
  n <- length(levChild)
  dt <- rbindlist(lapply(1:n,function(x){CJ(levPar[[x]],levChild[[x]])}))
  return(dt)
}

######################################
#   Define tree structure
######################################

#level 0
parent <- c("Categorized Data")

#level 1
categorized.data <- c("Complaints",
                      "Regulatory and Product Quality",
                      "System Generated Data",
                      "Simulator Generated Data",
                      "Surgeon Data",
                      "Contracts")

#level 2
sys.data <- c("Logs","Utilization","Service Records")
reg.data <- c("Complaints and Customer Feedback","Audits")
sim.data <- c("Usage","Performance")
surg.data <- c("Kinematic","Training","Demographic")
cont.data <- c("System Orders","Instrument Orders","Simulator Orders","Accessory Orders")
complaints.data <- c("Surveillance","Customer Service","dVSTAT","Clinical")

######################################
#   Build table
######################################

lev0 <- data.table(NA,"Categorized Data")

lev1 <- get_branch(parent,
                   list(categorized.data))

lev2 <- get_branch(categorized.data,
                   list(complaints.data,
                        reg.data,
                        sys.data,
                        sim.data,
                        surg.data,
                        cont.data))


dt.map <- rbind(lev0,lev1,lev2)
setnames(dt.map,c("Category","Dataset"))


# ######################
# #   SUBCATEGORIES
# ######################
# 
# sys <- c("logs","utilization","service records")
# sim <- c("usage","modules")
# surg <- c("kinematic","training scores")
# purch <- c("system orders","instruments orders","simulator orders","accessory orders")
# comp <- c("Post Market Surveillance","Customer Service","dVSTAT","Clinical")
# subcategory <- list(sys,sim,surg,purch,comp)
# 
# dt.map <- rbindlist()
# setnames(dt.map,c("Category","Subcategory"))
# 
# ######################
# #   DATASET
# ######################
# 
# dt.map[,Dataset:=paste("name",1:8)]

######################
#   RESTRICTION LEVEL
######################

#dt.map[,Restriction:=round(runif(.N,1,3),0)]
dt.map[,Restriction:=.I]

######################
#   SIZE OF DATA
######################

#dt.map[,SizeGB:=round(runif(.N,1,10000),0)]
dt.map[,RowCount:=1]

######################
#   EXPERTS
######################


dt.expert <- data.table(Expert=c("Michael J: 4234","Power Rangers: 6276","Batman: 2432","John D: 9855"))
setkey(dt.expert,Expert)

dt.map <- cbind(dt.map,
                dt.expert[round(runif(nrow(dt.map),1,.N),0)])

######################
#   PLOT GRAPH
######################

Tree <- gvisTreeMap(dt.map,
                    idvar = "Dataset", 
                    parentvar = "Category", 
                    colorvar = "Restriction", 
                    sizevar = "RowCount",
                    options=list(width=600, height=500,
                                 fontSize=16,
                                 minColor='#00CC66',
                                 midColor='#FFFF66',
                                 maxColor='#FF6666',
                                 headerHeight=20,
                                 fontColor='black',
                                 showScale=TRUE))

Org <- gvisOrgChart(dt.map, idvar = "Dataset", parentvar = "Category",
                    tipvar = "Expert",
                    options=list(width=200, height=200,
                                 size='large', allowCollapse=TRUE,
                                 gvis.editor="Customize"))


table <- gvisTable(dt.map[-1], options=list(gvis.editor="Customize"))

a <- gvisMerge(Tree,table,horizontal=T)
b <- gvisMerge(Org,a, horizontal=F) 
plot(b)

htmlcode <- unlist(b$html)
writeLines(htmlcode,con = "../Desktop/dash.html")
