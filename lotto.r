alc <- read.csv("C:/Users/carandangc/Documents/Winter 2017 BIA/INTF 4001 - Special Topics (Bill) - Notes/carlo_data_2016_2017.csv", header=FALSE)
alc2 <- c(alc$V2,alc$V3,alc$V4,alc$V5,alc$V6,alc$V7,alc$V8)
hist(alc2, col="BLUE", ylab = "Frequency",las=1, xlim=c(1,49),xlab = "VALUES", breaks=49)
?hist
#scatter plot
plot(V2~V8, alc)
with(alc,text(V2~V8, labels=V2,pos=4,cex=.3))
#Normalization will create a level plain field, the avareage for each variable becomes 0 and std becomes 1 
Z <-alc[, -c(1,1)]
#calculating mean for rows = 1 and columns= 2 
M <-  apply(Z,2,mean)
SD <- apply(Z,2,sd)
Z <- scale(Z,M,SD)
Z
#calculating Eculidean distance
distance <- dist(Z)
print(distance,digits=3)
# Create new column filled with default colour
alc$Colour="black"
# Set new column values to appropriate colours
alc$Colour[alc$V2>=3]="red"
alc$Colour[alc$V8<=1]="blue"
# Plot all points at once, using newly generated colours
plot(alc$V2,alc$V8,  col=alc$Colour, ylim=c(0,5))

#cluster dendogram with complete linkage
hc.c <- hclust(distance)
plot(hc.c)
plot(hc.c, hang= -1)
#Cluster dendogram average
hc.a <- hclust(distance,method="average")
plot(hc.a, hang=-1)
#cluster member
lottery.c <- cutree(hc.c,3)
lottery.a <- cutree(hc.a,3)
