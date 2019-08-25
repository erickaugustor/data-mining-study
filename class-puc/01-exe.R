iris

#1:
data = iris
nrow(iris)

#2: 
ncol(data)

#3: 
max(data[,1])
min(data[,1])
mean(data[,1])

max(data[,2])
min(data[,2])
mean(data[,2])

max(data[,3])
min(data[,3])
mean(data[,3])

max(data[,4])
min(data[,4])
mean(data[,4])

#4:
plot(iris)

#5:
a<-data[,1]
hist(a)

b<-data[,2]
hist(b)

c<-data[,3]
hist(c)

d<-data[,4]
hist(d)

#6:
boxplot(data[,1]~data[,5], data)
boxplot(data[,2]~data[,5], data)
boxplot(data[,3]~data[,5], data)
boxplot(data[,4]~data[,5], data)
boxplot(iris)

#7:
#col=ifelse(y>0,"blue","red")