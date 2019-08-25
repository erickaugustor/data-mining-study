data()

library(dslabs)
# For isntall you should go to Tools and add the name of the package

data("murders")
murders

# Dataframe are equals to table in SQL
# with a lot of different values types
class(murders)

# Factor
# They have "class", every array should have their values based on the Levels.
murders$region

str(murders)


# Build vectors
pais <- c("Itália", "Canada", "Egito")
class(pais)
str(pais)

# Build Seq
seq(1, 20)
seq(1, 10, 2)


# Sort - Show the data
sort(murders$total)

# Order - Column indice
order(murders$total)

murders$total[5]
murders[5,]

# Show the max number of murders
max(murders$total)

# Show the indice of the max number of muder
# You can use with murders[5,] to know the data information
which.max(murders$total) 
murders[which.max(murders$total),]
murders$state[which.max(murders$total)]

which(max(murders$total) == murders$total)




# Exercise - 1
pop <- murders$population
pop
pop <- sort(pop)
pop[which.min(pop)]
min(pop)

# Exercise - 2
pop <- murders$population
pop
order(pop)
pop[order(pop)]
pop <- pop[order(pop)]
pop
min(pop)]

# Exercise - 3
pop <- murders$population
pop
pop <- sort(pop, decreasing = TRUE)
pop



tamanho <- c(1, 23, 45)
tamanho <- tamanho * 1.5
tamanho

# Create the indice
murders
murder_rate <- murders$total / murders$population * 100000
murder_rate

# Order by the new indice
murders$state[order(murder_rate)]


dst_murders <- murders
dst_murders$rate <- murder_rate


# Where rate < 0.71
ind <- murder_rate < 0.71
ind
dst_murders$state[ind]

# Find a specific value
west <- murders$region == 'West'
west
ind <- west && ind
dst_murders[ind,]

# Find with which
ind <- which(murders$state == "California")
ind
murders[ind,]

# Match
ind <- match(c("New York", "Florida", "Texas"), murders$state)
ind
dst_murders[ind,]

c("Boston", "Dakota", "Washington") %in% murders$state





# Exercise - 4
low <- dst_murders$rate < 1
low

# Exercise - 5
dst_murders[low,]

# Exercise - 6
nort <- dst_murders$region == 'Northeast'
dst_murders[nort,]
ind <- low & nort

dst_murders[ind,]
dst_murders[low & nort,]





x <- murders$population / 10^6
y <- murders$total
plot(x, y)



x <- with(murders, total/population * 100000)
hist(x)
x



boxplot(rate~region, data=dst_murders)



iris
nrow(iris)
ncol(iris)
