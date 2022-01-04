emis = read.table("emissions.txt", header=T, sep="\t") 
set.seed(123)
cor(emis)
summary(emis)
colnames(emis)
emis$NOx
emis[,2]
View(emis)

hist(emis[,"NOx"],main="NOx", xlab="kg/cm^2")
b1 <- c(seq(min(emis$NOx), max(emis$NOx), length.out=10))
