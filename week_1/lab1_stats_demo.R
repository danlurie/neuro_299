# Set working directory.  
# This allows the script to access files located in this directory.
setwd('Dropbox/Classes/NEURO299/MasterDirectory/Week_01/01_teaching_materials/')

# Load data from filename.
df <- data.frame(read.csv('lab1_stats_demo.csv'))

# Alternate: Load data by selecting file from browser.
#   > df <- data.frame(read.csv(file.choose()))

# Check out df.
df
colnames(df)
# Three columns are identical.
# This implies that they have the same statistics (e.g. mean, sd, etc.).
df$x1 == df$x2
df$x1 == df$x3

# Calculate mean of unique columns.
?mean
mean(df$x1)
mean(df$x4)
mean(df$y1)
mean(df$y2)
mean(df$y3)
mean(df$y4)

# Calculate standard deviation of unique columns.
?sd
sd(df$x1)
sd(df$x4)
sd(df$y1)
sd(df$y2)
sd(df$y3)
sd(df$y4)

# Calculate Pearson's correlation between x and y values.
?cor
cor(df$x1, df$y1, method='pearson')
cor(df$x2, df$y2, method='pearson')
cor(df$x3, df$y3, method='pearson')
cor(df$x4, df$y4, method='pearson')

# Make function to plot both histogram and boxplot of each vector.
make.plots <- function(x){
  par(mfrow=c(2,1))
  hist(x, main='Histogram')
  boxplot(x, main='Boxplot')
}

make.plots(df$x1)
make.plots(df$x4)
make.plots(df$y1)
make.plots(df$y2)
make.plots(df$y3)
make.plots(df$y4)

# Scatterplot.
par(mfrow=c(2,3))
plot(df$x1, main='df$x1')
plot(df$x4, main='df$x4')
plot(df$y1, main='df$y1')
plot(df$y2, main='df$y2')
plot(df$y3, main='df$y3')
plot(df$y4, main='df$y4')


# Other statistics.
# max(), min(), median(), sum(), var(), mad(), table()
summary(df)

# Write a geometric mean function.
# This is a measure of central tendency.
# It is useful for geometric series (e.g. c(2,4,8,16,32,64))
# and power series (e.g. c(3,9,27,81,243,729)).
gm.mean <- function(x){
  exp(sum(log(x)) / length(x))
}

gm.mean(df$x1)
gm.mean(df$x4)
gm.mean(df$y1)
gm.mean(df$y2)
gm.mean(df$y3)
gm.mean(df$y4)


# Alternate: Import an outside package with this function.
#   > install.packages('psych')
#   > library('psych')
#   > geometric.mean(df$x1)


# Find the skewness and kurtosis of data.
#   > install.packages('e1071')
#   > library('e1071')
#   > skewness(df$x1)
#   > kurtosis(df$x1)


# Plotting error bars with ggplot2.
#  > install.packages('ggplot2')
library('ggplot2')
par(mfrow=c(1,1))

# e.g. Estimation of mean.
# Generate random Gaussian data.
dat1 <- rnorm(20, mean=0, sd=1)
dat2 <- rnorm(1000, mean=0, sd=1)
mean.dat <- c(mean(dat1), mean(dat2))

# Use of standard deviation.
sd.dat <- c(sd(dat1), sd(dat2))
qplot(c(1,2), mean.dat) +
  geom_errorbar(aes(c(1,2),ymin=mean.dat-sd.dat, ymax=mean.dat+sd.dat), width=0.25)

# Use of standard error.
se <- function(x){
  sd(x)/sqrt(length(x))
}
se.dat <- c(se(dat1), se(dat2))
qplot(c(1,2), mean.dat) +
  geom_errorbar(aes(c(1,2),ymin=mean.dat-se.dat, ymax=mean.dat+se.dat), width=0.25)

# e.g. Estimation of median.
median.error <- function(x){
  median(x)/sqrt(length(x))
}
med.x <- c(median(df$x1), median(df$x2), median(df$x3), median(df$x4))
med.y <- c(median(df$y1), median(df$y2), median(df$y3), median(df$y4))
med.error.x <- c(median.error(df$x1), median.error(df$x2), median.error(df$x3), median.error(df$x4))
med.error.y <- c(median.error(df$y1), median.error(df$y2), median.error(df$y3), median.error(df$y4))
qplot(c(1,2,3,4), med.x) +
  geom_errorbar(aes(c(1,2,3,4),ymin=med.x-med.error.x, ymax=med.x+med.error.x), width=0.25)
qplot(c(1,2,3,4), med.y) +
  geom_errorbar(aes(c(1,2,3,4),ymin=med.y-med.error.y, ymax=med.y+med.error.y), width=0.25)
