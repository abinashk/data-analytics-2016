# load datasets nyt21 to nyt25
nyt21 <- read.csv("~/Projects/DataAnalytics2016/LabData/dds_ch2_nyt/nyt21.csv", header=TRUE)
nyt22 <- read.csv("~/Projects/DataAnalytics2016/LabData/dds_ch2_nyt/nyt22.csv", header=TRUE)
nyt23 <- read.csv("~/Projects/DataAnalytics2016/LabData/dds_ch2_nyt/nyt23.csv", header=TRUE)
nyt24 <- read.csv("~/Projects/DataAnalytics2016/LabData/dds_ch2_nyt/nyt24.csv", header=TRUE)
nyt25 <- read.csv("~/Projects/DataAnalytics2016/LabData/dds_ch2_nyt/nyt25.csv", header=TRUE)

par(mfrow=c(1,2))
# Impressions boxplot
boxplot(nyt21$Impressions, nyt22$Impressions, nyt23$Impressions, nyt24$Impressions, nyt25$Impressions, las=2, names = c("nyt21","nyt22","nyt23","nyt24","nyt25"), main="Impressions Boxplot")
# Age boxplot
boxplot(nyt21$Age, nyt22$Age, nyt23$Age, nyt24$Age, nyt25$Age, las=2, names = c("nyt21","nyt22","nyt23","nyt24","nyt25"), main="Age Boxplot")

# Histograms
par(mfrow=c(1,2))
hist(nyt21$Impressions, breaks=19, main="nyt21 Impressions Histogram")
hist(nyt21$Age, breaks=24, main="nyt21 Age Histogram")
par(mfrow=c(1,2))
hist(nyt22$Impressions, breaks=19, main="nyt22 Impressions Hist")
hist(nyt22$Age, breaks=24, main="nyt22 Age Hist")
par(mfrow=c(1,2))
hist(nyt23$Impressions, breaks=19, main="nyt23 Impressions Hist")
hist(nyt23$Age, breaks=24, main="nyt23 Age Hist")
par(mfrow=c(1,2))
hist(nyt24$Impressions, breaks=19, main="nyt24 Impressions Hist")
hist(nyt24$Age, breaks=24, main="nyt24 Age Hist")
par(mfrow=c(1,2))
hist(nyt25$Impressions, breaks=19, main="nyt25 Impressions Hist")
hist(nyt25$Age, breaks=24, main="nyt25 Age Hist")

# ECDF
par(mfrow=c(3,2))
plot(ecdf(nyt21$Impressions), do.points=FALSE, verticals=TRUE, main="nyt21 Impressions ECDF") 
plot(ecdf(nyt21$Age), do.points=FALSE, verticals=TRUE, main="nyt21 Age ECDF") 
plot(ecdf(nyt22$Impressions), do.points=FALSE, verticals=TRUE, main="nyt22 Impressions ECDF") 
plot(ecdf(nyt22$Age), do.points=FALSE, verticals=TRUE, main="nyt22 Age ECDF") 
plot(ecdf(nyt23$Impressions), do.points=FALSE, verticals=TRUE, main="nyt23 Impressions ECDF") 
plot(ecdf(nyt23$Age), do.points=FALSE, verticals=TRUE, main="nyt23 Age ECDF") 
par(mfrow=c(2,2))
plot(ecdf(nyt24$Impressions), do.points=FALSE, verticals=TRUE, main="nyt24 Impressions ECDF") 
plot(ecdf(nyt24$Age), do.points=FALSE, verticals=TRUE, main="nyt24 Age ECDF") 
plot(ecdf(nyt25$Impressions), do.points=FALSE, verticals=TRUE, main="nyt25 Impressions ECDF") 
plot(ecdf(nyt25$Age), do.points=FALSE, verticals=TRUE, main="nyt25 Age ECDF")

# Q-Q norm
par(mfrow=c(1,2))
qqnorm(nyt21$Impressions, main="nyt21 Impressions Norm Q-Q"); qqline(nyt21$Impressions)
qqnorm(nyt21$Age, main="nyt21 Age Norm Q-Q"); qqline(nyt21$Age)
par(mfrow=c(1,2))
qqnorm(nyt22$Impressions, main="nyt22 Impressions Norm Q-Q"); qqline(nyt22$Impressions)
qqnorm(nyt22$Age, main="nyt22 Age Norm Q-Q"); qqline(nyt22$Age)
par(mfrow=c(1,2))
qqnorm(nyt23$Impressions, main="nyt23 Impressions Norm Q-Q"); qqline(nyt23$Impressions)
qqnorm(nyt23$Age, main="nyt23 Age Norm Q-Q"); qqline(nyt23$Age)
par(mfrow=c(1,2))
qqnorm(nyt24$Impressions, main="nyt24 Impressions Norm Q-Q"); qqline(nyt24$Impressions)
qqnorm(nyt24$Age, main="nyt24 Age Norm Q-Q"); qqline(nyt24$Age)
par(mfrow=c(1,2))
qqnorm(nyt25$Impressions, main="nyt25 Impressions Norm Q-Q"); qqline(nyt25$Impressions)
qqnorm(nyt25$Age, main="nyt25 Age Norm Q-Q"); qqline(nyt25$Age)

# Significance tests
nyt21_sampled <- nyt21[sample(nrow(nyt21), 5000), ]
nyt22_sampled <- nyt22[sample(nrow(nyt22), 5000), ]
nyt23_sampled <- nyt23[sample(nrow(nyt23), 5000), ]
nyt24_sampled <- nyt24[sample(nrow(nyt24), 5000), ]
nyt25_sampled <- nyt25[sample(nrow(nyt25), 5000), ]

# Impressions
shapiro.test(nyt21_filtered_sampled$Impressions)
shapiro.test(nyt22_filtered_sampled$Impressions)
shapiro.test(nyt23_filtered_sampled$Impressions)
shapiro.test(nyt24_filtered_sampled$Impressions)
shapiro.test(nyt25_filtered_sampled$Impressions)

# Age
shapiro.test(nyt21_filtered_sampled$Age)
shapiro.test(nyt22_filtered_sampled$Age)
shapiro.test(nyt23_filtered_sampled$Age)
shapiro.test(nyt24_filtered_sampled$Age)
shapiro.test(nyt25_filtered_sampled$Age)

###################################################
# Filtering out Age=0 rows and considering just nyt21 and nyt22 datasets
nyt21_filtered = nyt21[nyt21$Age != 0,]
nyt22_filtered = nyt22[nyt22$Age != 0,]

# Histograms after filter
par(mfrow=c(2,2))
hist(nyt21_filtered$Impressions, breaks=19, main="nyt21 Impressions Hist")
hist(nyt21_filtered$Age, breaks=24, main="nyt21 Age Hist")
hist(nyt22_filtered$Impressions, breaks=19, main="nyt22 Impressions Hist")
hist(nyt22_filtered$Age, breaks=24, main="nyt22 Age Hist")

# ECDF after filter
par(mfrow=c(1,2))
plot(ecdf(nyt21_filtered$Impressions), do.points=FALSE, verticals=TRUE, main="nyt21 Impressions ECDF") 
plot(ecdf(nyt21_filtered$Age), do.points=FALSE, verticals=TRUE, main="nyt21 Age ECDF") 
par(mfrow=c(1,2))
plot(ecdf(nyt22_filtered$Impressions), do.points=FALSE, verticals=TRUE, main="nyt22 Impressions ECDF") 
plot(ecdf(nyt22_filtered$Age), do.points=FALSE, verticals=TRUE, main="nyt22 Impressions ECDF") 

# Q-Q Norm after filter
par(mfrow=c(1,2))
qqnorm(nyt21_filtered$Impressions, main="nyt21 Impressions Q-Q"); qqline(nyt21_filtered$Impressions)
qqnorm(nyt21_filtered$Age, main="nyt21 Age Q-Q"); qqline(nyt21_filtered$Age)
par(mfrow=c(1,2))
qqnorm(nyt22_filtered$Impressions, main="nyt22 Impressions Q-Q"); qqline(nyt22_filtered$Impressions)
qqnorm(nyt22_filtered$Age, main="nyt22 Age Q-Q"); qqline(nyt22_filtered$Age)

# Significance tests after filter
nyt21_filtered_sampled <- nyt21_filtered[sample(nrow(nyt21_filtered), 5000), ]
nyt22_filtered_sampled <- nyt22_filtered[sample(nrow(nyt22_filtered), 5000), ]
shapiro.test(nyt21_filtered_sampled$Impressions)
shapiro.test(nyt21_filtered_sampled$Age)
shapiro.test(nyt22_filtered_sampled$Impressions)
shapiro.test(nyt22_filtered_sampled$Age)
