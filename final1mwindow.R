library('tidyverse')

# importing the data, here input the heat maps for the two replicates from each strain
bl61 <- read.table('heatmapdata/BL6replicate1million.txt', skip = 8)
bl62 <- read.table('heatmapdata/BL6replicate2million.txt', skip = 8)
dba1 <- read.table('heatmapdata/DBAreplicate1million.txt', skip = 8)
dba2 <- read.table('heatmapdata/DBAreplicate2million.txt', skip = 8)

# window size chosen in clustering, here 200
pos <- -99:99

# comparing strains
table3 <- bl61 - dba1
table4 <- abs(table3/nrow(table3))
sums1 <- colSums(table4)

table3 <- bl62 - dba2
table4 <- abs(table3/nrow(table3))
sums2 <- colSums(table4)

meansums <- abs(sums1+sums2)/2

df1 <- data.frame(pos, meansums)


p <- ggplot(df1, aes(pos, meansums)) +
  ggtitle('bl6/dba') +
  geom_smooth(method = "loess") +
  geom_point()
p

# comparing replicates to calculate the "background noise"

# for BL6 replicates
table3 <- bl61 - bl62
table4 <- abs(table3/nrow(table3))
bgsums1 <- colSums(table4)

# for DBA replicates
table3 <- dba1 - dba2
table4 <- abs(table3/nrow(table3))
bgsums2 <- colSums(table4)

meanbackground <- abs(bgsums1+bgsums2)/2

# subtracting the background to get the final result
finalsums <- meansums-meanbackground

df2 <- data.frame(pos, finalsums, meansums, meanbackground)

p <- ggplot(df2, aes(pos, finalsums)) +
  ggtitle('bl6/DBA 100k basepair') +
  geom_smooth(method = "loess") +
  geom_point()
p


# visualizing background, initial result and the adjusted result in the same plot
ggplot(df2, aes(x = pos)) +
  geom_line(aes(y = meanbackground, color = "meanbackground"), size = 1) +
  geom_line(aes(y = finalsums, color = "finalsums"), size = 1) +
  geom_line(aes(y = meansums, color = "meansums"), size = 1) +
  scale_color_manual(values=c('cyan3','#999999', 'cadetblue1'))+
  labs(x = "Bin", y = "Signal", title = "BL6/DBA insetion variation, 1 million basepair window, 200 bins") +
  theme_minimal()

# visualization of the individual bins, with the option to chose to colour some of choise. 
# here 2 dots before and after is used as an example
df2$colour <- 0
df2$colour <- with(df2, ifelse(pos < -2, 'grey', ifelse(pos > 2, 'grey', 'red')))

ggplot(df2, aes(pos, finalsums, color = colour))  +
  labs(x = "Bin", y = "Signal", title = 'BL6/DBA insetion variation - 1 million basepair window - 200 bins') +
  geom_point() +
  scale_color_manual(values=c('#999999','red'))+
  theme_minimal()

