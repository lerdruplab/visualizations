library('tidyverse')

# importing the data, here input the heat maps for the two replicates from each strain
strain1rep1 <- read.table('heatmapdata/BL6replicate1million.txt', skip = 8)
strain1rep2 <- read.table('heatmapdata/BL6replicate2million.txt', skip = 8)
strain2rep1 <- read.table('heatmapdata/DBAreplicate1million.txt', skip = 8)
strain2rep2 <- read.table('heatmapdata/DBAreplicate2million.txt', skip = 8)

# window size chosen in clustering, here 200
pos <- -99:99

# comparing strains to calculate the absolute difference signal
table3 <- strain1rep1 - strain2rep1
table4 <- abs(table3/nrow(table3))
sums1 <- colSums(table4)

table3 <- strain1rep2 - strain2rep2
table4 <- abs(table3/nrow(table3))
sums2 <- colSums(table4)

meansums <- abs(sums1+sums2)/2

df1 <- data.frame(pos, meansums)

# plot of the signal without correction
p <- ggplot(df1, aes(pos, meansums)) +
  ggtitle('bl6/dba') +
  geom_smooth(method = "loess") +
  geom_point()
p

# comparing replicates to calculate the "background noise"

# for strain 1 replicates
table3 <- strain1rep1 - bl62
table4 <- abs(table3/nrow(table3))
bgsums1 <- colSums(table4)

# for strain 2 replicates
table3 <- dba1 - strain2rep2
table4 <- abs(table3/nrow(table3))
bgsums2 <- colSums(table4)

meanbackground <- abs(bgsums1+bgsums2)/2

# subtracting the background to get the final result
finalsums <- meansums-meanbackground

# creating dataframe for visualization
df2 <- data.frame(pos, finalsums, meansums, meanbackground)

# plot of the signal with correction
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
  scale_color_manual(values=c('#6495ED','#999999', '#87CEEB'), name ="Absolute", labels = c("Adjusted signal", "Background",  "Signal"))+
  labs(x = "Bin", y = "Signal", title = "BL6/DBA H3K4me3 signal variation at insertions sites - 1 million basepair window - 200 bins") +
  theme_minimal()

# visualization of the individual bins, with the option to chose to colour some of choise. 
# here 2 dots before and after is used as an example
df2$colour <- 0
df2$colour <- with(df2, ifelse(pos < -2, 'grey', ifelse(pos > 2, 'grey', 'red')))  # numbers can be adjuste for the bins of interest

ggplot(df2, aes(pos, finalsums, color = colour))  +
  labs(x = "Bin", y = "Signal", title = 'BL6/DBA insetion variation - 1 million basepair window - 200 bins') +
  geom_point() +
  scale_color_manual(values=c('#999999','red'))+
  theme_minimal()

