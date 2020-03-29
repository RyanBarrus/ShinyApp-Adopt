#Barrus, Khan, Hussein

library(igraph)
library(dplyr)
library(ggplot2)

#Get data
facebook <- read.table('107.edges')
facebook <- graph_from_data_frame(facebook)

#Reduce size of arrows for visibility
E(facebook)$arrow.size = .025

#Get adopt function
source('adopt.R')

#View arguments
args(adopt)

#Run the procedure sampling from entire population
par(mfrow = c(2,2))
adopt(facebook,viewPlots=TRUE)

#The results are the percent that adopted at the end of each sample
#Now we gather the results from various parameter combinations.
#Fix at 30 % of graph
#Fix at 50 samples per parameter combination
#Use combinations of SamplePopulation, SamplePopulationType, and Initial Adopters.

results <- data.frame()
parms <- expand.grid(c('Degree','Closeness','Entire Graph'),
                     c(3,4,5),
                     c('High','Low'))

#SamplePopulationType isn't meaningful for the Entire Graph sample population, remove 'Low' to avoid double counting.
parms <- parms[!c(parms[,1] == 'Entire Graph' & parms[,3] == 'Low'),]
#Relabel
parms[parms[,1] == 'Entire Graph',3] <- NA

#View parameter matrix
parms

#run the model 50 times for each combination 
#grab a snack, run time is about 8.5 mins on this machine. Or retrieve saved results
#results <- read.csv('AdoptResults.csv')
for (i in seq_len(dim(parms)[1])) {
  results <- rbind(results,
                   data.frame('SamplePopulation' = parms[i,1],
                              'SamplePopulationType' = parms[i,3],
                              'InitialAdopters' = parms[i,2],
                              'PercentAdopted' = adopt(facebook,
                                                       sampPop = parms[i,1],
                                                       sampPopType = parms[i,3],
                                                       initialAdopters = parms[i,2],
                                                       portionOfGraph = .3,
                                                       nSamples=50,
                                                       viewPlots = FALSE,
                                                       q= .1)))
}

#examine group statistics
options(digits=2)
results %>%
  group_by(SamplePopulation,SamplePopulationType,InitialAdopters) %>%
  summarise(MeanAdoption = mean(PercentAdopted),
            SDAdoption = sd(PercentAdopted),
            MedianAdoption = median(PercentAdopted)) %>%
  View()

#visualize
results %>%
  filter(SamplePopulationType == 'High' | is.na(SamplePopulationType)) %>%
  ggplot(aes(y=PercentAdopted,x=SamplePopulation,col=SamplePopulation,fill=SamplePopulation)) +
  geom_boxplot(alpha=.2) +
  facet_wrap(~InitialAdopters) +
  theme_bw()

#save the results
#write.csv(results,file='AdoptResults.csv')
