library(tidyverse)

#Read in the tsv as a dataframe
setwd("../")
data <- read.delim("MutationRate.tsv", header = TRUE)

#Convert Mutation Type to a factor for downstream applications
data$MutationType <- factor(data$MutationType)

#Make a new dataframe where all of the unique combinations of Genotype, MutationType, and Genome are summarized by their
#mean MutationRate and the standard error of their mutation rate. 
averaged_data <- data %>%
  group_by(Genotype, MutationType, Genome) %>%
  summarise(mean_MutationRate = mean(MutationRate),
            std_error = sd(MutationRate) / sqrt(n()))


plot <- ggplot(averaged_data) +
  #Set up the bargraph. The mutation rate is the y-axis and the x-axis is the genotype. Have the MutationType by adjacent bars
  #in the same graph
  geom_col(aes(x = Genotype, y = mean_MutationRate, fill = MutationType), position = "dodge") +
  #Add the error bars to the bar graph based on the std error
  geom_errorbar(aes(x = Genotype, ymin = mean_MutationRate - std_error, ymax = mean_MutationRate + std_error, 
                    color = MutationType), 
                width = 0.25, position = position_dodge(width = 0.9)) +
  #Add Labels
  labs(title = "Mutation Rate Across Multiple Genotypes in Mitochondrial and Plastid Genomes",
       x = "Genotype",
       y = "Mutation Rate") +
  #Add the final variable (Genome) in the facet_wrap
  facet_wrap(~ Genome) +
  #Make the bars be colored based on their mutation type
  scale_fill_manual(values = c("#FF5959", "#377eb8")) +  
  #Keep the color scheme of the error bars consistent with the rest of the graph
  #But make them darker so they stand out
  scale_color_manual(values = c("#9C0000", "#050076")) +  
  #I like theme_minimal
  theme_minimal()

ggsave("Module13.png", plot, width = 10, height = 6, dpi = 300)

#I tried to condense as much information into the graph in as few facet_wrap() as possible
#It made sense to me to just convert the replicates to an average and an error bar that show the spread of the data
#If there were more replicates adding a jitter plot on top would be good, but this condenses one of the variables very nicely
#Ultimately the meat and potatoes of this data is how the genotype affects the mutation rate so those became the corresponding axes
#The side-by-side bar graphs for the MutationType helps condense the data and make it easier to compare while still being
#data rich. 
#Since there are only two levels for the Genome it makes the most sense to be the facet_wrapped part. 
#I tried to play around with the y-axis since the lower values are not really visible, but I found that 
#the defaul honestly looked the best. 

