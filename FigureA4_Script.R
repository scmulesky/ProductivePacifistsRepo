#############################################
#### Project: Productive Pacifists
#### Task: Replication code for Figure A4
#### Contributor: Suzie Mulesky
#### Date: 06/09/2020
#############################################

# Set working directory


# Load packages
library(dplyr)
library(ggplot2)

# Import datasets
load("3.+Graham_Tucker_IPE_v3_0.RDATA")
load("ProductivePacifists_Data.RDATA")

# Keep only the variables we want
ipe <- select(ipe_full, country, year, gwno, combinedoil_AE)
rent <- select(prop1, country, year, gwno, resource_rents_WDI_avg)

rm(ipe_full)
rm(prop1)

# Which countries have oil exports accounting for at least 10% of GDP in our sample?
petrostates <- ipe %>% 
  filter(combinedoil_AE >= 10)

# What's the distribution of resource rents for countries with oil exports accounting for at least 10% of GDP?
petrostates <- merge(x = petrostates, 
                     y = rent, 
                     by = c("gwno", "year"), 
                     all.x = TRUE)
petrostates <- petrostates %>%  
  select(-country.y) %>%
  rename(country = country.x)

# Figure A4
figa4 <- ggplot(data = petrostates, aes(x = resource_rents_WDI_avg)) +
  geom_histogram(fill = "grey", color = "black", size = 0.5) +
  geom_vline(xintercept = 7.5, linetype = "dashed", size = .7) +
  labs(x = "Natural Resources Rents (% of GDP)", y = "Count") +
  ggtitle("Distribution of resource rents (% of GDP)",
          subtitle = "for countries with oil exports greater than 10% of GDP") +
  theme_classic() +
  theme(axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9))

ggsave("figurea4.jpg", plot = figa4, height = 4.5, width = 6.5, units = "in")



