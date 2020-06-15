############################################
#### Project: Productive Pacifists
#### Task: Replication code for Figure A2
#### Contributor: Suzie Mulesky
#### Date: 06/09/2020
############################################

# Set working directory

# Load packages
library(Amelia)
library(dplyr)

# Import dataset
load("FigureA2_Data.RDATA")

# Select only the variables needed
df <- select(rent, 
             ag_combined, ag_employ_OWID, aes_AES,
             pec_MC, WorldBank_gdppc_2010_con_estimate,
             upop_MC, gwno, country, year)

# upop_MC is measured in the thousands. 
df$upop_MC <- df$upop_MC*1000

# Set log values to -3; exp(-3)=near the empirical minimum
df$pec_MC[df$pec_MC==0] <- exp(-3)
df$upop_MC[df$upop_MC==0] <- exp(-3)

df$pec_MC <- log(df$pec_MC)
df$upop_MC <- log(df$upop_MC)

# Add proportion variables bounded [0,1]
df$ag_combined <- df$ag_combined/100
df$ag_employ_OWID <- df$ag_employ_OWID/100
df$aes_AES <- df$aes_AES/100

# Rename variables
names(df)[1] <- "ag/GDP"
names(df)[2] <- "ag employment (OWID)"
names(df)[3] <- "ag employment (AES)"
names(df)[4] <- "energy consumption"
names(df)[5] <- "GDP per capita"
names(df)[6] <- "urban population"

# Impute data
df.complete <- amelia(df, m = 5, p2s = 2, ts = "year", cs = "gwno", incheck = TRUE,
                      idvars = ("country"),
                      bound = rbind(c(1, 0, 1), c(2, 0, 1), c(3, 0, 1), 
                                    c(4, -3, Inf), c(5, -3, Inf), c(6, -3, Inf)), 
                      lags = c("ag/GDP", "ag employment (OWID)", "ag employment (AES)",
                               "energy consumption", "GDP per capita", "urban population"),
                      leads = c("ag/GDP", "ag employment (OWID)", "ag employment (AES)",
                                "energy consumption", "GDP per capita", "urban population"))

# Overimpute graph
pdf(file = "figurea2.pdf")
overimpute(df.complete, "ag/GDP",
           main = "Observed versus imputed values of\nagriculture value added (% of GDP)")
dev.off()
