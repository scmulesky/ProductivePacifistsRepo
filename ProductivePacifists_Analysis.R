############################################################################
#### Project: Productive Pacifists
#### Task: Test proposition (land-orientation -> competition over territory)
#### Contributor: Suzie Mulesky
#### Date: 06/09/2020
############################################################################

# Set working directory


# Load packages
library(tidyr)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(survival)
library(corrplot)

# Import dataset
load("ProductivePacifists_Data.RDATA")

# Use only the variables we need
dat <- prop1 %>%
  mutate(year2 = (year^2)/1000,
         year3 = (year^3)/100000) %>% 
  select(tc_dummy_ICOW, tc_resource_dummy_ICOW, tc_mid_dummy_ICOW, 
         tc_mid_resource_dummy_ICOW, spatial_dummy_TR,
         tc_count_ICOW, tc_resource_count_ICOW, tc_mid_count_ICOW, 
         tc_mid_resource_count_ICOW, spatial_count_TR,
         land_oriented_medium_binary, land_oriented_low_binary,
         land_oriented_high_binary, land_oriented_medium_continuous,
         land_oriented_medium_continuous_trim,
         resource_dependent_7.5, ag_dependent_15,
         autocracy_BX, autocracy_P4,
         milper_MC, milex_constant2010us_AFM, 
         WorldBank_gdp_2010_con_estimate, WorldBank_gdppc_2010_con_estimate,
         CINC_tpop_estimate,
         cinc_MC, trade_WDI, land_CONT, island_CONT,
         oil_producing_neighbors1,
         gwno, year, year2, year3)

################
### Figure 1 ###
################

# Decline in the proportion of land-oriented countries over time

df <- dat %>% 
  filter(!is.na(land_oriented_medium_binary) & year < 2014) %>% 
  group_by(year) %>% 
  summarise(n_countries = n(),
            n_land_oriented = sum(land_oriented_medium_binary)) %>% 
  mutate(prop_land_oriented = n_land_oriented / n_countries)

fig1 <- ggplot(data = df, aes(x = year, y = prop_land_oriented)) +
  geom_line(size = .6) +
  geom_area(position = "identity", alpha = .1) +
  ylim(0, 1) + 
  scale_x_continuous(breaks = seq(1825, 2000, 25)) +
  labs(x = "Year", y = "") +
  ggtitle("Proportion of Land-oriented Countries") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5))

ggsave("figure1.jpg", plot = fig1, height = 4.5, width = 6.5, units = "in")


#########################
### Regression Tables ###
#########################

# --- Table 1 in main paper (primary specification, 4 models)
# Model 1
summary(model1 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                        autocracy_BX +
                        milper_MC + 
                        milex_constant2010us_AFM + 
                        WorldBank_gdppc_2010_con_estimate + 
                        CINC_tpop_estimate + land_CONT + island_CONT +
                        year + year2 + year3,
                      family = binomial(link = "logit"),
                      na.action = na.exclude,
                      data = dat))

# Model 2
summary(model2 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                        autocracy_BX + 
                        milper_MC + 
                        milex_constant2010us_AFM + 
                        WorldBank_gdppc_2010_con_estimate + 
                        CINC_tpop_estimate + land_CONT + island_CONT +
                        year + year2 + year3,
                      family = binomial(link = "logit"),
                      na.action = na.exclude,
                      data = dat))

# Model 3
summary(model3 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_binary +
                        autocracy_BX + 
                        milper_MC + 
                        milex_constant2010us_AFM + 
                        WorldBank_gdppc_2010_con_estimate + 
                        CINC_tpop_estimate + land_CONT + island_CONT +
                        year + year2 + year3,
                      family = binomial(link = "logit"),
                      na.action = na.exclude,
                      data = dat))

# Model 4
summary(model4 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                        autocracy_BX + 
                        milper_MC + 
                        milex_constant2010us_AFM + 
                        WorldBank_gdppc_2010_con_estimate + 
                        CINC_tpop_estimate + land_CONT + island_CONT +
                        year + year2 + year3,
                      family = binomial(link = "logit"),
                      na.action = na.exclude,
                      data = dat))

# Regression table for Word (Table 1)
stargazer(model1, model2, model3, model4,
          type = "html",
          out = "table1.html",
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          column.labels = c("Resource-based territorial claim (binary)", 
                            "Resource-based territorial MID (binary)"),
          column.separate = c(2,2),
          notes.label = "Significance levels",
          omit.stat = c("aic", "ll"),
          covariate.labels = c("Land-orientation (binary)",
                               "Land-orientation (continuous)",
                               "Autocracy (binary)",
                               "Military personnel, log",
                               "Military expenditures, log",
                               "GDP per capita, log",
                               "Population, log",
                               "Neighbors",
                               "Island (dummy)",
                               "Time count",
                               "Time count$^{2}$",
                               "Time count$^{3}$"))



# --- Table 2 in main paper (split sample analysis, 4 models)

# Model 1 - autocracies
summary(model1_t2 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, autocracy_BX == 1)))

# Model 2 - democracies
summary(model2_t2 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, autocracy_BX == 0)))


# Model 3 - autocracies
summary(model3_t2 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, autocracy_BX == 1)))

# Model 4 - democracies
summary(model4_t2 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, autocracy_BX == 0)))


# Regression table for Word (Table 2)
stargazer(model1_t2, model2_t2, model3_t2, model4_t2,
          type = "html",
          out = "table2.html",
          dep.var.caption = "",
          column.labels = c("Autocracies", "Democracies", 
                            "Autocracies", "Democracies"),
          column.separate = c(1,1,1,1),
          notes.label = "Significance levels",
          omit.stat = c("aic", "ll"),
          covariate.labels = c("Land-orientation (binary)",
                               "Military personnel, log",
                               "Military expenditures, log",
                               "GDP per capita, log",
                               "Population, log",
                               "Neighbors",
                               "Island (dummy)",
                               "Time count",
                               "Time count$^{2}$",
                               "Time count$^{3}$"))


# --- Table 3 (test ag & natural resources independently)

# Model 1 - ag
summary(model1_t3 <- glm(tc_resource_dummy_ICOW ~ ag_dependent_15 + 
                           autocracy_BX +
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Model 2 - natural resources
summary(model2_t3 <- glm(tc_resource_dummy_ICOW ~ resource_dependent_7.5 + 
                           autocracy_BX +
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Model 3 - ag and natural resources
summary(model3_t3 <- glm(tc_resource_dummy_ICOW ~ ag_dependent_15 + 
                           resource_dependent_7.5 + 
                           autocracy_BX +
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Model 4 - ag
summary(model4_t3 <- glm(tc_mid_resource_dummy_ICOW ~ ag_dependent_15 +
                           autocracy_BX + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Model 5 - natural resources
summary(model5_t3 <- glm(tc_mid_resource_dummy_ICOW ~ resource_dependent_7.5 +
                           autocracy_BX + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Model 6 - ag and natural resources
summary(model6_t3 <- glm(tc_mid_resource_dummy_ICOW ~ ag_dependent_15 +
                           resource_dependent_7.5 +
                           autocracy_BX + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Regression table for Word (Table 3)
stargazer(model1_t3, model2_t3, model3_t3, model4_t3, model5_t3, model6_t3,
          type = "html",
          out = "table3.html",
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          column.labels = c("Resource-based territorial claim (binary)",
                            "Resource-based territorial MID (binary)"),
          column.separate = c(3,3),
          notes.label = "Significance levels",
          omit.stat = c("aic", "ll"),
          covariate.labels = c("Agricultural dependence (binary)",
                               "Resource rent dependence (binary)",
                               "Autocracy (binary)",
                               "Military personnel, log",
                               "Military expenditures, log",
                               "GDP per capita, log",
                               "Population, log",
                               "Neighbors",
                               "Island (dummy)",
                               "Time count",
                               "Time count$^{2}$",
                               "Time count$^{3}$"))


# --- Footnote #17 - using trimmed continuous land-orientation

# Model 2 (Table 1)
summary(model2_f <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_continuous_trim +
                          autocracy_BX + 
                          milper_MC + 
                          milex_constant2010us_AFM + 
                          WorldBank_gdppc_2010_con_estimate + 
                          CINC_tpop_estimate + land_CONT + island_CONT +
                          year + year2 + year3,
                        family = binomial(link = "logit"),
                        na.action = na.exclude,
                        data = dat))

# Model 4 (Table 1)
summary(model4_f <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_continuous_trim +
                          autocracy_BX + 
                          milper_MC + 
                          milex_constant2010us_AFM + 
                          WorldBank_gdppc_2010_con_estimate + 
                          CINC_tpop_estimate + land_CONT + island_CONT +
                          year + year2 + year3,
                        family = binomial(link = "logit"),
                        na.action = na.exclude,
                        data = dat))



# --- Appendix Table A1 (alternative control variables)

# Model 1
summary(model1_a1 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           autocracy_BX +
                           cinc_MC + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + island_CONT + 
                           trade_WDI + oil_producing_neighbors1 +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Model 2
summary(model2_a1 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                           autocracy_BX + 
                           cinc_MC + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + island_CONT + 
                           trade_WDI + oil_producing_neighbors1 +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Model 3
summary(model3_a1 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           autocracy_BX +
                           cinc_MC + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + island_CONT + 
                           trade_WDI + oil_producing_neighbors1 +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Model 4
summary(model4_a1 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                           autocracy_BX +
                           cinc_MC + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + island_CONT + 
                           trade_WDI + oil_producing_neighbors1 +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat))

# Regression table for LaTeX (Table A1)
stargazer(model1_a1, model2_a1, model3_a1, model4_a1,
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          column.labels = c("Resource-based territorial claim (binary)",
                            "Resource-based territorial MID (binary)"),
          column.separate = c(2,2),
          notes.label = "Significance levels",
          omit.stat = c("aic", "ll"),
          covariate.labels = c("Land-orientation (binary)",
                               "Land-orientation (continuous)",
                               "Autocracy (binary)",
                               "CINC Score",
                               "GDP per capita, log",
                               "Population, log",
                               "Island (dummy)",
                               "Trade (% of GDP)",
                               "Oil & gas producing neighbors",
                               "Time count",
                               "Time count$^{2}$",
                               "Time count$^{3}$"))


# --- Appendix Table A2 (pre-WW2)

# Model 1
summary(model1_a2 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           autocracy_BX +
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, year < 1939)))

# Model 2
summary(model2_a2 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                           autocracy_BX + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, year < 1939)))

# Model 3
summary(model3_a2 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           autocracy_BX +
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, year < 1939)))

# Model 4
summary(model4_a2 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                           autocracy_BX + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, year < 1939)))

# Regression table for LaTeX (Table A2)
stargazer(model1_a2, model2_a2, model3_a2, model4_a2,
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          column.labels = c("Resource-based territorial claim (binary)",
                            "Resource-based territorial MID (binary)"),
          column.separate = c(2,2),
          notes.label = "Significance levels",
          omit.stat = c("aic", "ll"),
          covariate.labels = c("Land-orientation (binary)",
                               "Land-orientation (continuous)",
                               "Autocracy (binary)",
                               "Military personnel, log",
                               "Military expenditures, log",
                               "GDP per capita, log",
                               "Population, log",
                               "Neighbors",
                               "Island (dummy)",
                               "Time count",
                               "Time count$^{2}$",
                               "Time count$^{3}$"))



# --- Appendix Table A3 (post-WW2)

# Model 1
summary(model1_a3 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           autocracy_BX +
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, year > 1945)))

# Model 2
summary(model2_a3 <- glm(tc_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                           autocracy_BX + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, year > 1945)))

# Model 3
summary(model3_a3 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                           autocracy_BX +
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, year > 1945)))

# Model 4
summary(model4_a3 <- glm(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                           autocracy_BX + 
                           milper_MC + 
                           milex_constant2010us_AFM + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + land_CONT + island_CONT +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = subset(dat, year > 1945)))

# Regression table for LaTeX (Table A3)
stargazer(model1_a3, model2_a3, model3_a3, model4_a3,
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          column.labels = c("Resource-based territorial claim (binary)",
                            "Resource-based territorial MID (binary)"),
          column.separate = c(2,2),
          notes.label = "Significance levels",
          omit.stat = c("aic", "ll"),
          covariate.labels = c("Land-orientation (binary)",
                               "Land-orientation (continuous)",
                               "Autocracy (binary)",
                               "Military personnel, log",
                               "Military expenditures, log",
                               "GDP per capita, log",
                               "Population, log",
                               "Neighbors",
                               "Island (dummy)",
                               "Time count",
                               "Time count$^{2}$",
                               "Time count$^{3}$"))


# --- Appendix Table A4 (country fixed effects on Table 1)

# Model 1
summary(model1_a4 <- clogit(tc_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                              autocracy_BX +
                              milper_MC + 
                              milex_constant2010us_AFM + 
                              WorldBank_gdppc_2010_con_estimate + 
                              CINC_tpop_estimate + land_CONT + island_CONT + strata(gwno),
                            na.action = na.exclude,
                            data = dat))

# Model 2
summary(model2_a4 <- clogit(tc_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                              autocracy_BX + 
                              milper_MC + 
                              milex_constant2010us_AFM + 
                              WorldBank_gdppc_2010_con_estimate + 
                              CINC_tpop_estimate + land_CONT + island_CONT + strata(gwno),
                            na.action = na.exclude,
                            data = dat))

# Model 3
summary(model3_a4 <- clogit(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_binary + 
                              autocracy_BX +
                              milper_MC + 
                              milex_constant2010us_AFM + 
                              WorldBank_gdppc_2010_con_estimate + 
                              CINC_tpop_estimate + land_CONT + island_CONT + strata(gwno),
                            na.action = na.exclude,
                            data = dat))

# Model 4
summary(model4_a4 <- clogit(tc_mid_resource_dummy_ICOW ~ land_oriented_medium_continuous +
                              autocracy_BX + 
                              milper_MC + 
                              milex_constant2010us_AFM + 
                              WorldBank_gdppc_2010_con_estimate + 
                              CINC_tpop_estimate + land_CONT + island_CONT + strata(gwno),
                            na.action = na.exclude,
                            data = dat))

# Regression table for LaTeX (Table A4)
stargazer(model1_a4, model2_a4, model3_a4, model4_a4,
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          column.labels = c("Resource-based territorial claim (binary)",
                            "Resource-based territorial MID (binary)"),
          column.separate = c(2,2),
          notes.label = "Significance levels",
          omit.stat = c("aic", "ll"),
          covariate.labels = c("Land-orientation (binary)",
                               "Land-orientation (continuous)",
                               "Autocracy (binary)",
                               "Military personnel, log",
                               "Military expenditures, log",
                               "GDP per capita, log",
                               "Population, log",
                               "Neighbors",
                               "Island (dummy)"))




#######################################################################

#######################################################################
### Estimating the full set of regression models (160 models total) ###
#######################################################################

# Store variable names in vector
ind_var <- c("land_oriented_medium_binary",                  
             "land_oriented_low_binary",                    
             "land_oriented_high_binary",                     
             "land_oriented_medium_continuous")

autoc_var <- c("autocracy_BX", "autocracy_P4")

# Create empty vectors
# For binary data
dv_variable = rep(NA, 40)
dv_beta = rep(NA, 40)
autoc_beta = rep(NA, 40)
dv_se = rep(NA, 40)
dv_zvalue = rep(NA, 40)
autoc_zvalue = rep(NA, 40)
dv_pvalue = rep(NA, 40)
dv_aic = rep(NA, 40)
dv_bic = rep(NA, 40)
dv_nobs = rep(NA, 40)
iv_variable = rep(NA, 40)
autoc_variable = rep(NA, 40)

dv_variable_alt = rep(NA, 40)
dv_beta_alt = rep(NA, 40)
autoc_beta_alt = rep(NA, 40)
dv_se_alt = rep(NA, 40)
dv_zvalue_alt = rep(NA, 40)
autoc_zvalue_alt = rep(NA, 40)
dv_pvalue_alt = rep(NA, 40)
dv_aic_alt = rep(NA, 40)
dv_bic_alt = rep(NA, 40)
dv_nobs_alt = rep(NA, 40)
iv_variable_alt = rep(NA, 40)
autoc_variable_alt = rep(NA, 40)

# For count data
dv_variable2 = rep(NA, 40)
dv_beta2 = rep(NA, 40)
autoc_beta2 = rep(NA, 40)
dv_se2 = rep(NA, 40)
dv_zvalue2 = rep(NA, 40)
autoc_zvalue2 = rep(NA, 40)
dv_pvalue2 = rep(NA, 40)
dv_aic2 = rep(NA, 40)
dv_bic2 = rep(NA, 40)
dv_nobs2 = rep(NA, 40)
iv_variable2 = rep(NA, 40)
autoc_variable2 = rep(NA, 40)

dv_variable_alt2 = rep(NA, 40)
dv_beta_alt2 = rep(NA, 40)
autoc_beta_alt2 = rep(NA, 40)
dv_se_alt2 = rep(NA, 40)
dv_zvalue_alt2 = rep(NA, 40)
autoc_zvalue_alt2 = rep(NA, 40)
dv_pvalue_alt2 = rep(NA, 40)
dv_aic_alt2 = rep(NA, 40)
dv_bic_alt2 = rep(NA, 40)
dv_nobs_alt2 = rep(NA, 40)
iv_variable_alt2 = rep(NA, 40)
autoc_variable_alt2 = rep(NA, 40)

number = 1
number2 = 1

# Analysis

for (i in 1:length(colnames(dat))){
  for (j in 1:length(ind_var)){
    for (k in 1:length(autoc_var)){
      if (str_detect(names(dat)[i], "dummy")) {
        model_prim <- glm(get(names(dat)[i]) ~ get(ind_var[j]) + 
                            get(autoc_var[k]) +
                            milper_MC + 
                            milex_constant2010us_AFM + 
                            WorldBank_gdppc_2010_con_estimate + 
                            CINC_tpop_estimate + land_CONT + island_CONT +
                            year + year2 + year3,
                          family = binomial(link = "logit"),
                          na.action = na.exclude,
                          data = dat)
        
        Vcov <- vcov(model_prim, useScale = FALSE)
        beta <- coef(model_prim)
        se <- sqrt(diag(Vcov))
        zval <- beta / se
        pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
        aic <- AIC(model_prim)
        bic <- BIC(model_prim)
        n_obs <- nobs(model_prim)
        
        dv_beta[number] <- as.numeric(beta[2])
        autoc_beta[number] <- as.numeric(beta[3])
        dv_se[number] <- as.numeric(se[2])
        dv_zvalue[number] <- as.numeric(zval[2])
        autoc_zvalue[number] <- as.numeric(zval[3])
        dv_pvalue[number] <- as.numeric(pval[2])
        dv_aic[number] <- as.numeric(aic)
        dv_bic[number] <- as.numeric(bic)
        dv_nobs[number] <- as.numeric(n_obs)
        dv_variable[number] <- names(dat)[i]
        iv_variable[number] <- ind_var[j]
        autoc_variable[number] <- autoc_var[k]
        
        
        model_alt <- glm(get(names(dat)[i]) ~ get(ind_var[j]) + 
                           get(autoc_var[k]) + 
                           cinc_MC + 
                           WorldBank_gdppc_2010_con_estimate + 
                           CINC_tpop_estimate + island_CONT + 
                           trade_WDI + oil_producing_neighbors1 +
                           year + year2 + year3,
                         family = binomial(link = "logit"),
                         na.action = na.exclude,
                         data = dat)
        
        Vcov_alt <- vcov(model_alt, useScale = FALSE)
        beta_alt <- coef(model_alt)
        se_alt <- sqrt(diag(Vcov_alt))
        zval_alt <- beta_alt / se_alt
        pval_alt <- 2 * pnorm(abs(zval_alt), lower.tail = FALSE)
        aic_alt <- AIC(model_alt)
        bic_alt <- BIC(model_alt)
        n_obs_alt <- nobs(model_alt)
        
        dv_beta_alt[number] <- as.numeric(beta_alt[2])
        autoc_beta_alt[number] <- as.numeric(beta_alt[3])
        dv_se_alt[number] <- as.numeric(se_alt[2])
        dv_zvalue_alt[number] <- as.numeric(zval_alt[2])
        autoc_zvalue_alt[number] <- as.numeric(zval_alt[3])
        dv_pvalue_alt[number] <- as.numeric(pval_alt[2])
        dv_aic_alt[number] <- as.numeric(aic_alt)
        dv_bic_alt[number] <- as.numeric(bic_alt)
        dv_nobs_alt[number] <- as.numeric(n_obs_alt)
        dv_variable_alt[number] <- names(dat)[i]
        iv_variable_alt[number] <- ind_var[j]
        autoc_variable_alt[number] <- autoc_var[k]
        
        number = number + 1
        
      } else if (str_detect(names(dat)[i], "count")) {
        model_prim2 <- glm(get(names(dat)[i]) ~ get(ind_var[j]) + 
                             get(autoc_var[k]) +
                             milper_MC + 
                             milex_constant2010us_AFM + 
                             WorldBank_gdppc_2010_con_estimate + 
                             CINC_tpop_estimate + land_CONT + island_CONT +
                             year + year2 + year3,
                           family = poisson,
                           na.action = na.exclude,
                           data = dat)
        
        Vcov2 <- vcov(model_prim2, useScale = FALSE)
        beta2 <- coef(model_prim2)
        se2 <- sqrt(diag(Vcov2))
        zval2 <- beta2 / se2
        pval2 <- 2 * pnorm(abs(zval2), lower.tail = FALSE)
        aic2 <- AIC(model_prim2)
        bic2 <- BIC(model_prim2)
        n_obs2 <- nobs(model_prim2)
        
        dv_beta2[number2] <- as.numeric(beta2[2])
        autoc_beta2[number2] <- as.numeric(beta2[3])
        dv_se2[number2] <- as.numeric(se2[2])
        dv_zvalue2[number2] <- as.numeric(zval2[2])
        autoc_zvalue2[number2] <- as.numeric(zval2[3])
        dv_pvalue2[number2] <- as.numeric(pval2[2])
        dv_aic2[number2] <- as.numeric(aic2)
        dv_bic2[number2] <- as.numeric(bic2)
        dv_nobs2[number2] <- as.numeric(n_obs2)
        dv_variable2[number2] <- names(dat)[i]
        iv_variable2[number2] <- ind_var[j]
        autoc_variable2[number2] <- ind_var[k]
        
        model_alt2 <- glm(get(names(dat)[i]) ~ get(ind_var[j]) + 
                            get(autoc_var[k]) +
                            cinc_MC + 
                            WorldBank_gdppc_2010_con_estimate + 
                            CINC_tpop_estimate + island_CONT + 
                            trade_WDI + oil_producing_neighbors1 +
                            year + year2 + year3,
                          family = poisson,
                          na.action = na.exclude,
                          data = dat)
        
        Vcov_alt2 <- vcov(model_alt2, useScale = FALSE)
        beta_alt2 <- coef(model_alt2)
        se_alt2 <- sqrt(diag(Vcov_alt2))
        zval_alt2 <- beta_alt2 / se_alt2
        pval_alt2 <- 2 * pnorm(abs(zval_alt2), lower.tail = FALSE)
        aic_alt2 <- AIC(model_alt2)
        bic_alt2 <- BIC(model_alt2)
        n_obs_alt2 <- nobs(model_alt2)
        
        dv_beta_alt2[number2] <- as.numeric(beta_alt2[2])
        autoc_beta_alt2[number2] <- as.numeric(beta_alt2[3])
        dv_se_alt2[number2] <- as.numeric(se_alt2[2])
        dv_zvalue_alt2[number2] <- as.numeric(zval_alt2[2])
        autoc_zvalue_alt2[number2] <- as.numeric(zval_alt2[3])
        dv_pvalue_alt2[number2] <- as.numeric(pval_alt2[2])
        dv_aic_alt2[number2] <- as.numeric(aic_alt2)
        dv_bic_alt2[number2] <- as.numeric(bic_alt2)
        dv_nobs_alt2[number2] <- as.numeric(n_obs_alt2)
        dv_variable_alt2[number2] <- names(dat)[i]
        iv_variable_alt2[number2] <- ind_var[j]
        autoc_variable_alt2[number2] <- autoc_var[k]
        
        number2 = number2 + 1
      } else {
        break
      }
    }
  }
}


# Create a dataframe with results:
results_prim = data.frame(dv_variable, iv_variable, autoc_variable, dv_beta, autoc_beta, dv_se, dv_zvalue, autoc_zvalue, dv_pvalue, dv_aic, dv_bic, dv_nobs, 
                          model = rep("logit; primary spec", 40))
results_alt = data.frame(dv_variable_alt, iv_variable_alt, autoc_variable_alt, dv_beta_alt, autoc_beta_alt, dv_se_alt, dv_zvalue_alt, autoc_zvalue, dv_pvalue_alt, dv_aic_alt, dv_bic_alt, dv_nobs_alt,
                         model = rep("logit; alt spec", 40))
results_prim2 = data.frame(dv_variable2, iv_variable2, autoc_variable2, dv_beta2, autoc_beta2, dv_se2, dv_zvalue2, autoc_zvalue2, dv_pvalue2, dv_aic2, dv_bic2, dv_nobs2,
                           model = rep("poisson; primary spec", 40))
results_alt2 = data.frame(dv_variable_alt2, iv_variable_alt2, autoc_variable_alt2, dv_beta_alt2, autoc_beta_alt2, dv_se_alt2, dv_zvalue_alt2, autoc_zvalue2, dv_pvalue_alt2, dv_aic_alt2, dv_bic_alt2, dv_nobs_alt2,
                          model = rep("poisson; alt spec", 40))

# Combine dfs
df_names <- list(results_prim, results_alt, results_prim2, results_alt2)
for (i in 1:4){
  names(df_names[[i]]) <- c("dv", "iv", "autoc", "beta", "autoc_beta", "se", "zvalue", 
                            "autoc_zvalue", "pvalue", "aic", "bic", "n_obs", "model")
}
results <- do.call("rbind", df_names)

results <- mutate(results, type = ifelse(str_detect(dv, "dummy"), "Logistic Regression", 
                                         "Poisson Regression"))



################
### Figure 3 ###
################

# --- Plotting coefficient estimates for land-orientation from 160 models

# Create more informative labels for the dependent variables
results$dv_name <- NA
results$dv_name[results$dv == "spatial_dummy_TR"] <- "territorial rivalry (dummy)"
results$dv_name[results$dv == "tc_dummy_ICOW"] <- "territorial claim (dummy)"
results$dv_name[results$dv == "tc_mid_dummy_ICOW"] <- "territorial MID (dummy)"
results$dv_name[results$dv == "tc_mid_resource_dummy_ICOW"] <- "resource-based territorial MID (dummy)"
results$dv_name[results$dv == "tc_resource_dummy_ICOW"] <- "resource-based territorial claim (dummy)"
results$dv_name[results$dv == "spatial_count_TR"] <- "territorial rivalry (count)"
results$dv_name[results$dv == "tc_count_ICOW"] <- "territorial claim (count)"
results$dv_name[results$dv == "tc_mid_count_ICOW"] <- "territorial MID (count)"
results$dv_name[results$dv == "tc_mid_resource_count_ICOW"] <- "resource-based territorial MID (count)"
results$dv_name[results$dv == "tc_resource_count_ICOW"] <- "resource-based territorial claim (count)"

# Reorder the levels so that the graph puts our main DVs on top
results$dv_name <- as.factor(results$dv_name)
levels(results$dv_name)
results$dv_name <- factor(results$dv_name,
                          levels = c("territorial rivalry (count)",
                                     "territorial rivalry (dummy)",
                                     "territorial MID (count)",
                                     "territorial MID (dummy)",
                                     "territorial claim (count)",
                                     "territorial claim (dummy)",
                                     "resource-based territorial MID (count)",
                                     "resource-based territorial MID (dummy)",
                                     "resource-based territorial claim (count)",
                                     "resource-based territorial claim (dummy)"))
levels(results$dv_name)

# Create a dummy zvalue
results <- results %>% 
  mutate(zvalue_dummy = ifelse(zvalue >= 1.96, "significant (0.05)", "not significant"),
         autoc_zvalue_dummy = ifelse(autoc_zvalue >= 1.96, "significant (0.05)", "not significant"))

# Create variable indicating if land-orientation is binary or continuous
results <- results %>% 
  mutate(binary = ifelse(str_detect(iv, "binary"), "binary", "continuous"))

# Boxplot - Land-orientation
fig3 <- ggplot(results, aes(x = dv_name, y = beta)) +
  geom_boxplot(fill = NA, outlier.shape = NA) +
  geom_jitter(aes(fill = zvalue_dummy, shape = zvalue_dummy), 
              color = "black", size = 2.5,
              position=position_jitter(w = 0.1, h = 0.05)) +
  geom_hline(yintercept = 0, size = .8, linetype = "dashed") +
  scale_fill_manual(name = "", values = c("black", "gray")) +
  scale_shape_manual(name = "", values = c(21, 22)) +
  labs(x = "", y = "Coefficient Estimates") +
  ggtitle("Land-orientation") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.text = element_text(size = 9),
    legend.position = "bottom")

ggsave("figure3.jpg", plot = fig3, height = 4.5, width = 6.5, units = "in")


################
### Figure 4 ###
################

# --- Plotting coefficient estimates for regime type from 160 models

# Boxplot - Regime type
fig4 <- ggplot(results, aes(x = dv_name, y = autoc_beta)) +
  geom_boxplot(fill = NA, outlier.shape = NA) +
  geom_jitter(aes(fill = autoc_zvalue_dummy, shape = autoc_zvalue_dummy), 
              color = "black", size = 2.5,
              position=position_jitter(w = 0.1, h = 0.1)) +
  geom_hline(yintercept = 0, size = 0.8, linetype = "dashed") +
  scale_fill_manual(name = "", values = c("black", "gray")) +
  scale_shape_manual(name = "", values = c(21, 22)) +
  labs(x = "", y = "Coefficient Estimates") +
  ggtitle("Autocracy") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.text.y = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 9),
        legend.position = "bottom")

ggsave("figure4.jpg", plot = fig4, height = 4.5, width = 6.5, units = "in")



########################
### Appendix Figures ###
########################

#################
### Figure A1 ###
#################

# Pearson correlation coefficients

cordat <- select(prop1, 
             ag_combined, ag_employ_OWID, aes_AES,
             pec_MC, WorldBank_gdppc_2010_con_estimate,
             upop_MC, gwno, country, year)

names(cordat)[1] <- "ag/GDP"
names(cordat)[2] <- "ag employment (OWID)"
names(cordat)[3] <- "ag employment (AES)"
names(cordat)[4] <- "energy consumption"
names(cordat)[5] <- "GDP per capita"
names(cordat)[6] <- "urban population"

# Pearson correlation
pearson.cor <- cordat %>% 
  select(1:6) %>% 
  cor(method = "pearson", use = "complete.obs")

# Plot
jpeg(file = "figurea1.jpg", height = 3000, width = 3000, type = "cairo")
corrplot(pearson.cor, method = "color", type = "upper", order = "hclust",  
         tl.col = "black", tl.srt = 35, diag = FALSE, outline = TRUE,
         tl.cex = 5, cl.cex = 5)
dev.off()

#################
### Figure A2 ###
#################

# See separate R script: FigureA2_Script.R

#################
### Figure A3 ###
#################

# Distribution of agriculture value added as a share of GDP during 
# the first and last year a country falls below 25% of agricultural employment

d <- prop1 %>% 
  select(country, ag_gdp_combined_estimate_avg, year, aes_AES, ag_employ_OWID)

# Name the new combined variable: agr
d$agr <- NA
d$agr <- ifelse((!is.na(d$aes_AES)) & (!is.na(d$ag_employ_OWID)), (((d$aes_AES)+(d$ag_employ_OWID))/2), d$agr)
d$agr <- ifelse((!is.na(d$aes_AES)) & (is.na(d$ag_employ_OWID)), d$aes_AES, d$agr)
d$agr <- ifelse((is.na(d$aes_AES)) & (!is.na(d$ag_employ_OWID)), d$ag_employ_OWID, d$agr)

# Look at some country trends
newd <- d %>% 
  filter(agr < 25)

# Select the first year for each country
newd_head <- ddply(newd, "country", function(z) head(z, 1))
newd_hdata <- newd_head %>% 
  select(country, agr, year, ag_gdp_combined_estimate_avg) %>% 
  filter(!is.na(ag_gdp_combined_estimate_avg))

# Select the last year for each country
# Create variable measuring the final year ag crosses below 15% (yat = year of agricultural transition)
d <- d %>% 
  mutate(dummy = ifelse(agr < 25, 1, 0))

# Create a transition dummy, = 1 the year it crosses below 25%
d <- d %>% 
  group_by(country) %>% 
  mutate(lag.dummy = lag(dummy, n = 1, default = NA),
         trans_dummy = ifelse(dummy == 1 & lag.dummy == 0, 1, 0))

# 69 countries transitioned off of ag in our sample (from 1816 to 2016)
yat <- d %>%
  filter(trans_dummy == 1) %>% 
  group_by(country) %>% 
  summarise(yat = max(year))

d <- merge(x = d, y = yat, by = "country", all = TRUE)
d <- d %>% 
  arrange(country, year)

new_last <- d %>%
  filter(year == yat) %>% 
  select(country, year, agr, ag_gdp_combined_estimate_avg)

# Merge 
figa3_dat <- merge(x = newd_hdata, y = new_last, by = c("country", "year"), all = TRUE)
figa3_dat <- figa3_dat %>% 
  arrange(country, year)

# Combine the agr and ag_gdp variables
figa3_dat <- figa3_dat %>% 
  mutate(agr = agr.x,
         agr = ifelse(is.na(agr), agr.y, agr),
         ag_gdp = ag_gdp_combined_estimate_avg.x,
         ag_gdp = ifelse(is.na(ag_gdp), ag_gdp_combined_estimate_avg.y, ag_gdp)
  ) %>% 
  select(-c(agr.x, agr.y, ag_gdp_combined_estimate_avg.x, ag_gdp_combined_estimate_avg.y))

median(figa3_dat$ag_gdp)

# Histogram
figa3 <- ggplot(data = figa3_dat, aes(x = ag_gdp, y = ..density..)) +
  geom_histogram(fill = "grey", color = "black", size = 0.4) +
  geom_line(stat = "density", size = .7) +
  geom_vline(aes(xintercept = median(ag_gdp)), linetype = "dashed", size = .7)+
  theme_bw()+
  xlab("Agriculture, value added (% of GDP)")+
  ylab("Density")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10))

ggsave("figurea3.jpg", plot = figa3, height = 4.5, width = 6.5, units = "in")


#################
### Figure A4 ###
#################

# See separate R script: FigureA4_Script.R


#################
### Figure A5 ###
#################

# Decline in the proportion of land-oriented countries over time with all three thresholds

# Figure 1 - Appendix version
df1 <- dat %>% 
  filter(!is.na(land_oriented_low_binary) & year < 2014) %>% 
  group_by(year) %>% 
  summarise(n_countries = n(),
            n_land_oriented_low = sum(land_oriented_low_binary)) %>% 
  mutate(prop_land_oriented_low = n_land_oriented_low / n_countries) %>% 
  select(year, prop_land_oriented_low)

df2 <- dat %>% 
  filter(!is.na(land_oriented_high_binary) & year < 2014) %>% 
  group_by(year) %>% 
  summarise(n_countries = n(),
            n_land_oriented_high = sum(land_oriented_high_binary)) %>% 
  mutate(prop_land_oriented_high = n_land_oriented_high / n_countries) %>% 
  select(year, prop_land_oriented_high)

df <- merge(x = df, y = df1, by = "year", all.x = TRUE)
df <- merge(x = df, y = df2, by = "year", all.x = TRUE)

figa5 <- ggplot(data = df, aes(x = year, y = prop_land_oriented)) +
  geom_line(size = .8) +
  geom_line(aes(x = year, y = prop_land_oriented_high), size = .8, alpha = .6) +
  geom_line(aes(x = year, y = prop_land_oriented_low), size = .8, alpha = .6) +
  annotate("text", x = 2015, y = 0.5341615, label = "Best estimate", size = 2.3, vjust = 1, hjust = .55) +
  annotate("text", x = 2015, y = 0.4276730, label = "High threshold", size = 2.3, vjust = 1, hjust = .6) +
  annotate("text", x = 2015, y = 0.6380368, label = "Low threshold", size = 2.3, vjust = 1, hjust = .6) +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(1825, 2025, 25)) +
  labs(x = "Year", y = "") +
  ggtitle("Proportion of Land-oriented Countries") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, hjust = 0.5))

ggsave("figurea5.jpg", plot = figa5, height = 4.5, width = 6.5, units = "in")


#################
### Figure A6 ###
#################

# Distribution of the continuous land-orientation measure

figa6 <- ggplot(data = dat, aes(x = land_oriented_medium_continuous)) +
  geom_histogram(bins = 50, fill = "grey", color = "black", size = .7) +
  geom_vline(aes(xintercept = mean(land_oriented_medium_continuous, na.rm = TRUE)), 
             linetype = "dashed", size = .7) +
  labs(x = "Land-orientation (continuous)", y = "Count") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 10))

ggsave("figurea6.jpg", plot = figa6, height = 4.5, width = 6.5, units = "in")
