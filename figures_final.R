# 4 plots: DCR, median income, poverty rate, Gini

library(tidyverse)
library(covdata)
library(viridis)

library(tidycensus)
census_api_key("your API here", install = TRUE)

# take US county-level data from the NYTimes, and we need to only get the most recent tallies
unt_nyt <- nytcovcounty 
treat_nyt <- unt_nyt %>% filter(date == "2020-05-28")

# Since NYTimes lumps together New York, Kings, Queens, Bronx and Richmond counties
# it doesn't have a FIPS county code, so we just give it a code of 99999.
treat_nyt$fips [treat_nyt$county == "New York City"] = "99999"

# There are also state-wide "unknown" non-county specific data, so we just get remove these 
# because we don't know what county it belongs to.
treat_nyt <- treat_nyt[treat_nyt$county != "Unknown", ] 

# We now calculate death-to-case ratio (deaths/cases), as a new column 
treat_nyt$deathToCase <- treat_nyt$deaths / treat_nyt$cases

# keep data from counties with a large enough sample size of cases
treat_nyt <- treat_nyt[treat_nyt$cases >= 30, ]

# get American Community Survey median income at county-level, 2018
income <- get_acs(geography = "county", variables = "B19013_001", year = 2018)
# now, we have to merge the New York, Kings, Queens, Bronx and Richmond counties/boroughs
# we make a new row, with county code 99999 and income as the average of the 5 borough incomes
NYC_avg_inc <- (income$estimate[income$GEOID == "36061"] + income$estimate[income$GEOID == "36047"]
               + income$estimate[income$GEOID == "36081"] + income$estimate[income$GEOID == "36005"]
               + income$estimate[income$GEOID == "36085"])/5
income <- income %>% add_row(GEOID = "99999", estimate = NYC_avg_inc)
income <- income %>% rename(med_income = estimate)
income <- income %>% rename(fips = GEOID)


# get American Community Survey poverty rate at county-level, 2018
# get the ACS variables for poverty population and total pop.
pop <- get_acs(geography = "county", variables = "B17001_001", year = 2018)
pov_pop <- get_acs(geography = "county", variables = "B17001_002", year = 2018)
pop <- pop %>% rename(population = estimate)
pov_pop <- pov_pop %>% rename(povpop = estimate)
treat_pov <- pop
treat_pov$povpop <- pov_pop$povpop
# We now calculate poverty rate (ppl in poverty/population), as a new column 
treat_pov$povRate <- treat_pov$povpop / treat_pov$population
# now, we have to merge the New York, Kings, Queens, Bronx and Richmond counties/boroughs
# we make a new row, with county code 99999 and corresponding total NYC poverty rate
NYC_Pop <- (treat_pov$population[treat_pov$GEOID == "36061"] + treat_pov$population[treat_pov$GEOID == "36047"]
            + treat_pov$population[treat_pov$GEOID == "36081"] + treat_pov$population[treat_pov$GEOID == "36005"]
            + treat_pov$population[treat_pov$GEOID == "36085"])
NYC_povPop <- (treat_pov$povpop[treat_pov$GEOID == "36061"] + treat_pov$povpop[treat_pov$GEOID == "36047"]
               + treat_pov$povpop[treat_pov$GEOID == "36081"] + treat_pov$povpop[treat_pov$GEOID == "36005"]
               + treat_pov$povpop[treat_pov$GEOID == "36085"])
treat_pov <- treat_pov %>% add_row(GEOID = "99999", povRate = NYC_povPop/NYC_Pop)
treat_pov <- treat_pov %>% rename(fips = GEOID)



# get American Community Survey Gini index at county-level, 2018
gini <- get_acs(geography = "county", variables = "B19083_001", year = 2018)
# now, we have to merge the New York, Kings, Queens, Bronx and Richmond counties/boroughs
# we make a new row, with county code 99999 and income as the average of the 5 borough incomes
NYC_avg_gini <- (gini$estimate[gini$GEOID == "36061"] + gini$estimate[gini$GEOID == "36047"]
                + gini$estimate[gini$GEOID == "36081"] + gini$estimate[gini$GEOID == "36005"]
                + gini$estimate[gini$GEOID == "36085"])/5
gini <- gini %>% add_row(GEOID = "99999", estimate = NYC_avg_gini)
gini <- gini %>% rename(gini_ind = estimate)
gini <- gini %>% rename(fips = GEOID)


# Combine the income/poverty rate/etc data with the case data. We ignore the counties with no covid cases.
# We only keep the columns with fips, deathToCase, and the income variables
comb_data <- merge(treat_nyt, income, by = "fips")
comb_data <- merge(comb_data, treat_pov, by = "fips")
comb_data <- merge(comb_data, gini, by = "fips")
keeps <- c("fips", "county", "state", "deathToCase", "med_income", "povRate", "gini_ind")
comb_data <- comb_data[keeps]

## we have now gathered our data

# remove outliers
#outliers <- boxplot(comb_data$deathToCase, plot=FALSE)$out
#comb_data <- comb_data[-which(comb_data$deathToCase %in% outliers),]

# Remove row for Rio Arriba County, New Mexico - 35039, which had a data collection error
# https://www.census.gov/programs-surveys/acs/technical-documentation/errata/125.html
comb_data <- comb_data[comb_data$fips != "35039", ]














# code worked from https://github.com/pdil/usmap/blob/master/resources/examples.R
library(usmap)
library(ggplot2)

citypop_t <- usmap_transform(citypop)

# poverty rate map
pov_rate_map <-
  plot_usmap(data = treat_pov, values = "povRate", color = "white", size = 0) + 
  scale_fill_continuous(name = "Poverty Rate", label = scales::comma, low = "white", high = "darkgreen") +
  theme(legend.position = "right")

# median income map
med_inc_map <-
  plot_usmap(data = income, values = "med_income", color = "white", size = 0) + 
  scale_fill_continuous(name = "Median Income", label = scales::comma, low = "black", high = "white") +
  theme(legend.position = "right")

# Gini Index rate map
gini_ind_map <-
  plot_usmap(data = gini, values = "gini_ind", color = "white", size = 0) + 
  scale_fill_continuous(name = "Gini Index", label = scales::comma, low = "white", high = "darkred") +
  theme(legend.position = "right")

# DCR map
dcr_map <-
  plot_usmap(data = treat_nyt, values = "deathToCase", color = "white", size = 0) + 
  scale_fill_continuous(name = "DCR", label = scales::comma, low = "white", high = "darkblue") +
  theme(legend.position = "right")


# Combine plots ####
cowplot::plot_grid(
  dcr_map,
  gini_ind_map,
  med_inc_map,
  pov_rate_map,
  nrow = 2
)

ggsave("map-plots.png", width = 6, height = 4, units = "in")



# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)

# Nice visualization of correlations
comb_data_new_names <- comb_data %>% rename(DCR = deathToCase)
comb_data_new_names <- comb_data_new_names %>% rename(Med_Income = med_income)
comb_data_new_names <- comb_data_new_names %>% rename(Pov_Rate = povRate)
comb_data_new_names <- comb_data_new_names %>% rename(Gini_Index = gini_ind)
ggcorr(comb_data_new_names, method = c("pairwise", "pearson"), label = TRUE, legend.size=5) 

ggsave("corr-matrix.png", width = 5, height = 4, units = "in")
