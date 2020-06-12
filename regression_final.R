# combine county-level med. income, poverty rate, and Gini index

library(tidyverse)
library(covdata)

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




# Four models in Table 1
model.fit1 <- glm(deathToCase ~ gini_ind, data = comb_data)
coef(model.fit1)
confint(model.fit1, level = 0.95)

model.fit2 <- glm(deathToCase ~ gini_ind + med_income, data = comb_data)
coef(model.fit2)
confint(model.fit2, level = 0.95)

model.fit3 <- glm(deathToCase ~ gini_ind + povRate, data = comb_data)
coef(model.fit3)
confint(model.fit3, level = 0.95)

model.fit4 <- glm(deathToCase ~ gini_ind + povRate + med_income, data = comb_data)
coef(model.fit4)
confint(model.fit4, level = 0.95)

# Display AIC for each model
AIC(model.fit1)
AIC(model.fit2)
AIC(model.fit3)
AIC(model.fit4)

# since model 2 is lowest AIC, use that
summary(model.fit2)
confint(model.fit2,conf.level=0.95)


library(scatterplot3d)
library(RColorBrewer)

# get colors for labeling the points
plotvar <- comb_data$deathToCase # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color





library(rgl)
# scatter plot
pdf("3d-plot.pdf", width = 8, height = 5)
plot.angle <- 45
scatterplot3d(comb_data$med_income, comb_data$gini_ind, 100*plotvar, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=0.5, 
              col.axis="gray", col.grid="gray", ylim=c(min(comb_data$gini_ind),max(comb_data$gini_ind)),
              xlab = "Median Income (US Dollars)", ylab = "Gini Index", zlab = "DCR x 100%")
dev.off()



# plot residual graphs
pdf("multi-resid.pdf", width = 8, height = 3)
model.fit2.resid <- resid(model.fit2)
par(mfrow = c(1, 2))
hist(model.fit2.resid, breaks = 20, xlab = "Residuals", ylab = "Frequency", main = "")
plot(model.fit2, which = 1, pch=".")
dev.off()



## look at Model 1
pdf("model1.pdf", width = 8, height = 3)
par(mfrow = c(1, 3))
# plot a scatterplot, deathToCase vs med_income
x <- comb_data$povRate
y <- comb_data$deathToCase
plot(x, y, pch=".", xlab = "Gini Index", ylab = "DCR x 100%")

# perform linear regression
beta0hat <- model.fit1$coefficients[1]
beta1hat <- model.fit1$coefficients[2]
abline(a = beta0hat, b=beta1hat, col="blue", lwd=2)

model1 <- lm(deathToCase ~ gini_ind, data = comb_data)
summary(model1)

model.fit1.resid <- resid(model.fit1)

hist(model.fit1.resid, breaks = 20, xlab = "Residuals", ylab = "Frequency", main = "")
plot(model.fit1, which = 1, pch=".")
dev.off()
