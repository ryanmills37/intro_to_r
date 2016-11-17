install.packages("ggplot2")
install.packages("data.table")
install.packages("reshape2")
library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)


load("suicides.rdata")

suicides$age <- as.factor(suicides$age)

all_suicides <- copy(suicides)
suicides <- suicides %>% 
  group_by(year, age, sex) %>% 
  mutate(deaths = sum(deaths))

#  Make a line plot of suicides by age
# (year on the x axis, deaths on the y axis, different line for each age).
# facet by sex.
plotSuicide <- ggplot(suicides, aes(x=year, y=deaths, color=means)) +
                geom_point(size=3) +
                facet_wrap(~year, scales="free")

##extra credit####

one_state <- all_suicides[all_suicides$state=="Uttar Pradesh"] %>% 
  group_by(year, state, sex, age, means) %>% 
  mutate(deaths = sum(deaths))

# Make a set of density plots faceted by sex and means of suicide,
# showing distributions of suicides by age, for the state of Uttar Pradesh.
# Label appropriately.

densityPlot <- ggplot(one_state, aes(x=age, y=state)) +
  geom_density(aes(color=means, fill=means), size=1, alpha=0.5) +
  facet_grid(sex~means, scales="free") +
  labs(title="Distribution of suicides by age, for Uttar Pradesh",
       x="Deaths",
       y="Density")


