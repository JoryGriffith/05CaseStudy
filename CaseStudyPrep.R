library(shiny)
library(vroom)
library(tidyverse)

dir.create("neiss")
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")
# Downloading data from the internet with a download function
# These are tab separated values
# Downloading this data will put it in the working directory so I can then upload it into my R script

# Vroom is able to read your zipped files without unzipping them, which saves time and space
injuries <- vroom::vroom("module 5/neiss/injuries.tsv.gz")
injuries
# Each row represents a single accident with 10 variables
# trmt_date is the date the person was seen in the hospital (not the date the accident occured)
# body_part is the location of the injury on the body (like ankle or ear)
# loaction is the place where the accident occured
# diag is the basic disgnosis of the injury
# prod_code is the primary product associated with the injury
# weight is the statistical weight giving the estimated number of people who would suffer this injury if the 
  # dataset was scaled up to the entire population of the US
# narrative is a brief story about how the accident occured

# The products let us look up the product name from the product code
products <- vroom::vroom("module 5/neiss/products.tsv")
products

# population tells us the total US population in 2017 for each combination of age and sex
population <- vroom::vroom("module 5/neiss/population.tsv")
population

### Exploration ###
# We will start by looking at a product with an interesting story: 649: "toilets"
selected <- injuries %>% filter(prod_code == 649)
nrow(selected)
#> [1] 2993

# we are going to count based on location and weight it
selected %>% count(location, wt = weight, sort = TRUE)

# by body part
selected %>% count(body_part, wt = weight, sort = TRUE)

# by diagnosis
selected %>% count(diag, wt = weight, sort = TRUE)

# we can also explore patterns across age and sex
# note: this is a terrible variable name because it is also a command
summary <- selected %>% 
  count(age, sex, wt = weight) # still working with toilet injuries
summary

summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

# one problem with interpreting this apttern is that we know that there are fewer older people than younger people
# so the population avaiable to be injured is smaller. 
# we can control for this by comparing the number of people injured with the total population and calculate an injury rate
# here we use rate per 10,000
summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)
# joining our summary data with population by age and sex
# dividing the rate by the population*10,000

summary
# note that the population data only goes up to 84 while injury data goes to 105
summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")

# Finally, we can look at some of the narratives
# Here is a random sample of 10
selected %>% 
  sample_n(10) %>% 
  pull(narrative)




















