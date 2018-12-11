pd_2017 <- read.csv('party_distribution.csv')
pd <- pd_2017[1:3] %>%
  mutate(state = tolower(State)) %>%
  select(state, Democrat.Percentage, Republican.Percentage)
names(pd) <- c('state', 'democratic_percentage_2017', 'republican_percentage_2017')
data <- readRDS('our_data.rds')
data <- data %>%
  mutate(sum = count1 + count2) %>%
  mutate(dem_percentage = round(count1 / sum * 100, 0), rep_percentage = round(count2 / sum * 100, 0)) %>%
  select(state, dem_percentage, rep_percentage)
names(data) <- c('state', 'democratic_percentage_app', 'republican_percentage_app')

both <- merge(data, pd, by='state')

chisq.test(x = both$democratic_percentage_app,y = both$democratic_percentage_2017)

# limitations: only states returned in our result (49)
# only a small number of tweets (2471) that we could identify locations of 

# chi-squared = 815.92, df = 768, p-value = 0.1122
# since p-value = 0.1122 is greater than alpha = 0.05, we reject the null hypothesis that the two distributions are different.