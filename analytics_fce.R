
# Loading pakcages
library(tidyverse)
library(lubridate)

# Loading data
standard <- read_delim("EloActualStd.txt")
rapid <- read_delim("EloActualRpd.txt")


# Adding time-stamp + type of ELO
standard$date <- ym("2021-12")
standard$type <- "standard"
rapid$date <- ym("2021-12")
rapid$type <- "rapid"


# All together + type as factor
cat <- rbind(standard, rapid)

cat$type <- as.factor(cat$type)


# Re-writting the category variable to more grouped 
cat %>% 
  mutate(categoria2 = case_when(categoria %in% c("Sub-8", "Sub-10", "Sub-12") ~ "01 - Sub-12",
                                categoria %in% c("Sub-14", "Sub-16", "Sub-18", "Sub-20") ~ "02 - Sub-20",
                                categoria %in% c("Sènior","Sènior-50") ~ "03 - Senior",
                                categoria == "Veterà-65" ~ "04 - Veteran",
                                TRUE ~ "check")) -> cat
  

# Some quick plots to analyze
cat %>% 
  ggplot(aes(x=elo, color = categoria2)) + geom_density() +
  facet_wrap(~ type)


# Now want to see the percentiles of distribution for my categoria
cat %>% 
  group_by(type, categoria2) %>% 
  mutate(decil_rank = ntile(elo,10)) -> cat

cat %>% 
  filter(categoria2 == "03 - Senior") %>% 
  ggplot(aes(x = elo, color = type)) + geom_density() +
  geom_vline(x=rank)


cat %>% 
  group_by(categoria2, type, decil_rank) %>% 
  summarise(avg_elo = mean(elo),
            min_elo = min(elo),
            max_elo = max(elo),
            median_elo = median(elo)) -> elos_by_cat



