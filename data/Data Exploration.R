library(tidyverse)
library(haven)
library(janitor)
library(gmodels)
library(cdlTools)
library(highcharter)
library(ggplot2)


Census.2020_raw <- read.csv("data/2020 Census/Race_2020.csv")

Census.2020 <- as_tibble(cbind(names(Census.2020_raw), t(Census.2020_raw))) %>% 
  row_to_names(1) %>% 
  select(c(1:10)) %>% 
  mutate(across(everything(), ~ str_replace_all(string = .x, 
                                            pattern = c(","),
                                            replacement = "")))
 
colnames(Census.2020) <- colnames(Census.2020) %>% 
  str_trim(side = "both") %>% 
  str_replace("\\.$", "") %>% 
  make.names()


ANES <- read_sav("data/ANES_Cumulative.sav") %>% select(VCF0009z, VCF0004, VCF0102, 
                                                            VCF0105a, VCF0107, VCF0112, 
                                                            VCF0114, VCF0115, VCF0128, 
                                                            VCF0129, VCF0901a, VCF0111)

ANES$State <- sapply(ANES$VCF0901a, cdlTools::fips, to = "Name")


ANES %>% subset(VCF0004 == 2020) %>% group_by(State) %>% count(VCF0105a) 
ANES %>% subset(VCF0004 == 2020) %>% group_by(State) %>% count(State, wt = VCF0105a)

ANES.2020 <- ANES %>% subset(VCF0004 == 2020)

state.pop_cen <- Census.2020[,c(1,2)] %>% 
                 rename(State = Label..Grouping, Census.total = Total.)

state.pop_cen$Census.total <- state.pop_cen$Census.total %>% 
  str_replace_all(",", "") %>% 
  as.numeric()
state.pop_cen$State <- state.pop_cen$State %>% 
  str_replace_all("\\.", " ")


state.pop_cen <- state.pop_cen %>% mutate(
  Census.pct = (Census.total / state.pop_cen$Census.total[state.pop_cen$State == "United States"]) * 100
  )


state.pop_ANES <- ANES.2020 %>% group_by(State) %>% 
                  summarise(ANES.total = n()) %>%
                  mutate(ANES.pct = (ANES.total / nrow(ANES.2020)) * 100)

weighted.state.pop_ANES <- ANES %>% 
  subset(VCF0004 == 2020) %>% 
  group_by(State) %>% 
  count(State, wt = VCF0009z) %>% 
  mutate(ANES.pctWT = (n / 8280) * 100) %>% 
  rename(n.WT = n)
sum(weighted.state.pop_ANES$ANES.pctWT)


joined.pop <- full_join(weighted.state.pop_ANES, state.pop_ANES) %>% 
  full_join(state.pop_cen)


joined.pop$Pop_Difference <- (joined.pop$ANES.pct - joined.pop$Census.pct) %>% round(digits = 3)

joined.pop$Pop_Difference.wt <- (joined.pop$ANES.pctWT - joined.pop$Census.pct) %>% round(digits = 3)
# Integrate this into main code
joined.pop <- joined.pop %>% rename(ANES.totalWT = n.WT)


test.join <- test.join[c(1:51),]
hcmap(
  "countries/us/us-all",
  showInLegend =F,
  data = new.data,
  value = 2,
  name = "State"
)

new.data <- test.join %>% select(
  State,
  Pop_Difference,
  Pop_Difference.wt,
  Black_Difference,
  Black_Difference.WT
)

new.data %>% select(c(starts_with("Pop"))) %>% select(ends_with("wt"))

joined.pop$Pop_Difference
test.join$Pop_Difference

hcmap(
  "countries/us/us-all",
  showInLegend =F,
  data = joined.pop,
  value = "Difference.wt",
  name = "State"
)

hcmap("countries/us/us-all", showInLegend = F) %>% 
  hc_add_series(
    data = joined.pop,
    type = "mappoint",
    name = "States",
  )


joined.pop %>% group_by(State) %>% ggplot(aes(x = Difference)) + 
  geom_bar()

highchart(type = "stock") %>% 
  hc_add_series(joined.pop$Difference, type = "column")

highchart(type = "stock") %>% 
  hc_add_series(joined.pop$Difference.wt, type = "column")

summary(joined.pop$Difference)
summary(joined.pop$Difference.wt)

saveRDS(test.join, file = "data/Joined_Pop.rds")
saveRDS(new.data, file = "data/test_data.rds")

Census.Race <- read.csv("C:/Users/tjmay/Downloads/DECENNIALPL2020.P1-2024-08-23T173025.csv")

Census.Race <- Census.Race %>% subset(Total. != "")

Census.Black <- Census.Race %>% select(c(1,2,3,5))
colnames(Census.Black) <- c("State", "Total", "One_Race", "One_Race.Black")
Census.Black$One_Race <- Census.Black$One_Race %>% str_replace_all(",", "")
Census.Black$One_Race <- as.numeric(Census.Black$One_Race)

Census.Black$One_Race.Black <- Census.Black$One_Race.Black %>% str_replace_all(",", "")
Census.Black$One_Race.Black <- as.numeric(Census.Black$One_Race.Black)
  


Census.Black <- Census.Black %>% mutate(Black.pct = (Census.Black$One_Race.Black / Census.Black$One_Race))
Census.Black$Black.pct <- Census.Black$Black.pct %>% round(digits = 3)



ANES %>% subset(VCF0004 == 2020) %>% group_by(State) %>% count(VCF0105a) %>% View()
ANES %>% subset(VCF0004 == 2020) %>% group_by(State) %>% count(State, wt = VCF0009z)
ANES %>% subset(VCF0004 == 2020) %>% group_by(State) %>% count(VCF0105a, wt = VCF0009z)

ANES %>% subset(VCF0004 == 2020) %>% group_by(State) %>% count(State)

ANES.Black <- ANES %>% subset(VCF0004 == 2020) %>% 
              group_by(State) %>% 
              count(VCF0105a) %>% 
              filter(VCF0105a == 2) %>% 
              rename(ANES_Total.Black = n)
ANES.Total <- ANES %>% subset(VCF0004 == 2020) %>% 
              group_by(State) %>% 
              count(State) %>% 
              rename(ANES_Total.State = n)

ANES.JoinedBlack <- full_join(ANES.Black, ANES.Total) %>% 
  mutate(ANES_PCT.Black = round((ANES_Total.Black / ANES_Total.State), digits = 3))

Census.Black$State <- Census.Black$State %>% str_replace("^\\s+", "") 
Census.Black <- Census.Black %>% 
  rename(Census.Black_pct = Black.pct)

ANES.JoinedBlack <- full_join(ANES.JoinedBlack, Census.Black)


ANES.Black_WT <- ANES %>% 
  subset(VCF0004 == 2020) %>% 
  group_by(State) %>% 
  count(VCF0105a, wt = VCF0009z) %>% 
  filter(VCF0105a == 2) %>% 
  rename(ANES_Total.BlackWT = n)
ANES.State_WT <- ANES %>% 
  subset(VCF0004 == 2020) %>% 
  group_by(State) %>% 
  count(State, wt = VCF0009z) %>% 
  rename(ANES_Total.StateWT = n)

ANES.JoinedBlack <-  full_join(ANES.JoinedBlack, ANES.Black_WT) %>% 
  full_join(ANES.State_WT) %>% 
  mutate(ANES_Black.pct.wt = ANES_Total.BlackWT/ANES_Total.StateWT)

test.join <- full_join(joined.pop, ANES.JoinedBlack)
test.join$ANES_PCT.Black[is.na(test.join$ANES_PCT.Black)] <- 0
test.join$ANES_Black.pct.wt[is.na(test.join$ANES_Black.pct.wt)] <- 0

test.join <- test.join %>% mutate(
  Black_Difference = (Census.Black_pct - ANES_PCT.Black),
  Black_Difference.WT = (Census.Black_pct - ANES_Black.pct.wt)
)
