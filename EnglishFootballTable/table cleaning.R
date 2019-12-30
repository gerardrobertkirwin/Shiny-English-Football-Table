#Download Data ------------------------------------------------------------

table_download <- readr::read_csv("Sheet_2_data.csv")


# Upload packages ---------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(tibble)

# Clean up Data -----------------------------------------------------------
table_download$`Away (A )`[is.na(table_download$`Away (A )`)] = 0
table_download$`Away (D )`[is.na(table_download$`Away (D )`)] = 0
table_download$`Away (F )`[is.na(table_download$`Away (F )`)] = 0
table_download$`Away (L )`[is.na(table_download$`Away (L )`)] = 0
table_download$`Away (W )`[is.na(table_download$`Away (W )`)] = 0

table_download$`Home (A )`[is.na(table_download$`Home (A )`)] = 0
table_download$`Home (D )`[is.na(table_download$`Home (D )`)] = 0
table_download$`Home (F )`[is.na(table_download$`Home (F )`)] = 0
table_download$`Home (L )`[is.na(table_download$`Home (L )`)] = 0
table_download$`Home (W )`[is.na(table_download$`Home (W )`)] = 0

table_home_away_add <- table_download %>% 
  mutate(W = `Away (W )` + `Home (W )`,
         D = `Away (D )` + `Home (D )`,
         L = `Away (L )` + `Home (L )`,
         F = `Away (F )` + `Home (F )`,
         A = `Away (A )` + `Home (A )` )

table_clean <- table_home_away_add %>% 
  select(Season, Tier, F19, `Relegation?`, Pos, Club, P, W, D, L, F, A, GD, Pts) %>% 
  mutate(`Away (A )`= NULL,
         `Away (D )` = NULL,
         `Away (F )`= NULL,
         `Away (L )`= NULL,
         `Away (W )` = NULL,
         `Home (A )` = NULL,
         `Home (D )` = NULL,
         `Home (F )` = NULL,
         `Home (L )` = NULL,
         `Home (W )`= NULL, `Number of Records` = NULL,
         Sheet = NULL, `Table Name` = NULL)

table_season_tiers <- table_clean %>% 
  mutate(Tier=str_replace_all(Tier, "First tier", '1')) %>% 
  mutate(Tier=str_replace_all(Tier, "Second tier", '2')) %>% 
  mutate(Tier=str_replace_all(Tier, "Third tier", '3')) %>% 
  mutate(Tier=str_replace_all(Tier, "Fourth tier", '4')) %>% 
  arrange(Season, Tier, Pos) %>% 
  within( {
    'Total Position' <- ave(Season, Season, FUN = seq_along)
  })  

table_season_tiers <-  table_season_tiers %>% 
  mutate(`Total Position` = as.numeric(table_season_tiers$`Total Position`))

# you forgot to clean the names up#
table_season_tiers_clean <- table_season_tiers %>% 
  mutate(Club=str_replace_all(Club, " \\(.*\\)", "")) 

table_season_clean <- table_season_tiers_clean %>% 
  mutate(Club=str_replace_all(Club, "Wolverhampton Wndrs", "Wolverhampton Wanderers")) %>% 
  mutate(Club=str_replace_all(Club, "Wolverhampton Wand$", "Wolverhampton Wanderers")) %>% 
  mutate(Club=str_replace_all(Club, "Wolverhampton W$", "Wolverhampton Wanderers")) %>% 
  mutate(Club=str_replace_all(Club, "West Bromwich A$", "West Bromwich Albion")) %>%   
  mutate(Club=str_replace_all(Club, "Shrewsbury$", "Shrewsbury Town")) %>% 
  mutate(Club=str_replace_all(Club, "Scunthorpe & Lindsey Utd", "Scunthorpe United")) %>% 
  mutate(Club=str_replace_all(Club, "Scunthorpe & Lindsey", "Scunthorpe United"))%>% 
  mutate(Club=str_replace_all(Club, "Queen's Park Rangers", "Queens Park Rangers")) %>% 
  mutate(Club=str_replace_all(Club, "Northampton$", "Northampton Town")) %>% 
  mutate(Club=str_replace_all(Club, "Middlesbrough\\*$", "Middlesbrough")) %>% 
  mutate(Club=str_replace_all(Club, "Hartlepools United", "Hartlepool United")) %>% 
  mutate(Club=str_replace_all(Club, "Hartlepool$", "Hartlepool United")) %>% 
  mutate(Club=str_replace_all(Club, "Dagenham and Redbridge", "Dagenham & Redbridge")) %>% 
  mutate(Club=str_replace_all(Club, "Dagenham and Redb'ge", "Dagenham & Redbridge")) %>% 
  mutate(Club=str_replace_all(Club, "CambridgeUnited", "Cambridge United")) %>% 
  mutate(Club=str_replace_all(Club, "Brighton & HA$", "Brighton & Hove Albion")) %>% 
  mutate(Club=str_replace_all(Club, "Brighton & Hove$", "Brighton & Hove Albion")) %>% 
  mutate(Club=str_replace_all(Club, "Brighton and Hove A$", "Brighton & Hove Albion")) %>% 
  mutate(Club=str_replace_all(Club, "Brighton and Hove Albion$", "Brighton & Hove Albion")) %>% 
  mutate(Club=str_replace_all(Club, "Bournemouth & Boscombe", "AFC Bournemouth")) %>% 
  mutate(Club=str_replace_all(Club, "^Bournemouth$", "AFC Bournemouth")) %>% 
  mutate(Club=str_replace_all(Club, "Brentford $", "Brentford")) 

table_season_clean <- table_season_clean %>% mutate_if(is.double, as.character)

table <- table_season_clean %>% 
  mutate(Tier=str_replace_all(Tier, '1',
        case_when(
            Season < '1992-93' ~ "First Division",
            Season >= '1992-93'~ "Premier League"
  ))) %>% 
  mutate(Tier=str_replace_all(Tier, '2',
          case_when(
            Season < '1992-93' ~ "Second Division",
            Season >= '1992-93' & Season < '2004-05' ~ "First Division",
            Season >= '2004-05' ~ "Championship"
  ))) %>% 
  mutate(Tier=str_replace_all(Tier, '3',
          case_when(
            Season < '1992-93' ~ "Third Division",
            Season >= '1992-93' & Season < '2004-05' ~ "Second Division",
            Season >= '2004-05' ~ "League One"
  ))) %>%  
  mutate(Tier=str_replace_all(Tier, '4',
          case_when(
            Season < '1992-93' ~ "Fourth Division",
            Season >= '1992-93' & Season < '2004-05' ~ "Third Division",
            Season >= '2004-05' ~ "League Two"
  ))) 

#write.csv2(table_season_clean, file="sheet3.csv")

#future work to put in promotion/relegation#  
#strsplit(table_season_tiers$Club, " \\(.*\\)", fixed = TRUE)

#calculate "post-war" top 10 by wins#
#table_postwar_bywins <- table_season_clean %>% 
#  group_by(Club) %>% 
#  summarise(total_wins = sum(W)) %>% 
#  arrange(desc(total_wins))

#top10 <- table_postwar_bywins %>% 
#  top_n(10, total_wins) %>% 
#  select(Club)
#top10list <-  as.vector(top10$Club)

# basic visualizations -----------

#success <- table_season_clean %>% filter(Club %in% top10list)

#subset(table_season_clean, Club %in% top10list)

#club_color <- c("Manchester United" = "red3", "Arsenal" = "red4", "Chelsea" = "blue4", "Liverpool" = "red", "Tottenham Hotspur" = "blue3", "Everton" = "blue")


#ggplot(success, aes(x=Season, y=`Total Position`, group=Club)) + 
#  geom_line(aes(colour=Club), size=1) + 
#  geom_point(shape = 1, stroke = 2, size=2, aes(colour=Club)) +
#scale_colour_manual(values=club_color) +
#  scale_y_reverse(breaks = c(1, 20, 40, 60, 80)) +
#  scale_x_discrete(breaks=c("1946-47", "1956-57", "1966-67", "1976-77", "1986-87","1996-97", "2006-07", "2016-17"))




