# packages
library(tidyverse)
library(lubridate)
library(janitor)
library(ggtext)

# read data and check it --------------------------------------------------

# read in the data
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

# clean date cols
netflix_titles <- netflix_titles %>% 
  mutate(date_added = as_date(date_added, format="%B %d, %Y"))

glimpse(netflix_titles)
summary(netflix_titles)


# shaws plot --------------------------------------------------------------

# add any one with a surname starting "Shaw"
netflix_titles <- netflix_titles %>% 
  mutate(shaw_in_cast = str_detect(cast," Shaw"))

netflix_shaws_toplot <- netflix_titles %>% 
  filter(!is.na(shaw_in_cast)) %>% 
  group_by(shaw_in_cast) %>% 
  janitor::tabyl(shaw_in_cast, type) %>%
  janitor::adorn_percentages(denominator="row") %>%
  #  adorn_ns() %>% 
  pivot_longer(cols=c("Movie","TV Show"), names_to="type")

ggplot(netflix_shaws_toplot,
       aes(x=shaw_in_cast, 
           y=value, 
           fill=type,
           label=paste0(round(value*100,1),"%"))) + 
  geom_bar(stat="identity") + 
  geom_label(position = position_stack(vjust=0.5)) +
  labs(x="A Shaw in the cast?",
       y = "%",
       title = "Shaws are proportionally in more Movies on Netflix than the non-Shaws",
       subtitle = paste0("I even did a stat test and got a p-value less than 0.05",
                         " so it **must** be causal /s"),
       fill="",
       caption=paste0("Netflix data source: https://tinyurl.com/3667a8wc",
                      "\n#TidyTuesday @lukefshaw")) +
  theme_bw() + 
  guides(fill=guide_legend(nrow=1)) +
  theme(plot.subtitle = ggtext::element_markdown(),
        legend.position=c(0.1,-0.1))

ggsave("2021-04-20-netflix-shaws.png")

# shaws chi-squared  ------------------------------------------------------

# Chi-squared test
netflix_titles %>% 
  filter(!is.na(shaw_in_cast)) %>% 
  group_by(shaw_in_cast) %>% 
  janitor::tabyl(type,shaw_in_cast) %>%
  chisq.test(correct=FALSE)
