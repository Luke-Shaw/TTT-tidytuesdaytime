# packages
library(tidyverse)

# read data and check it --------------------------------------------------

# read in the data
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

kids
table(kids$variable)
# Check the data set is tidy and complete
num_combs <- length(unique(kids$variable)) * 
  length(unique(kids$state)) * 
  length(unique(kids$year))
# every variable, state, year has a row
num_combs == nrow(kids)
# Any duplicates? (Answer: no)
kids %>% group_by(state, variable, year) %>% count() %>% filter(n!=1)
# Any blanks? (Answer: yes!)
kids %>% filter(is.na(raw))
# Which are blanks? (Answer: Medicaid and other_health for 1997 for all states)
kids %>% filter(is.na(raw)) %>% group_by(variable, year) %>% count()


# background wrangling for the plot ---------------------------------------

# look at following to work out which variable is of interest:
# https://jrosen48.github.io/tidykids/articles/tidykids-codebook.html

# I'll stick with PK12ed for now - it gets complicated quickly!

# Make a dataframe of the states ranked against each other. 
# inspired by https://twitter.com/JaredBraggins/status/1305782342179418112
rank_df <- kids %>% 
  filter(variable == "PK12ed") %>% 
  group_by(year) %>% 
  arrange(desc(inf_adj_perchild)) %>%
  mutate(rank = row_number()) %>% 
  ungroup()

# choose which state(s) to highlight. Went with one in the end as the plot
# looked messy with more
states_to_highlight <- rank_df %>% 
  group_by(state) %>% 
  filter(year %in% c(1997, 2016)) %>% 
  summarise(diff = max(rank) - min(rank),
            .groups="drop") %>%
  slice_max(n = 1, order_by = diff) %>% 
  pull(state)

# add the highlight variable for plotting
rank_df <- rank_df %>% 
  mutate(highlight = state %in% states_to_highlight)

# text for the RHS of plot showing overall change
start_to_end_text <- rank_df %>% 
  group_by(state, highlight) %>% 
  count() %>% select(state, highlight) %>% 
  left_join(rank_df %>% filter(year == 1997) %>% select(state, rank_1997 = rank),
            by = "state") %>% 
  left_join(rank_df %>% filter(year == 2016) %>% select(state, rank_2016 = rank),
            by = "state") %>% 
  mutate(text = paste0(state, ": ", rank_1997, "-->", rank_2016))


# the rank plot -----------------------------------------------------------

rank_plot <- ggplot(rank_df, aes(x = year, 
                    y = reorder(rank,desc(rank)),
                    group = state)) +
  # a lot of fiddling about trial-and-error here, not the most efficient 
  # I'm sure.
  scale_x_continuous(breaks = seq(1997,2016),
                     # limits bigger than data to fit text either side
                     limits = c(1995, 2019)) + 
  # greens chosen from https://colorbrewer2.org/
  scale_colour_manual(values = c("#c7e9c0", "#005a32")) + 
  # add the lines in - couldn't get one command to work with size nuance so
  # split over two geom_line calls
  geom_line(data = rank_df %>% filter(!highlight),
            aes(col = highlight),
            size = 0.5) +
  geom_line(data = rank_df %>% filter(highlight),
            aes(col = highlight),
            size = 1.5) +
  # same as geom_line for geom_point
  geom_point(data = rank_df %>% filter(!highlight),
             aes(col = highlight),
             size = 1) +
  geom_point(data = rank_df %>% filter(highlight),
             aes(col = highlight),
             size = 2) +
  # text on LHS of plot
  geom_text(data = rank_df %>%  filter(year == 1997),
            aes(label = state, x = 1996.8, col = highlight),
            hjust = "right",
            size=3) +
  # text on RHS of plot, with change overall
  geom_text(data = start_to_end_text %>% mutate(rank=rank_2016),
            aes(label = text, x = 2016.2),
            hjust = "left",
            size = 3) +
  # surroundings of the plot
  labs(title = "Rank of per-child public spending on elementary and secondary education by each state",
       subtitle = "North Dakota highlighted as the largest change in rank over the 19 years",
       x = "Year",
       y = "Rank",
       caption = paste0("#TidyTuesday",
                        "\ndata source: https://tinyurl.com/yyzvwqsw ",
                        "\nplot code: https://tinyurl.com/yxsgmc8z")) +
  theme_bw() +
  # don't need the legend and the y axis gridlines confuse the rank lines
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# save the plot
ggsave("./2020-09-15-state_ranks.png",
       plot = rank_plot,
       width=12, 
       height=7.2)

# code that didn't make it ------------------------------------------------

# library(gganimate)
# library(gridExtra)

# # get states data from ggplot2 package
# us_states <- map_data("state")
# 
# # join together onto our data
# geog_df <- kids %>% 
#   filter(variable == "PK12ed") %>% 
#   mutate(state = tolower(state)) %>%
#   right_join(us_states, by = c("state" = "region"))
# 
# map_plot <- ggplot(geog_df %>% filter(year == 2016), 
#                    aes(x = long,
#                        y = lat,
#                        group = group)) +
#   geom_polygon(aes(fill = inf_adj_perchild)) +
#   scale_fill_gradient(low = "#a1d99b", high = "#00441b") +
#   # add border lines to the states
#   geom_polygon(color = "gray90", size = 0.1, fill = NA) + 
#   theme_bw() +
#   labs(title = "Adjusted per-child expenditure in 2016",
#        fill = "thousands $ spent") +
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
# 
# 
# 
# grid.arrange(rank_plot, map_plot, 
#              widths = c(4,1),
#              nrow = 1,
#              respect=c(FALSE,TRUE))



