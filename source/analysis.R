library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
incarceration <- get_data()

black_incarceration_current <- function() {
  now <- incarceration %>%
    filter(year == max(year)) %>%
    summarize(total = sum(black_jail_pop, na.rm = TRUE))
  return(now$total)
}

black_incarceration_max <- function() {
  yearly_incarceration <- incarceration %>%
    group_by(year) %>%
    summarize(sum = sum(black_jail_pop, na.rm = TRUE))
  max <- filter(yearly_incarceration, sum == max(sum, na.rm = TRUE))
  max <- max$year
  return(max)
}

black_incarceration_decade <- function() {
  now <- incarceration %>%
    filter(year == max(year)) %>%
    summarize(total = sum(black_jail_pop, na.rm = TRUE))
  decade_ago <- incarceration %>%
    filter(year == max(year) - 10) %>%
    summarize(total = sum(black_jail_pop, na.rm = TRUE))
  difference <- now$total - decade_ago$total
  return(difference)
}

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function creates a data frame that displays aggregated data of  the United 
# States' total jail populations by year
get_year_jail_pop <- function() {
  jail_by_year <- incarceration %>%
    group_by(year) %>%
    summarize(pop = sum(total_jail_pop, na.rm = TRUE))
  return(jail_by_year)   
}

# This function creates a bar graph of the total jail population over years
plot_jail_pop_for_us <- function()  {
  ggplot(data = get_year_jail_pop()) +
    geom_col(
      mapping = aes(x = year, y = pop)
    ) + 
  labs(
    title = "Increase of Jail Population in U.S. (1970-2018)",
    x = "Year",
    y = "Total Jail Population"
  )
}

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State

# This function creates a data frame that displays aggregated data of the total
# jail populations by inputted states over the years
get_jail_pop_by_states <- function(states) {
  state_pops <- incarceration %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(pop = sum(total_jail_pop, na.rm = TRUE))
  return(state_pops)
}

# This function creates a line graph comparing the jail populations of each state
# over the years
plot_jail_pop_by_states <- function(states) {
  ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(
      mapping = aes(x = year, y = pop, color = state),
      size = 1
    ) +
    labs(x = "Year", y= "Average in Jail", 
         title = "Average Number of People Incarcerated for each State from 1970 to 2018",
         color = "Race")
}

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>

# Average number of black people in jail from 1970 to 2018
black_in_jail_avg <- incarceration %>%
  group_by(year) %>%
  summarize(black = mean(black_jail_pop, na.rm = TRUE))

# Average number of white people in jail from 1970 to 2018
white_in_jail_avg <- incarceration %>%
  group_by(year) %>%
  summarize(white = mean(white_jail_pop, na.rm = TRUE))

# Combining the data frames
in_jail_compare <- left_join(white_in_jail_avg,
                             black_in_jail_avg, by = "year") %>%
  pivot_longer(-c(year), names_to = "race", values_to = "avg_jail")

# Line chart displaying the combined data frame
incarceration_trends <- function() {
  ggplot(data = in_jail_compare) +
  geom_line(
    mapping = aes(x = year, y = avg_jail, color = race),
    size = 1
  ) +
  labs(x = "Year", y= "Average in Jail", 
       title = "Average Number of Black and White Incarcerated from 1970 to 2018",
       color = "Race")
}

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>

# Creating a data frame of current averages of black incarceration by state
black_incarceration <- incarceration %>%
  group_by(state) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  summarize(black_jail_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE))

# Loading States
state_shape <- map_data("state")

# Loading in state names an d abbreviations
state_names <- data.frame(state.abb, state.name)

# Joining data frames
black_state_data <- left_join(black_incarceration, state_names, by = c("state" = "state.abb"))

# Creating a new column with lowercase state names
state_data_lower <- black_state_data %>%
  mutate(region = tolower(state.name))

# Joining state_shape and state_data_lower
state_shape <- left_join(state_shape, state_data_lower)

# Creating a blank theme of the map code
blank_theme <- theme_bw() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
  )

# Creating a black map
black_map <- function() {
  ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop_rate),
    color = "white", size = 0.1
  ) +
  coord_map() +
  scale_fill_continuous(low = "cornflowerblue", high = "darkblue") +
  labs(fill = "Incarceration Rate of Black People",
       title = "Rate of Incarceration for Black People in the U.S. 2018") +
  blank_theme
}
  
#----------------------------------------------------------------------------#


