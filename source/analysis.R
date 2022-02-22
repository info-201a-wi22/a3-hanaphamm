# Download datasets
`jurisdiction_level` <- read.csv ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv") 
`incarceration_trends` <- read.csv ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Introduction & Summary Info: at least 5 relevant values of interest 
# 1. What is the maximum prison population among the black community from age 
# 15-64? 
  `max_black` <- incarceration_trends %>%
    drop_na() %>%
    select(black_pop_15to64) %>%
    filter(black_pop_15to64 == max(black_pop_15to64)) %>%
    pull(black_pop_15to64) 
     
# 2. What is the maximum prison population among the white community of age
# 15-64 at the county level? 
  `max_white` <- incarceration_trends %>%
    drop_na() %>%
    select(white_pop_15to64) %>%
    filter(white_pop_15to64 == max(white_pop_15to64)) %>% 
    pull(white_pop_15to64)
  
# 3. What is the difference between the maximum prison populations of the white 
# and black male community?  
  `diff_white_black` <- (max_white) - (max_black) 
  
# 4. What is the maximum population of Asian American and 
# Pacific Islanders ages 15 to 64? What's the difference between this number and
# the maximum white prisoner population? Store the difference in `diff_aapi_white`
  `max_aapi` <- incarceration_trends %>%
    drop_na() %>%
    select(aapi_pop_15to64) %>% 
    filter(aapi_pop_15to64 == max(aapi_pop_15to64)) %>%
    pull(aapi_pop_15to64)
    
  `diff_aapi_white` <- (max_white) - (max_aapi)
  
# 5. What was the maximum Latinx prison population ages 15 to 64? What is
# the difference between this number and maximum white prison population? Store in 
# `diff_latinx_white`
  `max_latinx` <- incarceration_trends %>%
    drop_na() %>%
    select(latinx_pop_15to64) %>%
    filter(latinx_pop_15to64 == max(latinx_pop_15to64)) %>%
    pull(latinx_pop_15to64)
  
  `diff_latinx_white` <- (max_white) - (max_latinx)

# 6. Which county experienced the highest population of black people in prison, of 
# the most recent date? From that county, what was the highest population of 
# white prisoners (store in `highest_county_white`)? 
  `highest_county_black` <- incarceration_trends %>%
    drop_na() %>%
    select(county_name, black_pop_15to64, year) %>% 
    filter(year == max(year)) %>%
    filter(black_pop_15to64 == max(black_pop_15to64)) %>% 
    pull(county_name) 
  
  `highest_county_white` <- incarceration_trends %>%
    select(county_name, white_pop_15to64, year) %>% 
    filter(year == max(year)) %>%
    filter(county_name == "New York County") %>%
    filter(white_pop_15to64 == max(white_pop_15to64)) %>%
    pull(white_pop_15to64)
    
# 6. Which county experienced the lowest population of black people in prison, 
# of the most recent date? 
    `lowest_county` <- incarceration_trends %>%
      drop_na() %>%
      select(county_name, black_pop_15to64, year) %>%
      filter(year == max(year)) %>%
      filter(black_pop_15to64 == min(black_pop_15to64)) %>%
      pull(county_name)

# 7. What is the ratio of maximum Black prisoners to the maximum total prisoners in a 
# location? May need to do in several steps. Store final answer in `final_ratio`
    `max_total` <- incarceration_trends %>%
      select(total_pop) %>%
      filter(total_pop == max(total_pop)) %>%
      pull(total_pop) 
    
    `final_ratio` <- (max_black) / (max_total)

# 8. What is the ratio of maximum White prisoners to the maximum total prisoners?
    `white_ratio` <- (max_white) / (max_total)
    
# Trends Over Time Chart
  `trend_of_white_vs_black_population` <- incarceration_trends %>%
    drop_na() %>%
    select(black_pop_15to64, white_pop_15to64, year) %>%
    gather(key = race, value = population, -year)

    `trend_chart` <- ggplot(trend_of_white_vs_black_population) + 
      geom_col(mapping = aes(x = year, y = population, fill = race)) +
      labs(title = "Trends of White vs. Black Prison Populations", 
           x = "Year", y= "Population Number", fill = "Race") 
      

# Variable Comparison Chart: show how 2 different continuous variables 
# are related -> use scatterplot 
  `white_vs_latinx_population` <- incarceration_trends %>% 
    drop_na() %>%
    select(white_pop_15to64, latinx_pop_15to64) 
   
  `variable_comparison` <- ggplot(white_vs_latinx_population) + 
    geom_point(mapping = aes(x= white_pop_15to64, y = latinx_pop_15to64)) + 
    labs(title = "Latinx vs. White Prisoner Population",
         x = "White Population", y ="Latinx Population") +
    scale_x_continuous(limits = c(0, 100500)) +
    scale_y_continuous(limits= c(0, 100500))
    
# Map: show how a variable is distributed geographically
 `black_prisoner_only` <- incarceration_trends %>%
   drop_na() %>%
   select(black_pop_15to64, state) 
 
 `blank_theme` <- theme_bw() +
   theme(
     axis.line = element_blank(),
     axis.text = element_blank(), 
     axis.ticks = element_blank(),
     axis.title = element_blank(),
     plot.background = element_blank(),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank()
   )
 
 `state_shape` <- map_data("state") %>%
   left_join(black_prisoner_only, by ="state") %>%
   ggplot(state_shape) + 
   geom_polygon(mapping = aes(x = long, y = late, group = group), 
                color = "white",
                size = .1) +
   coord_map() + 
   scale_fill_continuous() +
   labs(fill ="Black Prisoner Population") + 
   blank_theme 
    
  
