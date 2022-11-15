library(plotly)

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

datapoly <- merge(values, positions, by=c("id"))

p <- ggplot(datapoly, aes(x=x, y=y)) + geom_polygon(aes(fill=value, group=id))

fig <- ggplotly(p)

fig











```

***
  
  ### Material Added Wed. 4/20/2022
  
  * On Tuesday (4/19), a student asked me about creating state maps

* Together (during office hours) we drafted a map of their project data.

* There were details that I knew would take more time...

+ Yesterday, I tinkered...

+ Creating a composed visualization takes time

* This code could be useful for any project that has data by state. 

* For example

+ Average costs and expenditures by state

+ Demographics

+ Voting records

+ Sports/Arts/Entertainment investments and expenditures

+ etc.

* State data and map could also be filtered to a region for which you have data, such as:
  
  + Northeastern States


***
  
  ### US State Maps
  
  * `state_stats` is a great dataset in the `usdata` package

* state labels were created using the abbreviations from `state_stats`


```{r combine state polygons with state population data from R}

# state polygons (from R)
us_states <- map_data("state") |>
  select(long:region) |>
  rename("state" = "region")

# many useful variables in this dataset
state_abbr <- state_stats |>
  select(state, abbr) |>
  mutate(state = tolower(state))

# data by county (aggregated by state)
state_pop <- county_2019 |>
  select(state, pop) |>
  mutate(state=tolower(state),
         popM = pop/1000000) |>
  group_by(state) |>
  summarize(st_popM = sum(popM, na.rm=T)) |>
  full_join(state_abbr)


# used left join because the lat and long info 
# is missing for Hawaii and Alaksa
statepop_map <- left_join(us_states, state_pop) 

```

***
  
  ### Adding State Midpoint (centroid) Lat and Long
  
  * Using the median of each state lat and long data did not work 

+ States are oddly shaped and small.

* Instead I googled *'state midpoint lat and long'* and found this [website](https://www.latlong.net/category/states-236-14.html)

+ Centroid is another term for midpoint

+ Copied data and pasted into Excel

+ Saved as .csv file named `state_coords.csv` (included)

+ `state_coords` did not include DC so I googled that too and added it.

+ Added `state_coords` to `state_match` to verify agreement and added DC there.

- New dataset created: `state_match_check`

+ Final dataset for plot created: `state2019pop_map`


```{r add lat and long of state midpoints (centroid)}

# https://www.latlong.net/category/states-236-14.html

state_coords <- read_csv("state_coords.csv", show_col_types = F,
                         col_names = c("state", "m_lat", "m_long")) |>
  mutate(state = gsub(", USA", "", state, fixed=T),
         state = gsub(", the USA", "", state, fixed=T),
         state = gsub(", the US", "", state, fixed=T),
         state = tolower(state))

# save values for dc
state <- "district of columbia"
m_lat <- 38.9072
m_long <- -77.0369

# create dc dataset with 1 observation
dc <- tibble(state, m_lat, m_long)

# add dc to state_coords
state_coords <- bind_rows(state_coords, dc)

# remove dc values and tibble from Global Environment
# not required but useful for keeping Global Environment organized
rm(dc, state, m_lat, m_long)

statepop_map <- left_join(statepop_map, state_coords)

```

***
  
  ### State Population Plot
  
  * Similar to plots from Tuesday with a few changes

+ Added borders to states by adding `color="darkgrey"` to `geom_polygon` command.

+ Used State abbreviations for state labels.

+ Made State text labels smaller (Size = 2)

+ Changed breaks for log scaled population legend

* These details seem minor but they take time and trial and error.

* There was an engagement question asking for additional clarification on the log transformation 

+ I show both the Europe and US maps with and without it to clarify the benefit

+ If you have right-skewed data, a log transformation is very helpful.

+ Reminder: `log` in R is LN, Natural Log

### US State Pop. Map 

```{r us states pop map no transformation}

# plot of un-logged data
# transformation and breaks statement added

(st_pop <- statepop_map |>
    ggplot(aes(x=long, y=lat, group=group, fill=st_popM)) +
    geom_polygon(color="darkgrey") +
    
    theme_map() +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    
    scale_fill_continuous(type = "viridis")+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm")) +
    
    # add state abbreviations with shadowtext
    # size determined by trial and error
    geom_shadowtext(aes(x=m_long, y=m_lat,
                        label=abbr),
                    color="white",
                    check_overlap = T,
                    show.legend = F,
                    size=2) + 
    
    labs(fill= "Pop. in Millions", title="Population by State",
         subtitle="Unit is 1 Million People",
         caption= "Not Shown: HI: 1.42 Million   AK: 0.74 Million
         
         Data Source: https://CRAN.R-project.org/package=usdata"))

```

***
  
  ### US State Pop. Map with Log (LN) Transformation
  
  ```{r us states pop map with log transformation, fig.width=10, fig.height=6}

# plot of logged data
# transformation and breaks statement added

(st_lpop <- statepop_map |>
    ggplot(aes(x=long, y=lat, group=group, fill=st_popM)) +
    geom_polygon(color="darkgrey") +
    
    theme_map() +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    
    scale_fill_continuous(type = "viridis", trans="log",
                          breaks=c(0,1,2,3,5,10,20,35))+
    
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm")) +
    
    # add state abbreviations with shadowtext
    # size determined by trial and error
    geom_shadowtext(aes(x=m_long, y=m_lat,
                        label=abbr),
                    color="white",
                    check_overlap = T,
                    show.legend = F,
                    size=3) + 
    
    labs(fill= "Pop. in Millions", title="Population by State",
         subtitle="Unit is 1 Million People - Log-transformed (AK - 0.73;  HI - 1.42)") + 
    
    # adjust size of all map text
    theme(plot.title = element_text(size = 25),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20)))

png("states_lnpop.png", width= 600, height=400)
st_lpop
dev.off()

```

***
  
  #### Week 12 In-class Exercises
  
  ***TurningPoint Session ID: bua455s22***
  
  #### TP Question 1 (L24)
  
  What exploratory plot is good for checking if the variable you want to plot is right skewed and might need to be log transformed?
  
  ***
  
  
  ### Filtering to map a Region
  
  * Techniques above can also be used for a region

* Below I use an education dataset and filter it to 10 Northeastern states

```{r import modify filter education data}

# import dataset and rename variables
edu <- read_csv("education by state.csv", skip=3, show_col_types = F, 
                col_names = c("state", "pop_over_25", 
                              "pop_hs", "pct_hs",
                              "pop_bachelor", "pct_bachelor", 
                              "pop_advanced","pct_advanced")) |> glimpse() 

# select variables, clean data, change variables to format needed for plot
# filter data to NE states
edu1 <- edu |>
  select(state, pop_bachelor, pct_bachelor) |>
  mutate(state = str_trim(state) |> tolower(),
         pop_bachelor1K = pop_bachelor/1000,
         pct_bachelor = gsub("%","", pct_bachelor, fixed = T) |> as.numeric()) |> 
  filter(state %in% c("maine", "massachusetts", "connecticut" , "rhode island",
                      "vermont", "new hampshire", "new york", "new jersey", "pennsylvania",
                      "delaware")) |> glimpse()

# exploratory map of bachelors data
# Base-R version - unformatted 
hist(edu1$pop_bachelor1K)

plot(edu1$pop_bachelor1K, edu1$pct_bachelor)

```

***
  
  #### Week 12 In-class Exercises
  
  ***TurningPoint Session ID: bua455s22***
  
  #### TP Question 2 (L24)
  
  The Base-R command to create a quick unformatted histogram of a quantitative variable is `hist()`. 

Even though our Education histogram is created with data for only 10 states, do these data appear skewed?
  
  ***
  
  ```{r join edu data with state map and state abbr data}

# state map from above
# state polygons (from R)
us_states <- map_data("state") |>
  select(long:region) |>
  rename("state" = "region")

# state abbreviations from above
state_abbr <- state_stats |>
  select(state, abbr) |>
  mutate(state = tolower(state))

# left join to maintain filter to NE states
edu1 <- left_join(edu1, state_abbr) 

# left join to maintain filter to NE states
edu_NE_map <- left_join(edu1, us_states)

# add in state midpoints (centroids)
state_coords <- read_csv("state_coords.csv", show_col_types = F,
                         col_names = c("state", "m_lat", "m_long")) |>
  mutate(state = gsub(", USA", "", state, fixed=T),
         state = gsub(", the USA", "", state, fixed=T),
         state = gsub(", the US", "", state, fixed=T),
         state = tolower(state))

# left join to maintain filter to NE states
edu_NE_map <- left_join(edu_NE_map, state_coords) 

```

***
  
  ### Regional Maps with Education Information
  
  * [Data Source - Wikipedia](https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_educational_attainment)

```{r NE edu maps pop and pct, fig.dim=c(12,5)}

# plot of logged pop with bachelors data
# transformation and breaks statement added
ne_edu_pop <- edu_NE_map |>
  ggplot(aes(x=long, y=lat, group=group, fill=pop_bachelor1K)) +
  geom_polygon(color="darkgrey") +
  
  theme_map() +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  
  scale_fill_continuous(type = "viridis", trans="log",
                        breaks = c(100, 500, 1000, 5000))+
  
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm")) +
  
  # add state abbreviations with shadowtext
  # size determined by trial and error
  geom_shadowtext(aes(x=m_long, y=m_lat,
                      label=abbr),
                  color="white",
                  check_overlap = T,
                  show.legend = F,
                  size=3) + 
  
  labs(fill= "Unit: 1000 People", 
       title="NE States: Pop. with a Bachelor's Degree",)


# percent data - no transformation
ne_edu_pct <- edu_NE_map |>
  ggplot(aes(x=long, y=lat, group=group, fill=pct_bachelor)) +
  geom_polygon(color="darkgrey") +
  
  theme_map() +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  
  scale_fill_continuous(type = "viridis", 
                        breaks = c(32, 34, 36, 38, 40, 42, 44)) +
  
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm")) +
  
  # add state abbreviations with shadowtext
  # size determined by trial and error
  geom_shadowtext(aes(x=m_long, y=m_lat,
                      label=abbr),
                  color="white",
                  check_overlap = T,
                  show.legend = F,
                  size=3) + 
  
  labs(fill= "Unit: %", title="NE States: Percent with a Bachelor's Degree")

grid.arrange(ne_edu_pop, ne_edu_pct, ncol=2)

```


***
  


+ Examples

- HW5 - Part 1 Solution

- This lecture

***
  
  ### RPubs
  
  * If you want to publish your dashboard or any HTML file you create in R, you can do so for free.  

* R has a public online repository called [RPubs](https://rpubs.com/).  

* I am not requiring students to use it for their projects, but it is useful if you want post something online and provide the link to it.

* As an in class exercise, I am asking you each to create an account and publish your HW 5 - Part 1 dashboard html file (see below).

- It is useful to do this with your dashboard so you can see how publishing it changes the appearance of your panels and text.

- Previous students have saved their final dashboard online and linked it to their resumes and LinkedIn profiles.


***
  
  ### Lecture 24 In-class Exercise 
  
  1. Open your HW 5 - Part 1.Rmd file and knit it to create your dashboard. 

+ Make sure this file has your name in the header.

+ It is okay if you haven't revised it yet for HW 5 - Part 2 
   
     - Rminder HW 5 - Part 2 is due tomorrow.
   
2. Click the Rpubs icon, create a free account, and publish your html file.

   + If RStudio asks to install additional packages to complete the publishing process, click `Yes`.

3. Submit the link to your published file on Blackboard.

4. A Link to your published file must be submitted by Friday 4/22/22 at midnight to count for class participation today.

***
  
### Next Week:

#### Additional Topics

* Ask me questions about your project (Others may benefit)

* I have 1-2 Examples

* I will also demo knitting R Markdown to different formats:

  + Word document with Table of Contents
  
  + Powerpoint presentation
  
  + NOTE: HTML is ideal and flexible (and can easily be published online), but other formats are some times appropriate.
  
#### Skillset Terminology

* Now that you are (almost) done with BUA 455 and more so, when you graduate you have a very useful set of skills

* I will spend a little time talking about how to explain those skills to other people

* Preview: It took me decades to figure out how to talk about what I do.

  + Increased interest in Data Science and Analytics has resulted in better terminology.
  
  
  

  
    
    
  
  
  
  