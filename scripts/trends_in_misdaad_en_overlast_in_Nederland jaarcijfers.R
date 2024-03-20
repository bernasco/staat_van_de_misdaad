
# trends_in_misdaad_en_overlast_in_Nederland.R

#  1. install and load package(s) -----------------------------------------------
if (!require("tidyverse")) 
  install.packages("tidyverse")
if (!require("cbsodataR")) 
  install.packages("cbsodataR")
if (!require("here")) 
  install.packages("here")
if (!require("lubridate")) 
  install.packages("lubridate")
if (!require("tsibble")) 
  install.packages("tsibble")
if (!require("viridis")) 
  install.packages("viridis")

#  2. Set up folder structure --------------------------------------------------

# Downloaded data
if (dir.exists(here::here("data")) == FALSE) {
  dir.create(here::here("data"))
}
# Output
if (dir.exists(here::here("output")) == FALSE) {
  dir.create(here::here("output"))
}

#  3. Define functions and constants -----------------------------------------------

# Force fresh downloads from CBS 
FORCE_REFRESH <- FALSE

# How many crime and disorder types to report
N_CRIMETYPES <- 8
N_DISORDERTYPES <- 8

# Path name downloaded crime data
NL_CRIME_PATHNAME      <- here("data", "NL_Crime.csv")
# Path name downloaded disorder data
NL_DISORDER_PATHNAME      <- here("data", "NL_Disorder.csv")
# Path name downloaded population counts CBS
NL_POPULATION_PATHNAME <- here("data", "NL_Population.csv")

# Function definition: store ggplot figure as PNG file
ggsave_png <- function(ggp, output, ...) {
  ggsave(filename = paste(substitute(ggp),".png", sep=""), 
         device = "png", plot = ggp, path = output, 
         limitsize = TRUE, ...)
}

# Function definition: store ggplot figure as SVG file
ggsave_svg <- function(ggp, output, ...) {
  ggsave(filename = paste(substitute(ggp),".svg", sep=""), 
         device = "svg", plot = ggp, path = output, 
         limitsize = TRUE, ...)
}

# Function definition: store ggplot figures in a list as SVG file
ggsave_svg_list <- function(ggp_list, output, ...) {
  # first verify that the first argument is a list
  if(!is.list(ggp_list)) { 
    stop("not a list")
  }  
  listname <- substitute(ggp_list)
  # first get the name of the list object
  if (is.null(names(ggp_list))) {
    names(ggp_list) <- paste0(seq_along(ggp_list))
  }
  #for each named element of the list
  map(names(ggp_list),
      function(.x) {
        ggsave(
          path = here("output"),
          filename = paste0(listname, "_", .x, ".svg"),
          plot = ggp_list[[.x]]
        )
      }
  )
}



#  4. Read crime, disorder and population data ---------------------------------

# Download or read NL-level crime frequencies of all years 
if (file.exists(NL_CRIME_PATHNAME) == FALSE | FORCE_REFRESH) {
  nl_allcrime_allyears <-
    cbs_get_data(
      # this can be found with cbs_get_datasets(catalog="Politie")
      id = "47025NED", catalog = "Politie",
      base_url = "https://dataderden.cbs.nl",
      dir = here("data"),
      # only national level frequencies are needed
      RegioS = has_substring("NL01"),
      Perioden = has_substring("JJ00")
    ) |>
    # create a date-class for time
    cbs_add_date_column() |>
    cbs_add_label_columns() |>
    mutate(year = year(Perioden_Date)) |>
    select(incident_type_code  = SoortMisdrijf_label,
           year,
           incident_count = GeregistreerdeMisdrijven_1) |>
    # replace NA with 0
    mutate(incident_count = replace_na(incident_count, 0),
           # remove trailing whitespace
           incident_type_code  = trimws(incident_type_code)) 
  # remove 'total crime' category
  # filter(incident_type_code != "Totaal misdrijven") 
  
  write_csv(nl_allcrime_allyears, NL_CRIME_PATHNAME)
  # Read local copy if it exists
} else {
  nl_allcrime_allyears <- read_csv(NL_CRIME_PATHNAME,
                                   show_col_types = FALSE)
}


# Download or read NL-level disorder frequencies of all years 
if (file.exists(NL_DISORDER_PATHNAME) == FALSE | FORCE_REFRESH) {
  nl_alldisorder_allyears <-
    cbs_get_data(
      # this can be found with cbs_get_datasets(catalog="Politie")
      id = "47021NED", catalog = "Politie",
      base_url = "https://dataderden.cbs.nl",
      dir = here("data"),
      # only national level frequencies are needed
      RegioS = has_substring("NL01"),
      Perioden = has_substring("JJ00")
    ) |> 
    # create a date-class for time
    cbs_add_date_column() |>
    cbs_add_label_columns() |>
    mutate(year = year(Perioden_Date)) |>
    select(incident_type_code   = Overlast_label,
           year,
           incident_count = GeregistreerdeOverlast_1) |>
    # replace NA with 0
    mutate(incident_count = replace_na(incident_count, 0),
           # remove trailing whitespace
           incident_type_code  = trimws(incident_type_code))
  # remove 'total crime' category
  # filter(incident_type_code != "Totaal registraties overlast") 
  
  write_csv(nl_alldisorder_allyears, NL_DISORDER_PATHNAME)
  # Read local copy if it exists
} else {
  nl_alldisorder_allyears <- read_csv(NL_DISORDER_PATHNAME,
                                      show_col_types = FALSE)
}

# Download or read NL-level population frequencies of all years 
if (file.exists(NL_POPULATION_PATHNAME) == FALSE | FORCE_REFRESH) {
  nl_population_allyears <-
    cbs_get_data(
      # this can be found with cbs_get_datasets(catalog="CBS")
      id = "83474NED", catalog = "CBS",
      dir = here("data")
    ) |>
    # create a date-class for time
    cbs_add_date_column() |>
    # select only monthly (not yearly) records
    filter(Perioden_freq == "M") |>
    select(date             = Perioden_Date,
           population       = BevolkingAanHetBeginVanDePeriode_1) |>
    mutate(year = year(date)) |>
    group_by(year) |>
    summarize(population = mean(population, na.rm = TRUE))
  write_csv(nl_population_allyears, NL_POPULATION_PATHNAME)
  # Read local copy if it exists
} else {
  nl_population_allyears <- read_csv(NL_POPULATION_PATHNAME,
                                     show_col_types = FALSE)
}

#  5. Merge crime and disorder data with population data ----------------------------------------------
# Crime data with population data
nl_allcrime_allyears_merged <- 
  # merge with crime type category labels
  nl_allcrime_allyears |>
  # merge with population data
  left_join(nl_population_allyears, by = "year") |>
  mutate(COVID = as.numeric(year %in% c(2020, 2021)))

# Disorder data with population data 
nl_alldisorder_allyears_merged <- 
  # merge with crime type category labels
  nl_alldisorder_allyears |>
  # merge with population data
  left_join(nl_population_allyears, by = "year") |>
  mutate(COVID = as.numeric(year %in% c(2020, 2021)))


#  6. Select crime and disorder categories -------------------------------------

# First calculate total crime and disorder incidents per year

nl_allcrime_allyears_total <-
  nl_allcrime_allyears_merged |>
  # exclude traffic accidents and totals
  filter(incident_type_code   != "1.3.1 Ongevallen (weg)", 
         incident_type_code != "Totaal misdrijven") |>
  # aggregate by year
  group_by(year) |>
  summarize(aantal_misdrijven = sum(incident_count))

# Total disorder data per year
nl_alldisorder_allyears_total <-
  nl_alldisorder_allyears_merged |>
  # exclude totals
  filter(incident_type_code != "Totaal registraties overlast") |>
  # aggregate by year
  group_by(year) |>
  summarize(aantal_overlastregistraties = sum(incident_count))

# Next select the most common crime and disorder types
nl_allcrime_allyears_selection <-
  nl_allcrime_allyears_merged |>
  # Exclude totals
  filter(incident_type_code != "Totaal misdrijven") |> 
  # exclude traffic accidents
  filter(incident_type_code != "1.3.1 Ongevallen (weg)") |> 
  # count the total per crime type over all years
  group_by(incident_type_code) |>
  summarize(total_allyears = sum(incident_count)) |>
  # sort crime types in descending order
  arrange(-total_allyears) |>
  # rank order and select most common crime types
  mutate(rank_order = row_number()) |>
  filter(rank_order <= N_CRIMETYPES) |>
  # select just the type of crime and its rank order
  select(incident_type_code, rank_order) |>
  # merge back into data
  left_join(nl_allcrime_allyears_merged, by = "incident_type_code") |>
  
  # calculate crime rate
  mutate(rel_frequency = incident_count / (population / 100000)) |>
  select(-population)

# Selection of disorder data
nl_alldisorder_allyears_selection <-
  nl_alldisorder_allyears_merged |>
  # Exclude totals
  filter(incident_type_code != "Totaal registraties overlast") |> 
  # count the total number of disorder incidents per disorder type over all years
  group_by(incident_type_code) |>
  summarize(total_allyears = sum(incident_count)) |>
  # sort disorder types in descending order
  arrange(-total_allyears) |>
  # rank order and select most common disorder types
  mutate(rank_order = row_number()) |>
  filter(rank_order <= N_DISORDERTYPES) |>
  # select just the type of disorder and its rank order
  select(incident_type_code, rank_order) |>
  left_join(nl_alldisorder_allyears_merged, by = "incident_type_code") |>
  # calculate disorder rate
  mutate(rel_frequency = incident_count / (population / 100000)) |>
  select(-population)  



# Selection of population data
nl_population_allyears_selection <-
  nl_population_allyears |>
  filter(year >= 2012)





#  7. Visualize crime and disorder category frequencies ------------------------

crime_categories_bar_ggp <-
  nl_allcrime_allyears_selection |>
  group_by(incident_type_code) |>
  summarize(aantal = round(mean(incident_count),0)) |>
  # create percentages in addition to frequencies
  mutate(percentage = round(100 * aantal / sum(aantal),1),
         percentage = paste0(format(percentage, digits=2), "%"))  |>
  # Plot sorted by frequency/percentage
  mutate(incident_type_code = 
           fct_reorder(incident_type_code, aantal)) |> 
  ggplot() +
  geom_col(aes(x=aantal, y=incident_type_code),
           color="black",
           fill = "lightgrey") +
  geom_text(aes(x=aantal, y=incident_type_code, label=aantal), 
            nudge_x = 8000, size=3) +
  geom_text(aes(x=aantal, y=incident_type_code, label=percentage), 
            nudge_x = -6000, size=3) +
  xlab("Gemiddeld aantal misdrijven per jaar") +
  ylab("Misdaadcategorie")
crime_categories_bar_ggp

ggsave_svg(ggp=crime_categories_bar_ggp,
           output = here("output"),
           units = "mm",
           width = 120,
           height = 60,
           scale=2
)

disorder_categories_bar_ggp <-
  nl_alldisorder_allyears_selection  |>
  group_by(incident_type_code) |>
  summarize(aantal = round(mean(incident_count),0)) |>
  # create percentages in addition to frequencies
  mutate(percentage = round(100 * aantal / sum(aantal),1),
         percentage = paste0(format(percentage, digits=2), "%"))  |>
  # Plot sorted by frequency/percentage
  mutate(incident_type_code = 
           fct_reorder(incident_type_code, aantal)) |> 
  ggplot() +
  geom_col(aes(x=aantal, y=incident_type_code),
           color="black",
           fill = "lightgrey") +
  geom_text(aes(x=aantal, y=incident_type_code, label=aantal), 
            nudge_x = 10000, size=3) +
  geom_text(aes(x=aantal, y=incident_type_code, label=percentage), 
            nudge_x = -6000, size=3) +
  xlab("Gemiddeld aantal incidenten per jaar") +
  ylab("Overlastcategorie")
disorder_categories_bar_ggp

ggsave_svg(ggp=disorder_categories_bar_ggp,
           output = here("output"),
           units = "mm",
           width = 120,
           height = 60,
           scale=2
)


#  8. Visualize population growth 2012-2024 ------------------------------------
population_growth_ggp <-
  nl_population_allyears_selection |>
  mutate(population_in_million = population / 1000000) |>
  ggplot() +
  geom_line(aes(x=year, y=population_in_million)) +
  scale_x_continuous(breaks = 2012:2024) +
  scale_y_continuous(breaks = 15:18, limits = c(15, 18)) +
  theme_minimal() +
  xlab("") +
  ylab("Omvang bevolking in miljoen")
population_growth_ggp

ggsave_svg(ggp=population_growth_ggp,
           output = here("output"),
           units = "mm",
           width = 120, 
           height = 60,
           scale=2)

# Y scale including 0

population_growth_inc_zero_ggp <-
  nl_population_allyears_selection |>
  mutate(population_in_million = population / 1000000) |>
  ggplot() +
  geom_line(aes(x=year, y=population_in_million)) +
  scale_x_continuous(breaks = 2012:2024) +
  scale_y_continuous(breaks = 0:18, limits = c(0, 18)) +
  theme_minimal() +
  xlab("") +
  ylab("Omvang bevolking in miljoen")
population_growth_inc_zero_ggp

ggsave_svg(ggp=population_growth_inc_zero_ggp,
           output = here("output"),
           units = "mm",
           width = 120, 
           height = 60,
           scale=2)

#  9. Visualize development annual crime/disorder rates 2012-2024 ---------------------


# single y scale
annual_crime_rates_single_y_changes <- 
  nl_allcrime_allyears_selection |> 
  mutate(incident_type_code = str_sub(incident_type_code, 7,-1)) |>
  rename(`Soort misdrijf` = incident_type_code) |>
  group_by(`Soort misdrijf`) |>
  arrange(`Soort misdrijf`, year) |>
  mutate(prop_since2012 = 100* rel_frequency / first(rel_frequency),
         prop_sinceprevious = 100 * rel_frequency / lag(rel_frequency),
         percentage_change_since2012 = prop_since2012 - 100,
         percentage_change_sinceprevious = prop_sinceprevious - 100)

annual_crime_rates_single_y_changes |> 
  write_csv(here("output", "annual_crime_rates_single_y_changes.csv"))

annual_crime_rates_single_y_ggp <-   
  annual_crime_rates_single_y_changes |>
  ggplot() + 
  geom_col(data = nl_allcrime_allyears_selection |>
             # height of the COVID bar extends 5% above the maximum rate
             mutate(COVID = 1.05 * max(rel_frequency) * COVID) |> 
             group_by(year, COVID) |>
             summarize(.groups = "drop"),
           mapping = aes(x = year, y = COVID), color = "lightgrey", alpha = .2, width = 1) +
  geom_line(aes(x=year, y = rel_frequency, color = `Soort misdrijf`)) + 
  geom_point(aes(x=year, y = rel_frequency, color = `Soort misdrijf`)) + 
  scale_x_continuous(breaks = 2012:2024) +
  scale_y_continuous(breaks = seq(0,1200, 100), limits = c(0, 1200)) +
  # use discrete Viridis color palette (but note the yellow is )
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_minimal() +
  theme(legend.position = "right") +
  # no label on X axis as it is obvious that it is years
  xlab("") +
  ylab("Misdrijven / jaar / 100000 inwoners")

annual_crime_rates_single_y_ggp

ggsave_svg(ggp = annual_crime_rates_single_y_ggp,
           output = here("output"),
           units = "mm",
           width = 120, height = 60, scale=2)





# Plot disorder rates by type of disorder on a single y scale
annual_disorder_rates_single_y_changes <- 
  # annual disorder rates
  nl_alldisorder_allyears_selection |> 
  # use proper label for the plot (simpler than changing the legend)
  rename(`Soort overlast` = incident_type_code) |> 
  group_by(`Soort overlast`) |>
  arrange(`Soort overlast`, year) |>
  mutate(prop_since2012 = 100* rel_frequency / first(rel_frequency),
         prop_sinceprevious = 100 * rel_frequency / lag(rel_frequency),
         percentage_change_since2012 = prop_since2012 - 100,
         percentage_change_sinceprevious = prop_sinceprevious - 100)

annual_disorder_rates_single_y_changes |> 
  write_csv(here("output", "annual_disorder_rates_single_y_changes.csv"))

  
annual_disorder_rates_single_y_ggp <-   
  annual_disorder_rates_single_y_changes |>
  # start the plot
  ggplot() + 
  # plot the COVID years as a bar
  geom_col(data = nl_alldisorder_allyears_selection |>
             # height of the COVID bar extends 5% above the maximum rate
             mutate(COVID = 1.05 * max(rel_frequency) * COVID) |> 
             # We need only a single bar per year
             group_by(year, COVID) |>
             summarize(.groups = "drop"),
           # light grey and semi-transparent to create a background effect
           mapping = aes(x = year, y = COVID), color = "lightgrey", alpha = .2, width = 1) +
  # trendline to guide the eye
  geom_line(aes(x=year, y = rel_frequency, color = `Soort overlast`)) + 
  # data points
  geom_point(aes(x=year, y = rel_frequency, color = `Soort overlast`)) + 
  # pretty axis labels
  scale_x_continuous(breaks = 2012:2024) +
  scale_y_continuous(breaks = seq(0,1200, 100), limits = c(0, 1200)) +
  # viridis 
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_minimal() +
  theme(legend.position = "right") +
  # No label on X axis as it is obvious that it is years
  xlab("") +
  ylab("Overlastincidenten / jaar / 100000 inwoners")

annual_disorder_rates_single_y_ggp

ggsave_svg(ggp = annual_disorder_rates_single_y_ggp,
           output = here("output"),
           units = "mm",
           width = 120, height = 60, scale=2)







# 10. Aggregate and visualise all crime all disorder types together -----------------

# create reported crime rate series 
total_crime <-
  nl_allcrime_allyears |>
  # exclude traffic accidents and totals
  filter(incident_type_code   != "1.3.1 Ongevallen (weg)", 
         incident_type_code != "Totaal misdrijven") |>
  # Summarize all crime types across years
  group_by(year) |>
  summarize(incident_count = sum(incident_count), .groups = "drop") |>
  mutate(`Soort incident` = "Criminaliteit")

# create total reported disorder rate series 
total_disorder <-
  nl_alldisorder_allyears |>
  # exclude totals
  filter(incident_type_code != "Totaal registraties overlast") |>
  # summarize all disorder types across years
  group_by(year) |>
  summarize(incident_count = sum(incident_count), .groups = "drop") |>
  mutate(`Soort incident` = "Overlast")  

annual_crime_disorder_rates <- 
  # combine crime and disorder in a single file (to simplify creating a legend)
  bind_rows(total_crime, total_disorder) |> 
  # add population
  left_join(nl_population_allyears, by = "year") |> 
  # calculate disorder rate
  mutate(rel_frequency = incident_count / (population / 100000)) |>
  select(-population) |>
  mutate(COVID = as.numeric(year %in% c(2020, 2021))) 

annual_crime_disorder_changes <-
  annual_crime_disorder_rates |>
  group_by(`Soort incident`) |>
  arrange(`Soort incident`, year) |>
  mutate(prop_since2012 = 100* rel_frequency / first(rel_frequency),
         prop_sinceprevious = 100 * rel_frequency / lag(rel_frequency),
         percentage_change_since2012 = prop_since2012 - 100,
         percentage_change_sinceprevious = prop_sinceprevious - 100)
annual_crime_disorder_changes |> 
  write_csv(here("output", "annual_crime_disorder_changes.csv"))

annual_crime_disorder_rates_ggp <- 
  annual_crime_disorder_rates|>
  # make the height of the 'COVID' bar equal to the maximum on the Y axis
  mutate(COVID = 1.00 * max(rel_frequency) * COVID) |> 
  # start the plot
  ggplot() + 
  # first draw the COVID bar in years 2020-2021
  geom_col(mapping = aes(x = year, y = COVID), color = "lightgrey", alpha = .2, width = 1) + 
  # then draw the crime and disorder lines with points on them
  geom_line(aes(x=year, y = rel_frequency, color = `Soort incident`)) + 
  geom_point(aes(x=year, y = rel_frequency, color = `Soort incident`)) + 
  # use a label for every year  
  scale_x_continuous(breaks = 2012:2024) +
  #   
  scale_y_continuous(breaks = seq(0,6000, 500), limits = c(0, 6500)) +
  # use discrete Viridis color palette (but note the yellow is )
  # scale_color_viridis(discrete = TRUE, option = "A") +
  theme_minimal() +
  theme(legend.position = "right") +
  # no label on X axis as it is obvious that it is years
  xlab("") +
  ylab("Incidenten / jaar / 100000 inwoners")

annual_crime_disorder_rates_ggp 

ggsave_svg(ggp = annual_crime_disorder_rates_ggp,
           output = here("output"),
           units = "mm",
           width = 120, height = 60, scale=2)



