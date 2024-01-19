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
      id = "47022NED", catalog = "Politie",
      base_url = "https://dataderden.cbs.nl",
      dir = here("data"),
      # only national level frequencies are needed
      WijkenEnBuurten = has_substring("NL00")
    ) |>
    # create a date-class for time
    cbs_add_date_column() |>
    cbs_add_label_columns() |>
    select(crime_type_code  = SoortMisdrijf_label,
           date             = Perioden_Date,
           frequency        = GeregistreerdeMisdrijven_1) |>
    # replace NA with 0
    mutate(frequency = replace_na(frequency, 0),
           # remove trailing whitespace
           crime_type_code  = trimws(crime_type_code)) |>
    group_by(crime_type_code) |>
    mutate(total_incident_count = sum(frequency)) |>
    ungroup()
  write_csv(nl_allcrime_allyears, NL_CRIME_PATHNAME)
  # Read local copy if it exists
} else {
  nl_allcrime_allyears <- read_csv(NL_CRIME_PATHNAME,
                                   show_col_types = FALSE)
}

nl_allcrime_allyears <-
  nl_allcrime_allyears |>
  filter(crime_type_code %in% 
           c("1.6.2 Overige vermogensdelicten",
             "1.2.3 Diefstal van brom-, snor-, fietsen",
             "2.2.1 Vernieling cq. zaakbeschadiging",
             "3.9.1 Horizontale fraude",
             "1.2.1 Diefstal uit/vanaf motorvoertuigen",
             "1.1.1 Diefstal/inbraak woning",
             "1.4.5 Mishandeling",
             "2.5.2 Winkeldiefstal"
             )
  )

# Download or read NL-level disorder frequencies of all years 
if (file.exists(NL_DISORDER_PATHNAME) == FALSE | FORCE_REFRESH) {
  nl_alldisorder_allyears <-
    cbs_get_data(
      # this can be found with cbs_get_datasets(catalog="Politie")
      id = "47024NED", catalog = "Politie",
      base_url = "https://dataderden.cbs.nl",
      dir = here("data"),
      # only national level frequencies are needed
      WijkenEnBuurten = has_substring("NL00")
    ) |>
    # create a date-class for time
    cbs_add_date_column() |>
    cbs_add_label_columns() |>
    select(crime_type_code  = Overlast_label,
           date             = Perioden_Date,
           frequency        = GeregistreerdeOverlast_1) |>
    # replace NA with 0
    mutate(frequency = replace_na(frequency, 0),
           # remove trailing whitespace
           crime_type_code  = trimws(crime_type_code)) |>
    group_by(crime_type_code) |>
    mutate(total_incident_count = sum(frequency)) |>
    ungroup()
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
           population       = BevolkingAanHetBeginVanDePeriode_1) 
  write_csv(nl_population_allyears, NL_POPULATION_PATHNAME)
  # Read local copy if it exists
} else {
  nl_population_allyears <- read_csv(NL_POPULATION_PATHNAME,
                                     show_col_types = FALSE)
}

#  5. Merge crime and disorder data with population data ----------------------------------------------
# Crime data with population data
nl_allcrime_allyears_clean <- 
  # merge with crime type category labels
  nl_allcrime_allyears |>
  # merge with population data
  left_join(nl_population_allyears, by = "date")

# Disorder data with population data 
nl_alldisorder_allyears_clean <- 
  # merge with crime type category labels
  nl_alldisorder_allyears |>
  # merge with population data
  left_join(nl_population_allyears, by = "date")

#  6. Create time series -----------------------------------------------------------
#     Exclude "verkeer (weg)" and correct "Horizontale fraude"
observed_crime_series <-
  nl_allcrime_allyears_clean |> 
  # exclude totals
  filter(crime_type_code != "Totaal misdrijven") |>
  # Exclude traffic accidents
  filter(crime_type_code != "1.3.1 Ongevallen (weg)") |> 
  # Correct for different month lengths
  mutate(rel_frequency = frequency / days_in_month(date) * 
           (365.25/12) / (population / 100000)) |> 
  mutate(year_month = yearmonth(date, format="%m%Y"),
         month = factor(month(date))) |>
  # According to the data provider and as suggested by the data
  #  'Horizontale fraude' was not registered before 2016
  dplyr::filter(!(year_month %in% seq(yearmonth(as_date("2012-01-01")),
                                      yearmonth(as_date("2015-12-31")), 1) &
                    crime_type_code == "3.9.1 Horizontale fraude")) |>
  dplyr::select(-population)


observed_disorder_series <-
  nl_alldisorder_allyears_clean |> 
  # Exclude totals
  filter(crime_type_code != "Totaal registraties overlast") |>
  # Correct for different month lengths
  mutate(rel_frequency = frequency / days_in_month(date) * 
           (365.25/12) / (population / 100000)) |> 
  mutate(year_month = yearmonth(date, format="%m%Y"),
         month = factor(month(date))) |>
  dplyr::select(-population)

#  7. Visualize crime and disorder category frequencies ------------------------

crime_categories_bar_ggp <-
  observed_crime_series |>
  group_by(crime_type_code) |>
  summarize(aantal = round(mean(frequency),0)) |>
  # create percentages in addition to frequencies
  mutate(percentage = round(100 * aantal / sum(aantal),1),
         percentage = paste0(format(percentage, digits=2), "%"))  |>
  # Plot sorted by frequency/percentage
  mutate(crime_type_code = 
           fct_reorder(crime_type_code, aantal)) |> 
  ggplot() +
  geom_col(aes(x=aantal, y=crime_type_code),
           color="black",
           fill = "lightgrey") +
  geom_text(aes(x=aantal, y=crime_type_code, label=aantal), 
            nudge_x = 400, size=3) +
  geom_text(aes(x=aantal, y=crime_type_code, label=percentage), 
            nudge_x = -600, size=3) +
  xlab("Gemiddeld aantal misdrijven per maand") +
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
  observed_disorder_series |>
  group_by(crime_type_code) |>
  summarize(aantal = round(mean(frequency),0)) |>
  # create percentages in addition to frequencies
  mutate(percentage = round(100 * aantal / sum(aantal),1),
         percentage = paste0(format(percentage, digits=2), "%"))  |>
  # Plot sorted by frequency/percentage
  mutate(crime_type_code = 
           fct_reorder(crime_type_code, aantal)) |> 
  ggplot() +
  geom_col(aes(x=aantal, y=crime_type_code),
           color="black",
           fill = "lightgrey") +
  geom_text(aes(x=aantal, y=crime_type_code, label=aantal), 
            nudge_x = 1200, size=3) +
  geom_text(aes(x=aantal, y=crime_type_code, label=percentage), 
            nudge_x = -2000, size=3) +
  xlab("Gemiddeld aantal incidenten per maand") +
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
  nl_population_allyears |>
  mutate(year_month = yearmonth(date, format="%m%Y")) |>
  filter(date >= as.Date("2012-01-01"), date <= as.Date("2023-12-31")) |>
  ggplot() +
  geom_line(aes(x=date, y=population/1000000)) +
  scale_x_yearmonth(
    # name = "Period",
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y") +
  geom_vline(
    xintercept =
      as.numeric(seq(
        from = as.Date("2012-01-01"),  
        to = as.Date("2024-01-01"), by = "year"
      )),
    linetype = 1,
    color = "lightgrey",
    linewidth = .1,
  ) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=6, hjust=0),
        axis.text.y = element_text(size=6, hjust=0)) +
  xlab("Jaar en maand") +
  ylab("Omvang bevolking in miljoen")
population_growth_ggp

ggsave_svg(ggp=population_growth_ggp,
           output = here("output"),
           units = "mm",
           width = 120, 
           height = 60,
           scale=2)


#  9. Visualize development annual crime rates 2012-2024 ---------------------

ggplot_annual_crime_rates <- function(by = crime_type_code,
                                xlab = "Jaar",
                                ylab = "Misdrijven / jaar / 100000") {
  observed_crime_series |> 
    group_by({{by}}, year = year(date)) |> 
    summarize(rel_frequency = sum(rel_frequency), .groups="drop") |> 
    filter(year < 2024) |> 
    ggplot() + 
    geom_line(aes(x=year, y = rel_frequency)) + 
    geom_point(aes(x=year, y = rel_frequency)) + 
    scale_x_continuous(breaks = 2012:2024) +
#   scale_color_manual(name="",  values =c("darkgrey", "black")) +  
    facet_wrap(facets = vars({{by}}),  scales = "free_y", ncol=4) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size=4, hjust=0),
          axis.text.y = element_text(size=6, hjust=0)) +
    xlab(xlab) +
    ylab(ylab)
}

annual_crime_rates_nl_ggp <- 
  ggplot_annual_crime_rates(crime_type_code, 
                      xlab = "Jaar", 
                      ylab = "Misdrijven / jaar / 100000")
annual_crime_rates_nl_ggp

ggsave_svg(ggp = annual_crime_rates_nl_ggp,
           output = here("output"),
           units = "mm",
           width = 120, height = 60, scale=2)




ggplot_annual_disorder_rates <- function(by = crime_type_code,
                                      xlab = "Jaar",
                                      ylab = "Misdrijven / jaar / 100000") {
  observed_disorder_series |> 
    group_by({{by}}, year = year(date)) |> 
    summarize(rel_frequency = sum(rel_frequency), .groups="drop") |> 
    filter(year < 2024) |> 
    ggplot() + 
    geom_line(aes(x=year, y = rel_frequency)) + 
    geom_point(aes(x=year, y = rel_frequency)) + 
    scale_x_continuous(breaks = 2012:2024) +
    facet_wrap(facets = vars({{by}}),  scales = "free_y", ncol=4) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size=4, hjust=0),
          axis.text.y = element_text(size=6, hjust=0)) +
    xlab(xlab) +
    ylab(ylab)
}

annual_disorder_rates_nl_ggp <- 
  ggplot_annual_disorder_rates(crime_type_code, 
                            xlab = "Jaar", 
                            ylab = "Overlastincidenten / jaar / 100000")
annual_disorder_rates_nl_ggp

ggsave_svg(ggp = annual_disorder_rates_nl_ggp,
           output = here("output"),
           units = "mm",
           width = 120, height = 60, scale=2)





