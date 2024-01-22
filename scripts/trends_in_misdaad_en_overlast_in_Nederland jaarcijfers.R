


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

# # To verify what police data are available:
# cbs_catalog_politie <-
#   cbs_get_datasets(catalog = "Politie") 


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
           total_incident_count = GeregistreerdeMisdrijven_1) |>
    # replace NA with 0
    mutate(total_incident_count = replace_na(total_incident_count, 0),
           # remove trailing whitespace
           incident_type_code  = trimws(incident_type_code)) |>
    # remove 'total crime' category
    filter(incident_type_code != "Totaal misdrijven") 
 
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
           total_incident_count = GeregistreerdeOverlast_1) |>
    # replace NA with 0
    mutate(total_incident_count = replace_na(total_incident_count, 0),
           # remove trailing whitespace
           incident_type_code  = trimws(incident_type_code)) |>
    # remove 'total crime' category
    filter(incident_type_code != "Totaal registraties overlast") 
    
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
    summarize(population = mean(population))
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
  left_join(nl_population_allyears, by = "year")

# Disorder data with population data 
nl_alldisorder_allyears_merged <- 
  # merge with crime type category labels
  nl_alldisorder_allyears |>
  # merge with population data
  left_join(nl_population_allyears, by = "year")



#  6. Select crime and disorder categories -------------------------------------

# Read categories Nationale Politie
inp_bvh <- readxl::read_excel(here::here("scripts", "INP-BVH en INP-GMS.xlsx"), 
                              sheet = "INP2013 BVH")

selection_safety_without_accidents <-
  inp_bvh |>
  filter(`INP niv1`=="Veiligheid" & !str_detect(`INP niv3`,"Ongevallen")) |>
  select(`INP niv3 code`) |>
  count(`INP niv3 code`) |>
  select(inp_niv3_code = `INP niv3 code`)

# Selection of crime data
nl_allcrime_allyears_selection <-
  nl_allcrime_allyears_merged |>
  mutate(inp_niv3_code = substring(incident_type_code, 1,5)) |>
  inner_join(selection_safety_without_accidents, by = c("inp_niv3_code")) |>
  mutate(rel_frequency = total_incident_count / (population / 100000)) |>
  # According to the data provider and as suggested by the data
  #  'Horizontale fraude' was not registered before 2016
  filter(year > 2015,
                incident_type_code != "3.9.1 Horizontale fraude") |>
  select(-population)

# Selection of disorder data
nl_alldisorder_allyears_selection <-
  nl_alldisorder_allyears_merged |>
  mutate(rel_frequency = total_incident_count / (population / 100000)) |>
  select(-population)

# Selection of population data
nl_population_allyears_selection <-
  nl_population_allyears |>
  filter(year >= 2012)



#  7. Visualize crime and disorder category frequencies ------------------------

crime_categories_bar_ggp <-
  nl_allcrime_allyears_selection |>
  group_by(incident_type_code) |>
  summarize(aantal = round(mean(total_incident_count),0)) |>
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
  summarize(aantal = round(mean(total_incident_count),0)) |>
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
  ggplot() +
  geom_line(aes(x=year, y=population/1000000)) +
  scale_x_continuous(breaks = 2012:2024) +  
  geom_vline(
    xintercept =
      as.numeric(seq(
        from = 2012,  
        to = 2024
      )),
    linetype = 1,
    color = "lightgrey",
    linewidth = .1,
  ) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=6, hjust=0),
        axis.text.y = element_text(size=6, hjust=0)) +
  xlab("Jaar") +
  ylab("Omvang bevolking in miljoen")
population_growth_ggp

ggsave_svg(ggp=population_growth_ggp,
           output = here("output"),
           units = "mm",
           width = 120, 
           height = 60,
           scale=2)


#  9. Visualize development annual crime rates 2012-2024 ---------------------

ggplot_annual_crime_rates <- function(by = incident_type_code,
                                      xlab = "Jaar",
                                      ylab = "Misdrijven / jaar / 100000") {
  nl_allcrime_allyears_selection |> 
    group_by({{by}}, year ) |> 
    ggplot() + 
    geom_line(aes(x=year, y = rel_frequency)) + 
    geom_point(aes(x=year, y = rel_frequency)) + 
    scale_x_continuous(breaks = 2012:2024) +
    #   scale_color_manual(name="",  values =c("darkgrey", "black")) +  
    facet_wrap(facets = vars({{by}}),  scales = "free_y", ncol=6) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size=4, hjust=0),
          axis.text.y = element_text(size=6, hjust=0)) +
    xlab(xlab) +
    ylab(ylab)
}

annual_crime_rates_nl_ggp <- 
  ggplot_annual_crime_rates(incident_type_code, 
                            xlab = "Jaar", 
                            ylab = "Misdrijven / jaar / 100000")
annual_crime_rates_nl_ggp

ggsave_svg(ggp = annual_crime_rates_nl_ggp,
           output = here("output"),
           units = "mm",
           width = 120, height = 60, scale=2)




ggplot_annual_disorder_rates <- function(by = incident_type_code,
                                         xlab = "Jaar",
                                         ylab = "Misdrijven / jaar / 100000") {
  nl_alldisorder_allyears_selection |> 
    group_by({{by}}, year) |> 
    summarize(rel_frequency = sum(rel_frequency), .groups="drop") |> 
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
  ggplot_annual_disorder_rates(incident_type_code, 
                               xlab = "Jaar", 
                               ylab = "Overlastincidenten / jaar / 100000")
annual_disorder_rates_nl_ggp

ggsave_svg(ggp = annual_disorder_rates_nl_ggp,
           output = here("output"),
           units = "mm",
           width = 120, height = 60, scale=2)














