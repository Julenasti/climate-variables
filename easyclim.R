library(easyclimate)
extrafont::loadfonts(device = "win")
library(tidyverse)
library(here)
library(scales)
library(viridis)
library(patchwork)
library(ggrepel)

# birmingham --------------------------------------------------------------
coords <- data.frame(
  lon = -1.898575,
  lat = 52.489471
)
# UPDATE TOKEN!!!!
ggplot() +
  borders(regions = "UK") +
  geom_point(data = coords, aes(x = lon, y = lat)) +
  coord_quickmap() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

# info about the package: https://verughub.github.io/easyclimate/
# download daily climatic data
df_prcp <- get_daily_climate(
  coords = coords,
  period = 1950:2017,
  climatic_var = "Prcp"
)

df_tmin <- get_daily_climate(
  coords = coords,
  period = 1950:2017,
  climatic_var = "Tmin"
)

df_tmax <- get_daily_climate(
  coords = coords,
  period = 1950:2017,
  climatic_var = "Tmax"
)

real_value <- function(x){x/100}

daily <- df_prcp %>%
  full_join(df_tmin) %>%
  full_join(df_tmax)

# daily average climatic values
# precipitation units are ºC*100 and mm*100 to avoid floating values
daily <- daily %>%
  mutate(
    across(c(Prcp, Tmin, Tmax), real_value)
    ) %>%
  mutate(
    Tmean = (Tmin + Tmax) / 2, # daily mean temperature
    date = as.Date(date),
    month = format(date, format = "%m"),
    year = format(date, format = "%Y"),
    yday = lubridate::yday(date)
  ) %>%
  mutate_if(
    is.character, as.numeric
    )

# yearly average climatic values
yearly <- daily %>%
  group_by(year) %>%
  summarise(
    tmin.year = mean(Tmin),
    tmean.year = mean(Tmean),
    tmax.year = mean(Tmax),
    prcp.year = sum(Prcp),
    )

# visualise data
# theming
theme_set(theme_minimal(base_size = 18, base_family = "Candara"))
theme_update(
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "top",
  legend.title = element_text(size = 14, color = "grey20"),
  legend.text = element_text(size = 12, color = "grey50"),
  plot.title = element_text(size = 22, face = "bold", margin = margin(b = 15)),
  plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
  plot.caption = element_text(size = 14, color = "grey50", margin = margin(t = 25)),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  axis.text = element_text(size = 14),
  axis.line.x = element_line(color = "grey20"),
  axis.ticks.x = element_line(color = "grey20"),
  plot.margin = margin(20, 20, 10, 20)
)

gg_year <- function(y, units, title){
  ggplot(yearly, aes(x = year, y = .data[[y]], color = year)) +
    geom_smooth(method = "lm", color = "firebrick", size = 1.3) +
    geom_point(alpha = .5) +
    scale_y_continuous(labels = function(x){paste0(x, units)}) +
    scale_color_viridis_c(option = "turbo", direction = -1, name = NULL,
                          breaks = seq(1950, 2020, 10)) +
    guides(color = guide_colorsteps(barwidth = unit(30, "lines"), barheight = unit(.4, "lines"))) +
    labs(y = NULL, x = NULL,
         title = title)
}

gg_year_tmean <- gg_year(
  y = "tmean.year",
  units = "°C",
  title = "Annual mean temperature"
)

gg_year_prcp <- gg_year(
  y = "prcp.year",
  units = " mm",
  title = "Annual Precipitation"
)

months <- tibble(yday = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                 label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

gg_day <- function(y, slicing, units, title, subtitle){
  ggplot(daily, aes(yday, .data[[y]], color = year)) +
    geom_point(alpha = .5, size = .9) +
    geom_point(data = slicing(daily, n = 5, .data[[y]]),
               aes(color = year), size = 4) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(breaks = months$yday,
                       labels = months$label,
                       expand = c(.001, .001)) +
    scale_y_continuous(labels = function(x){paste0(x, units)}) +
    scale_color_viridis_c(option = "turbo", direction = -1, name = NULL,
                          breaks = seq(1950, 2020, 10)) +
    geom_label_repel(data = slicing(daily, n = 5, .data[[y]]),
                     aes(label = year),
                     color = "black",
                     box.padding = unit(0.35, "lines"),
                     ylim = c(NA, Inf),
                     fontface = "bold",
                     size = 4) +
    guides(color = guide_colorsteps(barwidth = unit(30, "lines"), barheight = unit(.4, "lines"))) +
    labs(y = NULL, x = NULL,
         title = title,
         subtitle = subtitle)
    }

gg_day_tmax <- gg_day(
  y = "Tmax",
  slicing = slice_max,
  units = "°C",
  title = "Daily maximum temperature",
  subtitle = "The points labelled with years represent the 5 days with the highest maximum temperature"
)

gg_day_tmin <- gg_day(
  y = "Tmin",
  slicing = slice_min,
  units = "°C",
  title = "Daily minimum temperature",
  subtitle = "The points labelled with years represent the 5 days with the lowest minimum temperature"
)

final_plot <- (gg_year_tmean + gg_year_prcp) /
  (gg_day_tmax + gg_day_tmin) +
  plot_annotation(
    title = "Climate variables in Birmingham, UK (1950-2017)",
    caption = "   Data source:
      -Moreno A, Hasenauer H (2016). “Spatial downscaling of European climate data.” International Journal of Climatology, 1444–1458.
      -Rammer W, Pucher C, Neumann M (2018). Description, Evaluation and Validation of Downscaled Daily Climate Data Version 2.
      -Cruz-Alonso V, Rodríguez-Sánchez F, Pucher C, Ratcliffe S, Astigarraga J, Neumann M, Ruiz-Benito P (2021). easyclimate: Easy access to high-resolution daily climate data for Europe.
    Graphic: Julen Astigarraga",
    theme = theme(
      plot.title = element_text(size = 24),
      plot.caption = element_text(hjust = 0)
    )
  )

ggsave(
  "birmingham.png",
  width = 18, height = 14
)

# onati -----------------------------------------------------------------
coords <- data.frame(
  lon = -2.41135,
  lat = 43.0329
)

ggplot() +
  borders(regions = c("Spain", "Portugal", "France")) +
  geom_point(data = coords, aes(x = lon, y = lat)) +
  coord_fixed(xlim = c(-10, 2), ylim = c(36, 44), ratio = 1.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw()

# download daily climatic data
df_prcp <- get_daily_climate(
  coords = coords,
  period = 1950:2017,
  climatic_var = "Prcp"
)

df_tmin <- get_daily_climate(
  coords = coords,
  period = 1950:2017,
  climatic_var = "Tmin"
)

df_tmax <- get_daily_climate(
  coords = coords,
  period = 1950:2017,
  climatic_var = "Tmax"
)

real_value <- function(x){x/100}

daily <- df_prcp %>%
  full_join(df_tmin) %>%
  full_join(df_tmax)

daily <- daily %>%
  mutate(
    across(c(Prcp, Tmin, Tmax), real_value)
  ) %>%
  mutate(
    Tmean = (Tmin + Tmax) / 2,
    date = as.Date(date),
    month = format(date, format = "%m"),
    year = format(date, format = "%Y"),
    yday = lubridate::yday(date)
  ) %>%
  mutate_if(
    is.character, as.numeric
  )

yearly <- daily %>%
  group_by(year) %>%
  summarise(
    tmin.year = mean(Tmin),
    tmean.year = mean(Tmean),
    tmax.year = mean(Tmax),
    prcp.year = sum(Prcp),
  )

gg_year_tmean <- gg_year(
  y = "tmean.year",
  units = "°C",
  title = "Bataz besteko temperatura"
)

gg_year_prcp <- gg_year(
  y = "prcp.year",
  units = " mm",
  title = "Prezipitazioak"
)

months <- tibble(yday = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                 label = c("Urt", "Ots", "Mar", "Api", "Mai", "Eka", "Uzt", "Abu", "Ira", "Urr", "Aza", "Abe"))

gg_day_tmax <- gg_day(
  y = "Tmax",
  slicing = slice_max,
  units = "°C",
  title = "Eguneko tenperatura maximoak",
  subtitle = "Urteak ageri diren puntuek tenperatura maximo altueneko 5 egunak irudikatzen dituzte"
)

gg_day_tmin <- gg_day(
  y = "Tmin",
  slicing = slice_min,
  units = "°C",
  title = "Eguneko tenperatura minimoak",
  subtitle = "Urteak ageri diren puntuek puntuek tenperatura minimo baxueneko 5 egunak irudikatzen dituzte"
)

final_plot <- (gg_year_tmean + gg_year_prcp) /
  (gg_day_tmax + gg_day_tmin) +
  plot_annotation(
    title = "Aldagai klimatikoak Oñatin, Gipuzkoa (1950-2017)",
    caption = "   Datu-iturria:
      -Moreno A, Hasenauer H (2016). “Spatial downscaling of European climate data.” International Journal of Climatology, 1444–1458.
      -Rammer W, Pucher C, Neumann M (2018). Description, Evaluation and Validation of Downscaled Daily Climate Data Version 2.
      -Cruz-Alonso V, Rodríguez-Sánchez F, Pucher C, Ratcliffe S, Astigarraga J, Neumann M, Ruiz-Benito P (2021). easyclimate: Easy access to high-resolution daily climate data for Europe.
    Grafikoa: Julen Astigarraga",
    theme = theme(
      plot.title = element_text(size = 24),
      plot.caption = element_text(hjust = 0)
    )
  )

ggsave(
  "onati_eus.png",
  width = 18, height = 14
)
