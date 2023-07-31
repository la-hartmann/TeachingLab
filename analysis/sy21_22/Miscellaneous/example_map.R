library(tmap)
library(tidycensus)

dat16 <- get_acs("county",
  table = "B27001", year = 2018,
  output = "tidy", state = NULL, geometry = TRUE, shift_geo = TRUE
) %>%
  rename(`2016` = estimate) %>%
  select(-moe)

dat12 <- get_acs("county",
  table = "B27001", year = 2012,
  output = "tidy", state = NULL, geometry = FALSE
) %>%
  rename(`2012` = estimate) %>%
  select(-NAME, -moe)

dat <- left_join(dat16, dat12, by = c("GEOID", "variable"))
st_geometry(dat) <- NULL # This drops the geometry and leaves a table

dat <- mutate(dat,
  cat = case_when(
    variable %in% paste0(
      "B27001_0",
      c("09", "12", "37", "40")
    ) ~ "pop1834",
    variable %in% paste0(
      "B27001_0",
      c("11", "14", "39", "42")
    ) ~ "pop1834ni"
  )
) %>%
  filter(!is.na(cat))

dat <- tidyr::gather(dat, year, estimate, c(`2012`, `2016`))

# Group the data by our new categories and sum
dat_final <- group_by(dat, GEOID, NAME, year, cat) %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup() %>%
  tidyr::spread(cat, estimate) %>%
  mutate(est = (pop1834ni / pop1834) * 100) %>%
  select(-c(pop1834, pop1834ni)) %>%
  tidyr::spread(year, est) %>%
  mutate(diff = `2016` - `2012`)

# dat16 is our original geographic object and dat is the tabular data
shp <- dat16 %>%
  filter(variable == "B27001_001") %>% # much faster than using distinct()
  select(GEOID, NAME) %>%
  left_join(dat_final, by = c("GEOID", "NAME")) %>%
  arrange(GEOID) %>%
  rename(
    uninsured_2012 = `2012`,
    uninsured_2016 = `2016`,
    uninsured_diff = diff
  )

shp <- filter(shp, GEOID != "02016")

# Remove the Aleutians West from shp for display purposes.
# NOTE: this isn't necessary since I'm using the shift_geo
# argument in the get_acs function. However if you're not
# using shift_geo or joining to a different spatial layer
# for the full US you may want to consider removing this
# record for display purposes.
# shp <- filter(shp, GEOID != "02016")

# tx <- filter(shp, STFIPS == "48") %>%
#   mutate(NAME = stringr::str_remove(NAME, ", Texas"),
#          `Insured Adults Ages 18-34, 2012-2016` = case_when(
#            uninsured_diff < 0  ~ "Insured population increased",
#            uninsured_diff > 0  ~ "Insured population decreased",
#            uninsured_diff == 0 ~ "Insured population stayed the same"),
#          diff2 = round(abs(uninsured_diff), 1),
#          popup = ifelse(uninsured_diff != 0,
#                         paste0(`Insured Adults Ages 18-34, 2012-2016`, " by ", diff2, "%"),
#                         `Insured Adults Ages 18-34, 2012-2016`),
#          diffrad = as.numeric(cut(diff2, c(0, 5, 10, 20, 30, 45),
#                                   right = FALSE)))

# Remove some unnecessary fields
# tx <- select(tx, -c(uninsured_2012, uninsured_2016, uninsured_diff, STFIPS))

# Basemap
carto <- "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}{r}.png"

# Create a "normal" tmap except we'll add
# the basemap and the `popup.vars` argument.
# The symbol size of the bubbles will be
# based on the data so use our calculated
# field `diffrad` which will apply sizes
# 1 through 5. Sizes can be further adjusted
# using the `scale` argument.

us_map <- shp %>%
  mutate(
    # NAME = stringr::str_remove(NAME, ", Texas"),
    `Insured Adults Ages 18-34, 2012-2016` = case_when(
      uninsured_diff < 0 ~ "Insured population increased",
      uninsured_diff > 0 ~ "Insured population decreased",
      uninsured_diff == 0 ~ "Insured population stayed the same"
    ),
    diff2 = round(abs(uninsured_diff), 1),
    popup = ifelse(uninsured_diff != 0,
      paste0(`Insured Adults Ages 18-34, 2012-2016`, " by ", diff2, "%"),
      `Insured Adults Ages 18-34, 2012-2016`
    ),
    diffrad = as.numeric(cut(diff2, c(0, 5, 10, 20, 30, 45),
      right = FALSE
    ))
  ) %>%
  slice_sample(n = 100)

mymap <- tm_basemap(carto) +
  tm_shape(us_map) +
  tm_borders(col = "azure2") +
  tm_bubbles("diffrad",
    col = "Insured Adults Ages 18-34, 2012-2016",
    border.col = "white",
    scale = 1.5,
    style = "fixed",
    palette = c("coral2", "aquamarine3", "gray"),
    popup.vars = c("County: " = "NAME", "Change: " = "popup")
  ) +
  tmap_options(check.and.fix = T)
  # tm_layout(legend.width = 0.5)

m <- tmap_leaflet(mymap)

library(htmlwidgets)
saveWidget(m, file = "~/Teaching Lab/Coding/teachinglab.github.io/example_map.html")
