
# Load dependencies -------------------------------------------------------

pacman::p_load(tidyverse, tidyquant, timetk, plotly, janitor)

# Load data ---------------------------------------------------------------

tbl <- read_rds("data/indata_for_shiny_app.rds")

# Process data ------------------------------------------------------------
## Make data long ---------------------------------------------------------
tbl_long <- tbl |> 
  select(
    everything(),
    symbol = ProductFront, # Product
    Volatility = volatility16day,
    `Close Price` = Price
  ) |> 
  distinct()


tbl_processed <- tbl_long |> # Gather types
  pivot_longer(
    cols = c(`Close Price`, Volatility),
    names_to = "type",
    values_to = "types_value"
  ) |> # Gather percentage percentile
  pivot_longer(
    cols = l1pct:l99pct,
    names_to = "percentile",
    values_to = "percentile_value"
  )


# Clean names, and filter out Momentum ------------------------------------

tbl_processed <- tbl_processed |> 
  mutate(
    percentile = paste(parse_number(percentile), "%")
  ) |> 
  clean_names()



# Plot function -----------------------------------------------------------


plot_ats <- function(data, types_value, exchange, type, product) {
  data |> 
    ggplot(aes(date, types_value, col = type)) +
    geom_line() +
    xlab("Date") +
    ylab(unique(data$type)) +
    labs(
      title = paste(exchange, type, product)
    ) +
    scale_color_tq(theme = "green") +
    theme_tq() +
    theme(legend.position = "none")
}



