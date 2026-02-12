# Validating R code from dashboard.qmd

# Load libraries
library(tidyverse)
library(palmerpenguins)
library(gapminder)
library(plotly)
library(ggrepel)
library(viridis)

tryCatch(
    {
        data(penguins)
        # Clean penguins data
        dt_clean <- penguins |>
            filter(
                !is.na(bill_length_mm),
                !is.na(bill_depth_mm),
                !is.na(flipper_length_mm),
                !is.na(body_mass_g),
                !is.na(sex)
            )
        print("Data cleaning successful")
    },
    error = function(e) {
        print(paste("Error in data cleaning:", e$message))
    }
)

tryCatch(
    {
        data(gapminder)
        # Prepare gapminder data for 2007 Americas
        gap_07_americas <- gapminder |>
            filter(year == 2007, continent == "Americas")
        print("Gapminder data prep successful")
        print(head(gap_07_americas))
    },
    error = function(e) {
        print(paste("Error in gapminder data prep:", e$message))
    }
)

# Plot 1
tryCatch(
    {
        p1 <- ggplot(dt_clean, aes(bill_length_mm, bill_depth_mm, color = species)) +
            geom_point() +
            scale_color_viridis_d(option = "plasma") +
            theme_minimal() +
            labs(x = "Bill Length (mm)", y = "Bill Depth (mm)")
        print(p1)
        print("Plot 1 created successfully")
    },
    error = function(e) {
        print(paste("Error in Plot 1:", e$message))
    }
)

# Plot 2
tryCatch(
    {
        p2 <- ggplot(dt_clean, aes(x = island, y = body_mass_g, fill = island)) +
            geom_boxplot() +
            scale_fill_brewer(palette = "Dark2") +
            theme_minimal() +
            labs(x = "Island", y = "Body Mass (g)")
        print(p2)
        print("Plot 2 created successfully")
    },
    error = function(e) {
        print(paste("Error in Plot 2:", e$message))
    }
)

# Plot 3
tryCatch(
    {
        p3 <- ggplot(gap_07_americas, aes(x = gdpPercap, y = reorder(country, gdpPercap))) +
            geom_segment(aes(x = 0, xend = gdpPercap, y = reorder(country, gdpPercap), yend = reorder(country, gdpPercap)),
                color = "grey70"
            ) +
            geom_point(color = "#2A5196", size = 2.5) +
            scale_x_continuous(labels = scales::label_dollar()) +
            theme_minimal() +
            labs(x = "GDP per Capita", y = "Country")
        print(p3)
        print("Plot 3 created successfully")
    },
    error = function(e) {
        print(paste("Error in Plot 3:", e$message))
    }
)

# Plot 4 (Interactive)
tryCatch(
    {
        p_interactive <- ggplot(gap_07_americas, aes(
            x = gdpPercap, y = lifeExp,
            text = paste(
                "Country:", country,
                "<br>Life Exp:", lifeExp,
                "<br>GDP:", gdpPercap
            )
        )) +
            geom_point(aes(color = country), show.legend = FALSE) +
            scale_x_log10(labels = scales::dollar) +
            labs(x = "GDP per Capita (log scale)", y = "Life Expectancy") +
            theme_minimal()

        # We can't easily validtate plotly output in script without a viewer, but checking creation is good
        p_plotly <- ggplotly(p_interactive, tooltip = "text")
        print("Plot 4 (Interactive) created successfully")
    },
    error = function(e) {
        print(paste("Error in Plot 4:", e$message))
    }
)
