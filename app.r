# ================================================================================
# Chargement packages & data
# ================================================================================

#### Packages ####
# -------------- #
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyhelper)

library(sf)
library(dplyr)
library(stringr)
library(knitr)
library(plotly)
library(ggplot2)
library(terra)

options(shiny.launch.browser = TRUE)
#### Local data ####
# ---------------- #
# utilisation des terres
comp <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_comp2010_ISQ_reg.rds")

# donnees prop habitats naturels ISQ
isq <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/ISQ/QC_meridional_natural_land_prop_2010.csv", h = T, encoding = "latin1")
isq_nat <- isq[isq$year == 2010, ]
isq_nat$region[isq_nat$region == "Gaspésie-Îles-de-la-Madeleine"] <- "Gaspésie–Îles-de-la-Madeleine"
# donnees prop habitats naturels ESA
esa_nat <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/ESA_prop_hab_nat_2010.rds")


# Several Polygons for Qc
# -----------------------
# ---- data pour decoupage admin
# dec <- st_read("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/QUEBEC_regions/BDGA_1M(adm)_SHP/regio_s.shp")
dec <- st_read("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/QUEBEC_regions/QC_admin_reg_simplify.gpkg")
# selection des regions d'interet
admin <- dec[dec$RES_NM_REG %in% c(
    "Bas-Saint-Laurent",
    "Capitale-Nationale",
    "Estrie",
    "Montréal",
    "Outaouais",
    "Abitibi-Témiscamingue",
    "Gaspésie–Îles-de-la-Madeleine",
    "Chaudière-Appalaches",
    "Laval",
    "Lanaudière",
    "Laurentides",
    "Montérégie"
), ]
# test


server <- function(input, output, session) {
    observe_helpers()


    # Ref map plot
    output$map_ref <- renderPlot({
        par(mar = rep(0, 4))
        plot(st_geometry(dec), col = ifelse(dec$RES_NM_REG %in% admin$RES_NM_REG, "grey", "#bebebe67"))
        plot(st_geometry(dec[dec$RES_NM_REG == input$admin_select, ]), col = "red", add = TRUE)
    })

    # trend plot
    output$trend_plot <- renderPlot({
        x <- comp[comp$region == input$admin_select, ]

        ggplot(
            data = x,
            aes(x = year, y = comp_rate, group = IPCC_class, colour = IPCC_class)
        ) +
            geom_line(linewidth = 1) +
            labs(title = unique(x$region), color = "Catégories") +
            xlab("Année") +
            ylab("Proportion (%)") +
            scale_x_continuous(n.breaks = length(unique(x$year))) +
            scale_color_manual(
                labels = c("agriculture", "forêt", "prairie", "autres", "habitation", "milieu humide"),
                values = c(
                    "#993300",
                    "#006600",
                    "#CC9900",
                    "#96ac9d",
                    "#CC0000",
                    "#019191"
                )
            )
    })

    # pie chart
    output$pie <- renderPlotly({
        data <- comp[comp$region == input$admin_select & comp$year == 2010, ]
        colors <- c(
            "#993300", # agriculture
            "#006600", # forest
            "#CC9900", # grassland
            "#96ac9d", # other
            "#CC0000", # settlement
            "#019191" # wetland
        )

        fig <- plot_ly(data,
            labels = c("agriculture", "forêt", "prairie", "autres", "habitation", "milieu humide"),
            # labels = ~IPCC_class,
            values = ~count_tot,
            type = "pie",
            textposition = "inside",
            textinfo = "label+percent",
            insidetextfont = list(color = "#FFFFFF"),
            hoverinfo = "text",
            text = ~ paste("n = ", count_tot),
            marker = list(
                colors = colors,
                line = list(color = "#FFFFFF", width = 1)
            ),

            # The 'pull' attribute can also be used to create space between the sectors

            showlegend = FALSE
        )

        fig <- fig %>% layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })

    # comparaison prop habitats naturels ESA vs. ISQ
    output$hab_nat <- renderPlotly({
        reg <- input$admin_select

        # data formatting
        esa <- esa_nat[esa_nat$region == reg, ]
        esa$data <- "esa"
        names(esa)[1] <- "nat_hab"

        isq <- isq_nat[isq_nat$region == reg, ]
        isq$prop <- isq$prop * 100
        isq$data <- "isq"
        isq$region <- reg

        cp <- rbind(isq, esa[, c("region", "nat_hab", "prop", "year", "data")])
        cp <- cp |> mutate(text = sprintf("Données: %s<br> Proportion: %s <br>", data, round(prop, digits = 2)))
        cp <- cp[order(cp$data, cp$nat_hab), ]
        cp$nat_hab[cp$nat_hab == "forest"] <- "forêt"
        cp$nat_hab[cp$nat_hab == "water"] <- "eau"
        cp$nat_hab[cp$nat_hab == "wetland"] <- "milieu humide"

        # figure
        fig <- cp %>% plot_ly()

        fig <- fig %>% add_trace(
            x = cp$nat_hab[cp$data == "esa"],
            y = cp$prop[cp$data == "esa"],
            type = "bar",
            text = cp$text[cp$data == "esa"],
            textposition = "auto",
            marker = list(
                color = c("rgba(46,72,62,0.8)", "rgba(123,181,177, 0.8)", "rgba(166,97,45, 0.8)"),
                line = list(
                    color = c("rgba(46,72,62,1)", "rgba(123,181,177, 1)", "rgba(166,97,45, 1)"),
                    width = 1.5
                )
            )
        )

        fig <- fig %>% add_trace(
            x = cp$nat_hab[cp$data == "isq"],
            y = cp$prop[cp$data == "isq"],
            type = "bar",
            text = cp$text[cp$data == "isq"],
            textposition = "auto",
            marker = list(
                color = c("rgba(46,72,62,0.5)", "rgba(123,181,177, 0.5)", "rgba(166,97,45, 0.5)"),
                line = list(
                    color = c("rgba(46,72,62,1)", "rgba(123,181,177, 1)", "rgba(166,97,45, 1)"),
                    width = 1.5
                )
            )
        )

        fig <- fig %>%
            layout(
                # title = "Comparaison proportion milieux naturels ESA - ISQ",
                barmode = "group",
                xaxis = list(title = "Type"),
                yaxis = list(title = "Proportion (%)"),
                showlegend = FALSE
            ) |>
            style(hoverinfo = "none")
    })
}

ui <- navbarPage(
    "Évolution de l'utilisation des terres au Qc",
    sidebarLayout(
        sidebarPanel(
            h4("Région administrative"),
            width = 2,
            selectInput(
                inputId = "admin_select",
                label = "",
                choices = admin$RES_NM_REG[order(admin$RES_NM_REG)]
            )
            # )
            ,
            plotOutput("map_ref", width = "100%")
        ),
        mainPanel(
            # First row
            fluidRow(
                box(
                    title = "Évolution utilisation des terres ESA",
                    width = 8,
                    plotOutput("trend_plot")
                ),
                box(
                    title = "Proportion de type de terre - ESA 2010",
                    width = 8,
                    plotlyOutput("pie")
                )
            ),
            fluidRow(
                box(
                    title = "Proportion des milieux naturels ESA - ISQ (2010)",
                    width = 6,
                    plotlyOutput("hab_nat")
                )
            )
        )
    )
)

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
