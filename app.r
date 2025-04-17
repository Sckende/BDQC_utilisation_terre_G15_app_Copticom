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
comp <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2022_land_cover_trend_comp2010_area.rds")

# Several Polygons for Qc
# -----------------------
# ---- data pour decoupage admin
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
    observeEvent(input$type_visu, {
        if (input$type_visu == "Proportion") {
            updateRadioGroupButtons(session, "range_y_scale", choices = c("Variable", "Fixe"))
        } else {
            updateRadioGroupButtons(session, "range_y_scale", choices = "Variable")
        }
    })

    data <- reactive({
        x <- comp[comp$region == input$admin_select, ]
        if (input$type_visu == "Proportion") {
            x$prop_comp_2010
        } else {
            x$area_km2
        }
    })

    output$plot <- renderPlot({
        x <- comp[comp$region == input$admin_select, ]
        g <- ggplot(
            data = x,
            aes(x = year, y = data(), group = copti_class, colour = copti_class)
        ) +
            geom_line(linewidth = 1) +
            labs(color = "Catégories") +
            xlab("Année") +
            scale_x_continuous(n.breaks = length(unique(x$year))) +
            scale_color_manual(
                labels = c("agriculture", "autres milieux naturels", "bâti", "eau", "forêt", "milieux humides", "végétation basse"),
                values = c(
                    "#993300",
                    "#727e07",
                    "#CC0000",
                    "#012d8b",
                    "#006600",
                    "#96ac9d",
                    "#CC9900"
                )
            )
        if (input$type_visu == "Proportion") {
            g <- g +
                ylab("Proportion (%)") +
                labs(title = "Evolution de la proportion pour chaque type de couverture des terres (comparaison ave 2010)")

            if (input$range_y_scale == "Fixe") {
                g <- g +
                    ylim(breaks = c(-40, 125))
            }
            g
        } else {
            g <- g +
                ylab("Superficie (km2)") +
                scale_y_log10() +
                labs(title = "Évolution de la superficie pour chaque type de couverture des terres")
            g
        }
    })
}
ui <- fluidPage(
    fluidRow(
        column(
            3,
            selectInput(
                inputId = "admin_select",
                label = "",
                choices = admin$RES_NM_REG[order(admin$RES_NM_REG)]
            ),
            plotOutput("map_ref", width = "100%"),
            radioGroupButtons("type_visu",
                label = "Échelle des y",
                choices = c("Proportion", "Superficie"),
                selected = "Superficie"
            ),
            radioGroupButtons("range_y_scale",
                label = "Valeurs pour l'axe y",
                choices = c("Variable", "Fixe"),
                selected = "Variable"
            )
        ),
        column(9, plotOutput("plot"))
    )
)

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
