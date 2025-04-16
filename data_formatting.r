library(stringr)
library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(rstac)
# ---- Polygones regions administratives Québec
reg <- st_read("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/QUEBEC_regions/BDGA_1M(adm)_SHP/regio_s.shp")
reg_conv <- st_transform(reg, crs = st_crs(4326))

# ---- ESA raster 2010 & 2022
io <- stac("https://io.biodiversite-quebec.ca/stac/")

ids <- io |>
    stac_search(collections = "esacci-lc") |>
    post_request() |>
    items_fetch() |>
    _$features |>
    sapply(X = _, function(i) {
        i$id
    })
# Conserver deux couches - 2010 à 2022
id_names <- ids[1:13]

cat_tot <- data.frame()
for (i in 1:length(id_names)) {
    print(paste0("----------> ", id_names[i]))
    url <- io |>
        stac_search(collections = "esacci-lc") |>
        post_request() |>
        items_fetch() |>
        _$features[[which(ids == ids[i])]]$assets[[1]]$href
    print("url retrieved")
    for (j in 1:nrow(reg_conv)) {
        rast <- rast(paste0("/vsicurl/", url))
        pol <- reg_conv[j, ]

        lc_rast <- crop(rast, pol)
        lc_rast <- mask(lc_rast, pol)
        print("crop & mask done")

        freq <- as.data.frame(freq(lc_rast))
        freq$layer <- id_names[i]
        freq$year <- as.numeric(substr(id_names[i], 11, 14))
        freq$region <- pol$RES_NM_REG

        cat_tot <- rbind(cat_tot, freq)
    }
}

# ---- recuperer toutes les valeurs de categories possibles
url <- io |>
    stac_search(collections = "esacci-lc") |>
    post_request() |>
    items_fetch() |>
    _$features[[which(ids == ids[1])]]$assets[[1]]$href
rast <- rast(paste0("/vsicurl/", url))

qc2022 <- crop(rast, reg_conv)
lc_rast <- mask(qc2022, reg_conv)
f <- freq(lc_rast)
cat <- f$value

# ---- ajouter les categories manquantes pour toutes les regions a chaque annee
ll <- split(cat_tot, list(cat_tot$region, cat_tot$year))

ll2 <- lapply(ll, function(x) {
    missing_cat <- cat[!cat %in% x$value]
    missing_df <- data.frame(
        layer = unique(x$layer),
        value = missing_cat,
        count = 0,
        year = unique(x$year),
        region = unique(x$region)
    )

    x <- rbind(x, missing_df)
    x
})

final <- do.call("rbind", ll2)
# saveRDS(final, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2022_frq_cat0_Qc_region_admin.rds")

# ---- regroupement des categories
