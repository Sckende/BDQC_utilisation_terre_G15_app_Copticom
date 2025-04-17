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
    rast <- rast(paste0("/vsicurl/", url))
    for (j in 1:nrow(reg_conv)) {
        print(j)
        pol <- reg_conv[j, ]

        lc_rast <- crop(rast, pol)
        lc_rast <- mask(lc_rast, pol)

        print("crop & mask done")

        freq <- as.data.frame(freq(lc_rast))
        freq$layer <- id_names[i]
        freq$year <- as.numeric(substr(id_names[i], 11, 14))
        freq$region <- pol$RES_NM_REG
        freq$total_pix <- sum(freq$count, na.rm = TRUE)

        lc_rast_conv <- project(lc_rast, "EPSG:6624", method = "near") # pour le calcul de l'aire
        freq2 <- freq(lc_rast_conv)
        names(freq2)[3] <- "n_pix_proj6624"

        freq3 <- left_join(freq, freq2[, c("value", "n_pix_proj6624")], by = join_by(value))
        freq3$res6624 <- res(lc_rast_conv)[1]
        freq3$total_pix6624 <- sum(freq3$n_pix_proj6624, na.rm = TRUE)

        cat_tot <- rbind(cat_tot, freq3)
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
        region = unique(x$region),
        total_pix = unique(x$total_pix),
        n_pix_proj6624 = 0,
        res6624 = unique(x$res6624),
        total_pix6624 = unique(x$total_pix6624)
    )

    x <- rbind(x, missing_df)
    x
})

final <- do.call("rbind", ll2)
# saveRDS(final, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2022_frq_cat0_Qc_region_admin.rds")

# ---- regroupement des categories
freq <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2022_frq_cat0_Qc_region_admin.rds")
# retrait de la region cote nord qui est separee en 4 poly
freq <- freq[freq$region != "Côte-Nord", ]
cat <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/CCI-LC_Maps_Legend.csv", h = T)

freq2 <- left_join(freq, cat[, c("cat1", "copti_class")], by = join_by(value == cat1))

f_ll <- split(freq2, list(freq2$region, freq2$year))

f_ll2 <- lapply(f_ll, function(x) {
    region <- unique(x$region)
    year <- unique(x$year)
    total_pix <- unique(x$total_pix)
    res6624 <- unique(x$res6624)

    summ <- x |>
        group_by(copti_class) |>
        summarise(ntot_pix = sum(count, na.rm = TRUE), ntot_pix6624 = sum(n_pix_proj6624, na.rm = TRUE))

    # summ$prop_percent <- (summ$ntot_pix/total_pix) * 100
    # summ$area_km2 <- (summ$ntot_pix6624*res6624*res6624)/1000000
    summ$region <- region
    summ$year <- year
    summ$total_pix_reg <- total_pix
    summ$res6624 <- res6624
    summ
})

recap <- do.call("rbind", f_ll2)

recap_ll <- split(recap, list(recap$region, recap$copti_class))
recap_ll2 <- lapply(recap_ll, function(x) {
    # x$prop_comp_2010 <- x$prop_percent - x$prop_percent[x$year == 2010]
    # x
    x$prop_comp_2010 <- (x$ntot_pix - x$ntot_pix[x$year == 2010]) / x$ntot_pix[x$year == 2010] * 100 # suit la methode de calcul de ISQ
    # x$prop_comp_2010 <- (x$ntot_pix - x$ntot_pix[x$year == 2010]) / unique(x$total_pix_reg) * 100
    x$area_km2 <- (x$ntot_pix6624 * unique(x$res6624) * unique(x$res6624)) / 1000000
    x
})

fin <- do.call("rbind", recap_ll2)

saveRDS(fin, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2022_land_cover_trend_comp2010_area.rds")
