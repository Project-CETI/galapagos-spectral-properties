##Galapagos

library(dplyr)
library(ggplot2)
library(effects)
library(ggpubr)
library(reshape2)
library(xtable)
library(tidyr)     
library(stringr)  
library(purrr)

Galapagos<-read.csv(".../codasp.galapagos.20260423.csv") # loads the file with hand-annotated vowels



nrow(Galapagos)

Galapagos

Galapagos$nclicks<-nchar(Galapagos$autovpkcodastr)


sum(Galapagos$nclicks)

str(Galapagos)

Galapagos$nClick<-nchar(Galapagos$autovpkcodastr)


table(Galapagos$mismatchN)
xtable(t(table(Galapagos$mismatchN)))



#dominica comparison
vowelNoise <- read.csv(".../codasp_w_urn.csv") # loads the file with hand-annotated vowels

vowelNoise
nrow(vowelNoise)
xtable(t(table(vowelNoise$mismatchN)))






table(Galapagos$mismatchpct)
table(Galapagos$autovbycoda)

table(Galapagos$nClick)


mismatchPctGGPlot<-ggplot(Galapagos, aes(x = mismatchpct)) +
  geom_bar() +                       # `geom_bar()` uses `stat = "count"` by default
  labs(
    x = "Proportion of mismatched clicks",
    y = "Number of codas",
    title = "Mismatched clicks percentages"
  ) +
  theme_classic()

mismatchNGGplot<-ggplot(Galapagos[], aes(x = factor(mismatchN))) +
  geom_bar() +
  labs(
    x = "Number of mismatched clicks",  
    y = "Number of codas",
    title = "Mismatched click count"
  ) +
  theme_classic()

nrow(Galapagos[Galapagos$autovbycoda=="a",])

nrow(Galapagos[Galapagos$autovbycoda=="i",])



hist(Galapagos$mean_pk1)
hist(Galapagos[Galapagos$autovbycoda=="a",]$mean_pk1)
hist(Galapagos[Galapagos$autovbycoda=="i",]$mean_pk1)
hist(Galapagos[Galapagos$autovbycoda=="i",]$mean_pk2)



GalapagosViolin<-Galapagos[Galapagos$autovbycoda!="?",]

names(GalapagosViolin)[6] <- "Peak1"
names(GalapagosViolin)[7] <- "Peak2"

GalapagosViolinA<-GalapagosViolin[GalapagosViolin$autovbycoda=="a",]

GalapagosViolinI<-GalapagosViolin[GalapagosViolin$autovbycoda=="i",]

GalapagosViolinI<-GalapagosViolinI[c(3,6,7)]

GalapagosViolinI<-melt(
  GalapagosViolinI,
  id.vars = "autovbycoda",      # column(s) to keep fixed
  variable.name = "Peak",       # new column for names
  value.name = "value"          # new column for values
)


Aplot<-ggplot(aes(x=autovbycoda,y=Peak1,fill=autovbycoda),data=GalapagosViolinA)+geom_violin()+geom_boxplot(width=0.2)+ylim(2000,18000)  + theme_classic()+ theme(legend.position="none")+ylab("Mean Peak in Hz") +xlab("Coda vowel")

Iplot<-ggplot(aes(x=autovbycoda,y=value,fill=Peak),data=GalapagosViolinI)+geom_violin(position = position_dodge(width = 0.8))+geom_boxplot(width=0.2,position = position_dodge(width = 0.8))+ylim(2000,18000) + theme_classic()+ylab("Mean Peak in Hz")+xlab("Coda vowel")

ggarrange(Aplot,Iplot,widths=c(1,1.6))+ggtitle("Averaged peak in Hz for")












Galapagos<-read.csv(".../codasp.galapagos.20260423.csv") # loads the file with hand-annotated vowels






# ── helper: re‑ordering inside facets ───────────────────────────────────────
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  stats::reorder(paste(x, within, sep = sep), by, FUN = fun, ...)
}
scale_y_reordered <- function(..., sep = "___") {
  ggplot2::scale_y_discrete(labels = function(x)
    gsub(paste0(sep, ".+$"), "", x), ...)
}

# ── 0.  wide‑table helpers ─────────────────────────────────────────────────
Galapagos <- Galapagos %>% 
  dplyr::mutate(
    nClick   = nchar(autovpkcodastr),  # number of clicks
    firstICI = ICI1                    # convenient handle
  )

# ── 1.  gather ALL ICI columns ─────────────────────────────────────────────
ici_cols <- grep("^ICI\\d+$", names(Galapagos), value = TRUE)

plot_df <- Galapagos %>% 
  tidyr::pivot_longer(
    cols      = all_of(ici_cols),
    names_to  = "ici_name",
    values_to = "ici"
  ) %>% 
  dplyr::arrange(
    codanum,
    readr::parse_number(ici_name)      # ICI1, ICI2, …, ICI10, …
  ) %>% 
  dplyr::group_by(codanum) %>% 
  dplyr::mutate(
    keep       = dplyr::cumall(!is.na(ici)),           # drop values after 1st NA
    click_time = cumsum(dplyr::coalesce(ici, 0))
  ) %>% 
  dplyr::filter(keep) %>% 
  dplyr::ungroup()

# ── 2.  add first click (t = 0) ─────────────────────────────────────────────
first_clicks <- plot_df %>% 
  dplyr::distinct(codanum, nClick, firstICI) %>% 
  dplyr::mutate(
    ici_name   = "Start",
    ici        = NA_real_,
    click_time = 0
  )

plot_df <- dplyr::bind_rows(plot_df, first_clicks)

# ── 3.  order within facet by first ICI ─────────────────────────────────────
plot_df <- plot_df %>% 
  dplyr::mutate(
    coda = reorder_within(codanum, firstICI, nClick)   # uses helper above
  )
plot_df$nClick<-as.numeric(plot_df$nClick)

plot_df <- plot_df[!(plot_df$nClick %in% c(2, 13, 14, 15, 19, 26)), ]

# ── 4.  plot ────────────────────────────────────────────────────────────────
GalapagosPlot<-ggplot(
  plot_df,
  ggplot2::aes(x = click_time, y = coda, group = codanum)
) +
  #ggplot2::geom_line(linewidth = 0.4) +
  ggplot2::geom_point(size = .1) +
  ggplot2::facet_grid(
    cols   = vars(nClick),            # one row per click count
    scales = "free_x", space = "free_y"
  ) +
  ggplot2::labs(
    x     = "Time (s)",
    y     = NULL,
    title = "Galapagos codas by number of clicks ordered by ICI1"
  ) +
  scale_y_reordered() +               # strips the helper suffix
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    strip.text.y       = ggplot2::element_text(angle = 0),
    axis.text.y=element_blank()# facet labels upright
  )

GalapagosPlot






# --- 0) Choose p for the null ---
p_click <- 0.5
# Optional: estimate p from your data’s overall a/i ratio
# p_click <- sum(stringr::str_count(Galapagos$autovpkcodastr, "a"), na.rm = TRUE) /
#            sum(nchar(Galapagos$autovpkcodastr), na.rm = TRUE)

# --- 1) Observed counts of mismatchN (your column) ---
obs_tab <- table(Galapagos$mismatchN)
obs_df  <- as.data.frame(obs_tab) |>
  dplyr::rename(mismatchN = Var1, observed = Freq) |>
  dplyr::mutate(mismatchN = as.integer(as.character(mismatchN)))

# --- 2) Exact expected counts under randomness, given your nClick mix ----
mismatch_pmf_for_k <- function(k, p) {
  # Enumerate #a ~ Binomial(k, p), map to mismatchN = min(#a, k-#a)
  b  <- 0:k
  j  <- pmin(b, k - b)
  pr <- dbinom(b, size = k, prob = p)
  tibble(mismatchN = j, prob = pr) |>
    dplyr::group_by(mismatchN) |>
    dplyr::summarise(prob = sum(prob), .groups = "drop") |>
    tidyr::complete(mismatchN = 0:floor(k/2), fill = list(prob = 0))
}

k_tab <- table(Galapagos$nClick)                 # your counts per coda length
exp_df <- purrr::imap_dfr(as.list(k_tab), ~{
  k   <- as.integer(.y)
  n_k <- as.integer(.x)
  mismatch_pmf_for_k(k, p_click) |>
    dplyr::mutate(expected = n_k * prob)
}) |>
  dplyr::group_by(mismatchN) |>
  dplyr::summarise(expected = sum(expected), .groups = "drop")

# --- 3) Plot: observed vs expected (counts) --------------------------------
plot_counts <- dplyr::full_join(obs_df, exp_df, by = "mismatchN") |>
  tidyr::pivot_longer(cols = c(observed, expected),
                      names_to = "Type", values_to = "count") |>
  dplyr::mutate(series = factor(Type, levels = c("observed","expected")))

MismatchPredicted <- ggplot(plot_counts, aes(x = mismatchN, y = count, fill = Type)) +
  geom_col(position = "dodge") +
  labs(x = "Number of mismatched clicks",
       y = "Number of codas",
       title = "Observed vs. Random") +
  theme_classic()


ggarrange(mismatchNGGplot,mismatchPctGGPlot,MismatchPredicted,ncol=3,labels=c("a","b",
                                                                              "c"))





# Bin tail 5+ so expected counts are not tiny
obs_binned <- obs_df |>
  dplyr::mutate(bin = ifelse(mismatchN >= 5, "5+", as.character(mismatchN))) |>
  dplyr::group_by(bin) |>
  dplyr::summarise(observed = sum(observed), .groups = "drop")

exp_binned <- exp_df |>
  dplyr::mutate(bin = ifelse(mismatchN >= 5, "5+", as.character(mismatchN))) |>
  dplyr::group_by(bin) |>
  dplyr::summarise(expected = sum(expected), .groups = "drop") |>
  dplyr::mutate(p = expected / sum(expected))

# align bins
obs_binned <- dplyr::right_join(obs_binned, exp_binned |> dplyr::select(bin),
                                by = "bin") |>
  dplyr::mutate(observed = tidyr::replace_na(observed, 0))

chisq_res <- chisq.test(x = obs_binned$observed, p = exp_binned$p, rescale.p = TRUE)
chisq_res






#map


library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(geosphere)
library(ggspatial)
library(terra)

# -----------------------
# 1) Base map
# -----------------------
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# -----------------------
# 2) Study areas
# -----------------------
gal_bbox <- st_as_sfc(
  st_bbox(c(xmin = -92.0, xmax = -87.5,
            ymin =  -2.0, ymax =   1.9),
          crs = st_crs(4326))
)
gal_sf <- st_sf(site = "Galapagos", geometry = gal_bbox)

dom_sf <- st_sf(
  site = "Dominica",
  geometry = st_sfc(st_point(c(-61.40, 15.30)), crs = st_crs(4326))
)

# -----------------------
# 3) Plot extent
# -----------------------
xlim <- c(-120, -20)
ylim <- c(-40, 45)

site_cols <- c("Dominica" = "#d95f02", "Galapagos" = "#1b9e77")

# -----------------------
# Ocean relief raster
# -----------------------
cache_dir <- file.path(getwd(), "ne_cache")
dir.create(cache_dir, showWarnings = FALSE)

# save the raster to disk
rnaturalearth::ne_download(
  scale = 50,
  type = "OB_50M",
  category = "raster",
  destdir = cache_dir,
  load = FALSE
)

# load the saved raster properly
relief <- rnaturalearth::ne_load(
  scale = 50,
  type = "OB_50M",
  category = "raster",
  destdir = cache_dir
)

# crop to your plotting extent
relief <- terra::crop(
  relief,
  terra::ext(xlim[1], xlim[2], ylim[1], ylim[2])
)
p <- ggplot() +
  ggspatial::layer_spatial(relief, interpolate = TRUE) +
  geom_sf(data = land, fill = "grey90", color = "grey40", linewidth = 0.25) +
  geom_sf(data = gal_sf, aes(fill = site),
          color = "grey20", alpha = 0.35, linewidth = 0.7) +
  geom_sf(data = dom_sf, aes(fill = site),
          shape = 21, size = 3.2, color = "grey20", stroke = 0.6) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_fill_manual(values = site_cols, name = "Dataset") +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(caption = "Base map: Natural Earth") +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(size = 8, hjust = 0)
  )
p



ggarrange(p,Aplot,Iplot,widths=c(1,1,1.6),
          GalapagosPlot,
          mismatchNGGplot,mismatchPctGGPlot,MismatchPredicted
)

row1 <- ggarrange(p, Aplot, Iplot,
                  ncol = 3, widths = c(1.6, 1, 1.6),labels=c('a',"b","c"))

row2<-ggarrange(GalapagosPlot,labels=c("d"))


row3 <- ggarrange(mismatchNGGplot, mismatchPctGGPlot, MismatchPredicted,
                  ncol = 3,labels=c('e',"f","g"))

ggarrange(row1,
          row2,
          row3,
          nrow = 3,
          heights = c(1, 1.5, 1),labels="auto")




