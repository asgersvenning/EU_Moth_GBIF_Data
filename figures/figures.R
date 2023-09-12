######### FIGURES ############
library(kableExtra)

GBIF_sample_quality %>% 
  mutate(
    # quality = ifelse(quality %in% c("Blurry", "Larvae", "Other"),
    #                  "LQ" ,"HQ"),
    source = identifier %>% 
      str_remove("www\\.") %>% 
      str_remove("files\\.") %>% 
      str_extract("(?<=\\/{2}).+\\.[a-z]{2,3}(?=\\/)"),
    quality = factor(quality, c(
      "Other", "Larvae", "Blurry", "Visible", "Clear", "HD"
    ))
  ) %>% 
  count(quality, source) %>% 
  complete(quality, source, fill= list(n = 0)) %>% 
  pivot_wider(id_cols = source, names_from = quality, values_from = n) %>% 
  rename(Source = source) %>% 
  kable("latex", booktabs = T)

############# Sample distribution plot
######################################
#### Unused code for now
## GBIF_combined %>% 
##   ggplot(aes(decimalLongitude, decimalLatitude)) +
##   geom_bin_2d(bins = 1000) +
##   scale_fill_viridis_c(option = "A", trans = "log10") 

sample_distribution_plot <- GBIF_combined %>% 
  count(match, sort = T) %>% 
  right_join(geography, c(match = "list_country")) %>%  
  st_set_geometry("geometry") %>% 
  mutate(
    area = st_area(geometry) %>% 
      units::set_units("km^2") %>% 
      units::drop_units()
  ) %>% 
  st_simplify(F, 2500) %>% 
  ggplot(aes(fill = n / area)) +
  geom_sf(#aes(color = after_scale(fill)),
    color = "black",
    linewidth = .05
  ) +
  scale_fill_viridis_c(option = "A", trans = "log10") +
  scale_x_continuous(breaks = seq(-90, 90, 10)) +
  scale_y_continuous(breaks = seq(-90, 90, 5)) +
  labs(fill = "Images\npr. km^2",
       title = "Sample density distribution in locally\nrecognised moth species on GBIF") +
  theme(
    panel.grid.major = element_line(color = "gray80",
                                    linetype = "dashed",
                                    linewidth = .35),
    legend.title = element_text(hjust = .5),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(4, "cm"),
    legend.text = element_text(size = 16)
  )

ggsave("figures/sample_distribution.pdf", sample_distribution_plot,
       device = cairo_pdf, width = 8, height = 6.25, scale = 1.5)

## Taxonomic distribution
library(ggforce)
library(ggraph)

parse_arguments <- function(...) {
  lapply(substitute(list(...))[-1], function(x) {
    m <- storage.mode(x)
    if (m == "language") {
      sapply(2:length(x), function(y) {
        y <- x[[y]]
        if (storage.mode(y) == "character") return(y)
        if (length(y) != 1) return(eval(call("test", y)))
        return(as.character(y))
      })
    } else as.character(x)
  }) %>% 
    unlist(F)
}

add_root <- function(df, what = names(df)[ncol(df) - 1]) {
  
  what <- str2lang(what)
  
  df %>% 
    mutate(across({{what}}, ~factor(.x, c(unique(.x), "root")))) %>%
    mutate(across(where(is.list), ~map(.x, add_root))) %>% 
    complete({{what}})
}

create_graph_from_hierarchy <- function(df, ...) {
  levels <- parse_arguments(...) %>% 
    unlist
  
  df <- df %>% 
    select(all_of(levels)) %>% 
    count(across(all_of(levels)))
  
  nest_levels <- levels
  nest_depth <- 1
  
  while (length(nest_levels) >= 2) {
    nest_levels <- nest_levels[-1]
    
    df <- df %>% 
      nest(!! paste0("L", nest_depth) := !all_of(nest_levels))
    
    nest_depth <- nest_depth + 1
  } 
  
  df <- df %>% 
    add_root
  
  for (i in 1:(nest_depth - 1)) {
    df <- df %>% 
      unnest(keep_empty = T) %>% 
      suppressWarnings() %>% 
      suppressMessages()
  }
  
  df <- df %>% 
    mutate(
      node = row_number()
    )
  
  df <- df %>% 
    group_by(across(all_of(levels[2]))) %>% 
    mutate(
      to = node[.data[[levels[1]]] == "root"]
    ) %>% 
    ungroup
  
  for (level in 3:length(levels)) {
   df <- df %>% 
     group_by(across(all_of(levels[level]))) %>% 
     mutate(
       to = ifelse(.data[[levels[level - 2]]] == "root",
                   node[.data[[levels[level - 1]]] == "root"],
                   to)
     ) %>% 
     ungroup
  }
  
  df <- df %>% 
    filter(.data[[levels[length(levels)]]] != "root")
  
  df_edges <- df %>% 
    select(node, to) %>% 
    rename(from = node) %>% 
    drop_na
  
  df_nodes <- df %>% 
    select(!to)
  
  tbl_graph(df_nodes, df_edges)
}

GBIF_graph <- GBIF_combined %>% 
  create_graph_from_hierarchy(acceptedScientificName, genus, family, order)

grp_wm <- function(x, g, w, min = 10) {
  # if (na.rm) {
  #   g <- g[!is.na(x)]
  #   x <- x[!is.na(x)]
  # }
  for (i in unique(g)) {
    ix <- weighted.mean(x[g == i], w[g == i], na.rm = T)
    if (sum(g == i) < min) ix <- NA
    x[g == i] <- ix
  }
  x
}

ColourPalleteMulti <- function(df, group, subgroup) {
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~" )), df, function(x) length(unique(x)))
  category.start <- (scales::hue_pal(l = 100)(nrow(categories))) # Set the top of the colour pallete
  category.end  <- (scales::hue_pal(l = 40)(nrow(categories))) # set the bottom
  
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])}))
  return(colours)
}

family_genus_pal <- as_tibble(GBIF_graph) %>%
  mutate(across(c(family, genus), ~replace_na(as.character(.x), "NA"))) %>% 
  ColourPalleteMulti("family", "genus")

observation_number_plot <- GBIF_graph %>% 
  mutate(
    weight = ifelse(is.na(n), 0.01, n),
    family = fct_shuffle(family),
    rank = node_distance_to(node_is_root()),
    family_genus = paste0(family, "-", genus)
    # color = family_genus_pal[as.numeric(factor(family_genus))]
  ) %>% 
  ggraph("treemap", weight = weight,
         direction = "in") +
  geom_node_tile(aes(fill = family_genus, alpha = leaf)) +
  geom_mark_circle(
    aes(x = ifelse(genus == "root", 
                   grp_wm(x, family, 
                           w = width * height), NA), 
        y = ifelse(genus == "root", 
                   grp_wm(y, family, 
                           w = width * height), NA),
        group = family, label = family
    ),
    color = "black"
  ) +
  # scale_fill_identity() +
  scale_fill_manual("Subject", values = family_genus_pal,
                    guide = guide_none()) +
  scale_alpha_discrete(guide = guide_none(),
                         range = c(0, 1)) +
  coord_cartesian(expand = F) +
  theme_void()

ggsave("figures/observation_number.pdf", observation_number_plot,
       device = cairo_pdf,
       width = 8, height = 6, scale = 2.5) 

# Rarity distribution

species_SFS <- GBIF_combined %>% 
  select(acceptedScientificName, genus, family, order) %>% 
  mutate(
    family = fct_lump_n(family, 8)
  ) %>% 
  add_count(family) %>% 
  arrange(n) %>% 
  mutate(family = factor(family, unique(family)) %>% 
           fct_rev %>% 
           fct_relevel("Other", after = Inf)) %>% 
  select(!n) %>%  
  count(acceptedScientificName) %>% 
  count(n, name = "f") %>% 
  mutate(cf = cumsum(f * n)/sum(f * n)) %>% 
  ggplot(aes(n, f)) +
  geom_col(key_glyph = draw_key_point,
           color = "gray50") +
  geom_line(aes(y = cf * max(f)),
            color = "firebrick") +
  scale_x_log10(
    labels = Vectorize(function(x) {
      if (!is.finite(x)) return(x)
      base <- log10(x)
      if (base < 3) return(latex2exp::TeX(paste0("$",x,"$")))
      latex2exp::TeX(paste0("$10^{",base,"}$"))
    }),
    breaks = 10^(0:10),
    limits = c(0.75,  1.5 * 10^5),
    expand = expansion()
  ) +
  scale_y_continuous(
    expand = expansion(c(0, 0.1)),
    breaks = function(x) {
      if (x[2] > 20) c(1, seq(10, 1000, 10)) else if (x[2] > 10) seq(0, 30, 2) else seq(0, 20, 1)
    },
    sec.axis = sec_axis(trans = ~./(max(.) / 1.1), 
                        name = "Cumulative no. Observation",
                        labels = scales::label_percent())
  ) +
  # scale_color_manual(values = c(RColorBrewer::brewer.pal(8, "Dark2"), "royalblue1")) +
  # guides(color = guide_legend(override.aes = list(size = 5),
  #                             ncol = 1)) +
  labs(x = "No. Observations", y = "No. Species",
       color = "Family") +
  theme(legend.title.align = 0.5,
        legend.background = element_rect(
          color = "black",
          fill = rgb(243, 243, 237, maxColorValue = 255)
        ),
        panel.background = element_rect(fill = "#00000009"),
        plot.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(vjust = 0,
                                   margin = margin(0,0,0.2,0,"cm")),
        axis.text.y.right = element_text(margin = margin(0,0.25,0,0,"cm")),
        axis.line.y.right = element_line(color = "firebrick"),
        axis.ticks.y.right = element_line(color = "firebrick"))

ggsave("figures/species_SFS.pdf", species_SFS,
       device = cairo_pdf, width = 8, height = 4,
       scale = 1)

shift_trans = function(d = 0) {
  scales::trans_new("shift", transform = function(x) x - d, inverse = function(x) x + d)
}

data_distribution_rebalanced <- GBIF_combined %>% 
  count(acceptedScientificName, genus, family, order) %>% 
  filter(n > 25) %>% 
  mutate(n = pmin(n, 2000)) %>% 
  group_by(family, genus, order) %>% 
  summarize(n = sum(n), .groups = "drop") %>% 
  reframe(
    across(!n, ~c(.x, "Total")),
    n = c(n, sum(n))
  ) %>% 
  group_by(family) %>% 
  arrange(family, desc(n)) %>% 
  mutate(fam_n = sum(n),
         n_end = cumsum(n) + 1,
         n_start = lag(n_end, default = 1),
         n_mid = (n_end + n_start) / 2,
         n_width = n_end - n_start,
         last_genus_in_family = row_number() == n()) %>% 
  ungroup %>% 
  arrange(order, fam_n, desc(n), family, genus) %>% 
  mutate(across(where(is.character), ~factor(.x, unique(.x)))) %>% 
  ggplot(aes(y = family, group = genus)) +
  geom_tile(
    aes(x = n_mid, width = n_width, 
        fill = n / sum(n[n != max(n)]) * ifelse(n == max(n), NA, 1),
        color = after_scale(fill)),
    height = 0.8,
    linewidth = 0.2
    ) +
  geom_tile(
    aes(y = family, 
        x = fam_n / 2 * ifelse(last_genus_in_family, 1, NA), 
        width = fam_n),
    color = "black",
    height = 0.8,
    fill = "transparent",
    linewidth = .5
  ) +
  geom_label(
    aes(
      x = (fam_n + 75) * 1.05 * ifelse(last_genus_in_family, 1, NA), y = family,
      label = paste0(
        str_replace_all(as.character(fam_n), "(?<!^)(?=(\\d{3}){1,10}$)", " "),
        " (",
        scales::label_percent(.01, drop0trailing = T)(fam_n / n[family == "Total"]),
        ")"
      )
    ),
    hjust = 0, vjust = .5,
    family = "CMU Serif",
    fill = "gray95",
    color = "black",
    label.size = 0,
    label.r = unit(0, "mm")
  ) +
  scale_x_continuous(
    expand = expansion(c(0,0.11)),
    breaks = c(100, rep(10^seq(3, 10), each = 2) * c(1, 3)),
    minor_breaks = c(),
    labels = label_prettify_scientific(T,2,.5)
  ) +
  scale_fill_distiller(
    palette = "Greys",
    trans = "log10",
    labels = scales::label_percent(drop0trailing = T),
    limits = c(10^-5, 0.1),
    breaks = 10^seq(-5, 0, 1),
    direction = 1,
    na.value = "black"
  ) +
  scale_y_discrete(expand = expansion()) +
  coord_trans(x = scales::pseudo_log_trans(1000)) +
  labs(
    x = "No. Observations", 
    y = "Family", 
    fill = "Genera % of\nObservations\nin Data Set",
    title = "Data Distribution after Class Rebalancing" 
  ) +
  theme(
    axis.text.x.bottom = element_text(vjust = 0),
    panel.grid.major.x = element_line(color = "black",
                                      linetype = "dashed",
                                      linewidth = .25),
    panel.grid.minor.x = element_line(color = "black",
                                      linetype = "solid",
                                      linewidth = .05),
    legend.title.align = 0.5,
    legend.key.height = unit(2.5, "cm")
  ) 

ggsave("figures/data_distribution_rebalanced.pdf", data_distribution_rebalanced,
       device = cairo_pdf, width = 8, height = 4, scale = 2)  
