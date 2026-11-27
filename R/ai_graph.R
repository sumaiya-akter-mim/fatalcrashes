ai_graph <- function(d) {
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  # Exclude unknown lighting conditions
  df <- d %>%
    filter(light_group != "Unknown") %>%
    group_by(state) %>%
    summarise(
      n = n(),
      dark_share = mean(light_group == "Dark"),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(dark_share))
  
  # National share (among crashes with known lighting)
  national_share <- d %>%
    filter(light_group != "Unknown") %>%
    summarise(share = mean(light_group == "Dark")) %>%
    pull(share)
  
  # Highlight the five states with the highest dark-crash share
  df <- df %>%
    mutate(
      highlight = if_else(row_number() <= 5, "Top 5", "Other"),
      state = factor(state, levels = rev(state))
    )
  
  # Plot
  ggplot(df, aes(x = dark_share, y = state, color = highlight)) +
    geom_point(size = 3) +
    geom_vline(
      xintercept = national_share,
      linetype = "dashed",
      color = "black"
    ) +
    scale_color_manual(
      values = c("Top 5" = "#2a78d6", "Other" = "grey65")
    ) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1)
    ) +
    labs(
      title = paste0(
        "Share of fatal crashes occurring in the dark\n",
        "National share: ",
        percent(national_share, accuracy = 0.1)
      ),
      x = "Share of fatal crashes in dark conditions",
      y = NULL,
      color = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}