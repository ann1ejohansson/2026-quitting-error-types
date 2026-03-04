## entropy functions 

plot_entropy_items <- function(only_single_digit = TRUE, 
                               n_items, entropy, top_errors, response_colors,
                               decreasing = TRUE,
                               subtitle_text = "") {
  
  # filter entropy
  if (only_single_digit) {
    entropy <- entropy %>% 
      filter(single_digit)
    top_errors <- top_errors %>% 
      filter(single_digit)
  }
  
  # select items
  select_items <- entropy %>%
    arrange(if (decreasing) desc(dominance) else dominance) %>%
    slice_head(n = n_items) %>%
    pull(question_clean)
  
  # build plot
  top_errors %>%
    filter(question_clean %in% select_items) %>%
    left_join(entropy, by = "question_clean") %>%
    
    mutate(
      facet_label = paste0(question_clean, "\n -entropy = ", round(dominance, 2)),
      facet_label = factor(facet_label, levels = unique(facet_label[order(-dominance)])),
      answer_str = as.character(answer)
    ) %>%
    filter(str_length(answer_str) < 6) %>%
    
    group_by(question_clean) %>%
    slice_head(n = 15) %>%
    ungroup() %>%
    
    mutate(answer = reorder_within(answer, n, question_clean)) %>%
    
    ggplot(aes(answer, n)) +
    geom_bar(stat = "identity", aes(fill = response, color = response), alpha = 0.4) +
    scale_discrete_manual(
      aesthetics = c("fill", "color"),
      values = response_colors,
      name = "type"
    ) +
    labs(
      title = "Number of responses per error",
      subtitle = subtitle_text,
      y = "n",
      x = "answer",
      caption = "Note. Time-out and skipped responses were not included in entropy calculation."
    ) +
    coord_flip() +
    facet_wrap(~facet_label, scales = "free_y", ncol = 3) +
    scale_x_reordered() +
    plot_theme
}
