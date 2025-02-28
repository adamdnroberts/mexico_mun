minimal_match$match = "minimal"
medium_match1$match = "medium1" 
medium_match2$match = "medium2" 
  full_match1$match = "full1"
  full_match2$match = "full2"

test <- rbind(minimal_match, medium_match1, medium_match2, full_match1, full_match2)

matching_plot1 <- ggplot(subset(test, bw_type == "MSE" & match != "medium2" & match != "full2"), aes(x = n, y = est
                                                , color = match)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = -0.5)) +
  geom_point(position = position_dodge(width = -0.5)) +
  geom_hline(yintercept = 0, color = "red", linetype = 3) +
  labs(x = "Number of References in Weighted Average", y = "RD Estimate", title = "Matching RD Estimates by number of references included"
       , subtitle = "Controlling for matching distance, state and year fe"
  ) +
  theme_minimal() 

print(matching_plot1)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/num_refs_different_matches1.png", plot = matching_plot1, width = 6, height = 4)


matching_plot2 <- ggplot(subset(test, bw_type == "MSE" & match != "medium1" & match != "full1"), aes(x = n, y = est
                                                                                   , color = match
)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = -0.5)) +
  geom_point(position = position_dodge(width = -0.5)) +
  geom_hline(yintercept = 0, color = "red", linetype = 3) +
  labs(x = "Number of References in Weighted Average", y = "RD Estimate", title = "Matching RD Estimates by number of references included"
       , subtitle = "Controlling for matching distance, state and year fe"
  ) +
  theme_minimal() 

print(matching_plot2)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/Third Year Paper Results Outline/images/num_refs_different_matches1.png", plot = matching_plot2, width = 6, height = 4)
