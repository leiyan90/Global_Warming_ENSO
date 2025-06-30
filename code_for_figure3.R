
TimeRange <- seq.Date(from = as.Date("1881/01/01",format = "%Y/%m/%d"), 
                      by = "month", length.out = 1704)
state = c(2, 1, 3, 5, 4)

y_axis_logit10_lower_bound <- -5 
y_axis_logit10_upper_bound <-  5 

plot0 <- list() 
k <- 1         

for (i in 1:5) { 
  for (j in 1:5) { 
    
    current_tp_values <- Trans_prob[state[i], state[j], ] 
    
    if (length(TimeRange) != length(current_tp_values)) {
      warning(paste("Skipping plot for i=", i, ", j=", j, 
                    ": TimeRange length (", length(TimeRange), 
                    ") does not match tp length (", length(current_tp_values), ")."))
      next 
    }
    dataf <- data.frame(TimeRange = TimeRange, tp = current_tp_values)
    
    dataf$logit10_tp <- ifelse(dataf$tp > 0 & dataf$tp < 1, 
                               log10(dataf$tp / (1 - dataf$tp)), 
                               NA_real_) 
    
    if (all(is.na(dataf$logit10_tp))) {
      message(paste("Skipping plot for i=", i, ", j=", j, 
                    " as all logit10_tp values are NA (e.g., all tp are 0, 1, or invalid)."))
      next 
    }
    
    x_title_text <- if(i == 5) "Observation years" else "" 
    y_base_title <- "log(Transition Odds)" 
    y_title_text <- if(j == 1) y_base_title else "" 
    
    plot0[[k]] <- ggplot(dataf, aes(x = TimeRange, y = logit10_tp)) + 
      geom_line(size = 0.2, color = "blueviolet", alpha = 1, na.rm = FALSE) + 
      geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "solid", size = 0.7, na.rm = TRUE) + 
      coord_cartesian(ylim = c(y_axis_logit10_lower_bound, y_axis_logit10_upper_bound)) + 
      ggtitle(paste0('From ', i, ' to ', j)) + 
      xlab(x_title_text) + 
      ylab(y_title_text) +
      #theme_minimal(base_size = 13) + 
      theme(
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 14), 
        #panel.grid.minor = element_blank() 
      )
    
    k <- k + 1
  }
}

if (length(plot0) > 0) {
  plot0_filtered <- plot0[!sapply(plot0, is.null)] 
  if (length(plot0_filtered) > 0) {
    aa <- wrap_plots(plot0_filtered, ncol = 5) 
    print(aa) 
  } else {
    message("No plots")
  }
} else {
  message("plot0 is empty。")
}

aa = wrap_plots(plot0, ncol = 5) 

ggsave("trans_prob_log(odds)4.tiff", aa, device = "tiff", width = 30, height = 30, units = "cm")
