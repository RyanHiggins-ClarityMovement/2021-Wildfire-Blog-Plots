load("model_output_data.Rdata")
pacman::p_load(tidyverse, ggplot2, plotly, tidyr, broom, lubridate, zoo,
               leaflet,leafgl,sf,colourvalues, ClaRity, htmlwidgets)

i <- 4
tz <- "America/Boise"
selected_independent <- "Particulate Matter (2.5) Mass Concentration - Reference"
pollutant_modeling <- "Particulate Matter (2.5) Mass Concentration"
accuracy_range_low <- case_when(
  pollutant_modeling == "Particulate Matter (1) Mass Concentration" ~ 10,
  pollutant_modeling == "Particulate Matter (2.5) Mass Concentration" ~ 10,
  pollutant_modeling == "Particulate Matter (10) Mass Concentration" ~ 30,
  pollutant_modeling == "Nitrogen Dioxide" ~ 30)

accuracy_range_cutpoint <- case_when(
  pollutant_modeling == "Particulate Matter (1) Mass Concentration" ~ 100,
  pollutant_modeling == "Particulate Matter (2.5) Mass Concentration" ~ 100,
  pollutant_modeling == "Particulate Matter (10) Mass Concentration" ~ 200,
  pollutant_modeling == "Nitrogen Dioxide" ~ 200)

accuracy_range_high_percent <- case_when(
  pollutant_modeling == "Particulate Matter (1) Mass Concentration" ~ 10,
  pollutant_modeling == "Particulate Matter (2.5) Mass Concentration" ~ 10,
  pollutant_modeling == "Particulate Matter (10) Mass Concentration" ~ 15,
  pollutant_modeling == "Nitrogen Dioxide" ~ 15)
temp_calibrated_hourly <- evaluation_combined_hourly %>% 
  filter(`Site ID` == eval_collos$`Site ID`[i], Date > eval_collos$startTime[i], Date <= eval_collos$endTime[i]) %>% 
  ungroup() 

temp_stats <- temp_calibrated_hourly %>%
  select(Date, 
         `Particulate Matter (2.5) Mass Concentration - Calibrated`,
         `Particulate Matter (2.5) Mass Concentration - Raw`,
         `Particulate Matter (2.5) Mass Concentration - Reference`,
         `Temperature - Raw`,
         `Relative Humidity - Raw`,
         `Site ID`) %>%
  dplyr::mutate(Date = with_tz(Date, tzone = tz), Error = `Particulate Matter (2.5) Mass Concentration - Calibrated` - `Particulate Matter (2.5) Mass Concentration - Reference`) %>%
  drop_na(`Particulate Matter (2.5) Mass Concentration - Reference`, `Particulate Matter (2.5) Mass Concentration - Calibrated`) %>%
  dplyr::group_by(`Site ID`, Month = clock::date_group(Date, "month")) %>%
  tidyr::nest() %>%
  dplyr::mutate(fit = map(data,
                          ~ lm(`Particulate Matter (2.5) Mass Concentration - Calibrated` ~ `Particulate Matter (2.5) Mass Concentration - Reference`,
                               data = .x)),
                n = map_dbl(data, nrow),
                glance = map(fit, broom::glance)) %>%
  tidyr::unnest(glance) %>%
  dplyr::rowwise() %>%
  dplyr::select(-fit) %>%
  dplyr::mutate(
    MAE = mean(abs(data$Error),na.rm = TRUE),
    RMSE = sqrt(mean((data$Error)^2, na.rm = TRUE)),
    `Mean Reference` = mean(data$`Particulate Matter (2.5) Mass Concentration - Reference`, na.rm = T),
    `Max Reference` = max(data$`Particulate Matter (2.5) Mass Concentration - Reference`, na.rm = T),
    `Completeness (%)` = (n / (lubridate::days_in_month(Month) * 24)) * 100,
    Complete = if_else(`Completeness (%)` >= 75, TRUE, FALSE)
  ) %>%
  dplyr::select(-data) %>%
  dplyr::ungroup()

temp_temporal <- temp_calibrated_hourly %>%
  group_by(Hour = hour(with_tz(Date, tzone = tz)), `Site ID`) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# max_Calibrated <- max(pull(temp_calibrated_hourly, clarity_calibrated), na.rm = TRUE)
max_reference_hourly <- max(pull(temp_calibrated_hourly, selected_independent), na.rm = TRUE)
if (max_reference_hourly < accuracy_range_cutpoint ) {
  accuracy_polygon <- data.frame(
    x = c(
      -5,
      -5,
      max_reference_hourly,
      max_reference_hourly),
    y = c(
      -accuracy_range_low,
      accuracy_range_low,
      max_reference_hourly + accuracy_range_low,
      max_reference_hourly - accuracy_range_low))
}  else {
  accuracy_polygon <- data.frame(
    x = c(-5,
          -5,
          accuracy_range_cutpoint,
          max_reference_hourly,
          max_reference_hourly,
          accuracy_range_cutpoint,
          accuracy_range_low),
    y = c(-accuracy_range_low,
          accuracy_range_low,
          accuracy_range_cutpoint + accuracy_range_low,
          max_reference_hourly * (1 + accuracy_range_high_percent/100),
          max_reference_hourly * (1 - accuracy_range_high_percent/100),
          accuracy_range_cutpoint - accuracy_range_low,
          -5))
}


ts_hourly <- ggplotly(dynamicTicks = TRUE,
         temp_calibrated_hourly  %>% drop_na(Date, Uncalibrated, Reference, Calibrated) %>%
           ggplot(data = .) +
           ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Hourly Averages)")) +
           geom_line(aes(x = Date, y = Uncalibrated, color = "Uncalibrated"), alpha = .9) +
           geom_line(aes(x = Date, y = Reference, color = "Reference"), alpha = .9) +
           geom_line(aes(x = Date, y = Calibrated, color = "Calibrated"), alpha = .9) +
           # ylab("Nitrogen Dioxide (ppb)") +
           scale_color_manual(values = c("Calibrated" = "#2897ff", "Uncalibrated" = "azure3", "Reference" = "black")) + 
           theme_bw() +
           labs(color = ""), width = 942, height = 549) %>%
  layout(legend = list(orientation = "h", x = 0.6, y = -0.075, bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "PM<sub>2.5</sub> (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "Date", titlefont = title_font))

saveWidget(ts_hourly, "ts_hourly_plot.HTML")

mean_by_hour_of_day <- ggplotly(
  ggplot(data = temp_temporal, aes(x = Hour)) +
    ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Hourly Averages)")) +
    geom_line(aes(y = Uncalibrated, color = "Uncalibrated"), alpha = .9) +
    geom_line(aes(y = Reference, color = "Reference"), alpha = .9) +
    geom_line(aes(y = Calibrated, color = "Calibrated"), alpha = .9) +
    # ylab("Mean Nitrogen Dioxide (ppb)") +
    scale_color_manual(values = c("Calibrated" = "#2897ff", "Uncalibrated" = "azure3", "Reference" = "black")) + 
    
    theme_bw() , width = 942, height = 549) %>%
  layout(legend = list(orientation = "h", x = 0.6, y = -0.075, bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "Mean PM<sub>2.5</sub> (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "Hour", titlefont = title_font))

saveWidget(mean_by_hour_of_day, "mean_by_hour_of_day_plot.HTML")

scatter_hourly <- ggplotly(dynamicTicks = TRUE,
         ggplot(temp_calibrated_hourly) +
           geom_point(aes(x = Reference, y = Uncalibrated, label = Date, color = "Uncalibrated"), alpha = .6) +
           geom_point(aes(x = Reference, y = Calibrated, label = Date, color = "Calibrated"), alpha = .6) +
           geom_segment(aes(x = 0, y = 0, xend = max(Reference, na.rm = T), yend = max(Reference, na.rm = T)), color = "darkblue", linetype = "dotted")  +
           geom_polygon(data = accuracy_polygon, aes(x = x, y = y), alpha = .1, color = "#2897ff", fill = "#2897ff") +
           geom_smooth(aes(x = Reference, y = Uncalibrated, color = "Uncalibrated"), method = 'lm', formula = y~x, na.rm = TRUE, se = FALSE) +
           geom_smooth(aes(x = Reference, y = Calibrated, color = "Calibrated"), method = 'lm', formula = y~x, na.rm = TRUE, se = FALSE, fill = "black") +
           ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Hourly Averages)")) +
           theme_bw() +
           scale_color_manual(values = c("Calibrated" = "#2897ff", "Uncalibrated" = "azure3")) +
           labs(color = ""), width = 942, height = 549)  %>%
  layout(legend = list(orientation = "h", x = 0.7, y = -0.075, bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "PM<sub>2.5</sub> - Clarity (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "PM<sub>2.5</sub> - Reference (µg/m<sup>3</sup>)", titlefont = title_font))

saveWidget(scatter_hourly, "scatter_hourly_plot.HTML")

error_overtime_hourly <- subplot(ggplotly(dynamicTicks = TRUE,  
                 temp_calibrated_hourly %>% 
                   ggplot(data = ., aes(x = Date)) +
                   geom_point(aes(y = `Calibrated Error`,color = Reference, label = Calibrated)) +
                   ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Hourly Averages)")) +
                   geom_ribbon(aes(ymax = `Target Accuracy`, ymin = -`Target Accuracy`), fill = "#2897ff", color = "#2897ff", alpha = .1) +
                   theme_bw() #+
                 # ylab("Calibrated Error (ppb)")
                 , width = 1050, height = 521
) %>%
  layout(legend = list(bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "Calibrated Error (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "Date", titlefont = title_font)),
ggplotly(dynamicTicks = TRUE,  
         temp_calibrated_hourly %>% 
           ggplot(data = ., aes(x = Date)) +
           geom_point(aes(y = `Uncalibrated Error`,color = Reference, label = Calibrated)) +
           ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Hourly Averages)")) +
           geom_ribbon(aes(ymax = `Target Accuracy`, ymin = -`Target Accuracy`), fill = "#2897ff", color = "#2897ff", alpha = .1) +
           theme_bw() #+
         # ylab("Uncalibrated Error (ppb)")
         , width = 1050, height = 521
) %>%
  layout(legend = list(bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "Uncalibrated Error (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "Date", titlefont = title_font))
, nrows = 2, titleY = T, shareX = T)

saveWidget(error_overtime_hourly, "error_overtime_hourly_plot.HTML")



error_by_hour_of_day <- ggplotly(
  ggplot(data = temp_temporal, aes(x = Hour)) +
    ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Hourly Averages)")) +
    geom_line(aes(y = `Uncalibrated Error`, color = "Uncalibrated"), alpha = .9) +
    geom_line(aes(y = `Calibrated Error`, color = "Calibrated"), alpha = .9) +
    # ylab("Mean Error (ppb)") +
    scale_color_manual(values = c("Calibrated" = "#2897ff", "Uncalibrated" = "azure3", "Reference" = "black")) + 
    theme_bw() , width = 942, height = 549) %>%
  layout(legend = list(orientation = "h", x = 0.6, y = -0.075, bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "Mean Error (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "Hour", titlefont = title_font))

saveWidget(error_by_hour_of_day, "error_by_hour_of_day_plot.HTML")


# Daily Average Plots -----------------------------------------------------


temp_calibrated_daily <- evaluation_combined_daily %>% 
  filter(`Site ID` == eval_collos$`Site ID`[i], Date > eval_collos$startTime[i], Date <= eval_collos$endTime[i]) %>% 
  ungroup() 

temp_stats_daily <- temp_calibrated_daily %>%
  select(Date, 
         `Particulate Matter (2.5) Mass Concentration - Calibrated`,
         `Particulate Matter (2.5) Mass Concentration - Raw`,
         `Particulate Matter (2.5) Mass Concentration - Reference`,
         `Temperature - Raw`,
         `Relative Humidity - Raw`,
         `Site ID`) %>%
  dplyr::mutate(Error = `Particulate Matter (2.5) Mass Concentration - Calibrated` - `Particulate Matter (2.5) Mass Concentration - Reference`) %>%
  drop_na(`Particulate Matter (2.5) Mass Concentration - Reference`, `Particulate Matter (2.5) Mass Concentration - Calibrated`) %>%
  dplyr::group_by(`Site ID`, Quarter = zoo::as.yearqtr(Date)) %>%
  tidyr::nest() %>%
  dplyr::mutate(fit = map(data,
                          ~ lm(`Particulate Matter (2.5) Mass Concentration - Calibrated` ~ `Particulate Matter (2.5) Mass Concentration - Reference`,
                               data = .x)),
                # n = map_dbl(data, nrow),
                glance = map(fit, broom::glance),
                tidy = map(fit, broom::tidy)) %>%
  tidyr::unnest(tidy) %>%
  pivot_wider(id_cols = c(`Site ID`, Quarter, glance, data), names_from = term, values_from = estimate) %>%
  rename(Slope = "`Particulate Matter (2.5) Mass Concentration - Reference`",
         Intercept = "(Intercept)") %>%
  tidyr::unnest(glance) %>%
  dplyr::rowwise() %>%
  # dplyr::select(-fit) %>%
  dplyr::mutate(
    MAE = mean(abs(data$Error),na.rm = TRUE),
    RMSE = sqrt(mean((data$Error)^2, na.rm = TRUE)),
    `Mean Reference` = mean(data$`Particulate Matter (2.5) Mass Concentration - Reference`, na.rm = T),
    `NRMSE (%)` = (sqrt(mean((data$Error)^2, na.rm = TRUE))/`Mean Reference`)*100,
    `Max Reference` = max(data$`Particulate Matter (2.5) Mass Concentration - Reference`, na.rm = T),
    # `Completeness (%)` = (nobs / (lubridate::days_in_month(Quarter))) * 100,
    ` Pair-Wise Complete Days` = nobs,
    days_in_month = (lubridate::days_in_month(Quarter))
  ) %>%
  dplyr::select(-data) %>%
  dplyr::ungroup() %>%
  select(Quarter, r.squared, RMSE, `NRMSE (%)`, `Mean Reference`, `Max Reference`, ` Pair-Wise Complete Days`, Slope, Intercept) %>%
  rename(`RMSE (µg/m<sup>3</sup>)` = RMSE,
         `R<sup>2</sup>` = r.squared,
         `Intercept (µg/m<sup>3</sup>)` = Intercept,
         `Mean Reference (µg/m<sup>3</sup>)` = `Mean Reference`,
         ` Max Reference (µg/m<sup>3</sup>)` = `Max Reference`) %>%
  pivot_longer(-c(Quarter), names_to = "Metric", values_to = "Value") %>%
  mutate(`Target High` = case_when(
    Metric == "RMSE (µg/m<sup>3</sup>)" ~ 7,
    Metric == "Slope" ~ 1.35,
    Metric == "Intercept (µg/m<sup>3</sup>)" ~ 5,
    Metric == "NRMSE (%)" ~ 30,
    Metric == "R<sup>2</sup>" ~ 1,
    Metric == " Max Reference (µg/m<sup>3</sup>)" ~ if_else(Value >= 25, Value, 25),
    Metric == " Pair-Wise Complete Days"  ~ if_else(Value >= 30, Value, 30)
  ),
  `Target Low` = case_when(
    Metric == "RMSE (µg/m<sup>3</sup>)" ~ 0,
    Metric == "Slope" ~ .65,
    Metric == "Intercept (µg/m<sup>3</sup>)" ~ -5,
    Metric == "NRMSE (%)" ~ 0,
    Metric == "R<sup>2</sup>" ~ .7,
    Metric == " Max Reference (µg/m<sup>3</sup>)" ~ 25,
    Metric == " Pair-Wise Complete Days" ~ 30
  ),
  `Individual Target Met` = case_when(Value >= `Target Low` & Value <= `Target High` ~ TRUE,
                                      Value < `Target Low` | Value > `Target High` ~ FALSE),
  Quarter = as.Date(Quarter)
  ) %>%
  pivot_wider(names_from = "Metric", values_from = c("Value", "Individual Target Met", "Target High", "Target Low")) %>%
  mutate(`Performance Targets Met` = case_when(`Individual Target Met_R<sup>2</sup>` &
                                                 `Individual Target Met_Slope` &
                                                 `Individual Target Met_Intercept (µg/m<sup>3</sup>)` &
                                                 (`Individual Target Met_RMSE (µg/m<sup>3</sup>)` |
                                                    `Individual Target Met_NRMSE (%)`)
                                               ~ T,
                                               T ~ F),
         `Period Qualifying Targets Met` = case_when(`Individual Target Met_ Max Reference (µg/m<sup>3</sup>)` &
                                                       `Individual Target Met_ Pair-Wise Complete Days`
                                                     ~ T,
                                                     T ~ F),
         `All Targets Met` = case_when(`Performance Targets Met` & `Period Qualifying Targets Met`
                                       ~ T,
                                       T ~ F)) %>%
  pivot_longer(-c(Quarter, `All Targets Met`,`Performance Targets Met`, `Period Qualifying Targets Met`), names_to = c(".value", "Metric"), names_sep = "_") %>%
  mutate(Metric = case_when(Metric == "Intercept (µg/m<sup>3</sup>)" ~ paste0(Metric," (Target: ", `Target Low`," \u2264 b \u2264 ", `Target High`,")"),
                            Metric == " Max Reference (µg/m<sup>3</sup>)" ~ paste0(Metric," (Period Qualifying Target: \u2265 ", `Target Low`,")"),
                            Metric == "NRMSE (%)" ~ paste0(Metric," (Target: \u2264 ", `Target High`,")"),
                            Metric == "RMSE (µg/m<sup>3</sup>)" ~ paste0(Metric," (Target: \u2264 ", `Target High`,")"),
                            Metric == " Pair-Wise Complete Days" ~ paste0(Metric," (Period Qualifying Target: \u2265 ", `Target Low`,")"),
                            Metric == "R<sup>2</sup>" ~ paste0(Metric," (Target Range: \u2265 ", `Target Low`,")"),
                            Metric == "Slope" ~ paste0(Metric," (Target: ", `Target Low`," \u2264 m \u2264 ", `Target High`,")"),
                            T ~ Metric)) 

qualifying_stats <- temp_stats_daily %>% filter(!(grepl("Mean Reference", Metric)), zoo::as.yearqtr(Quarter) != zoo::as.yearqtr(Sys.time()),
                                                grepl("Period Qualifying Target", Metric)) %>%
  mutate(Quarter = zoo::as.yearqtr(Quarter, format = "%Y-Q%q")) %>%
  group_by(Metric) %>% 
  mutate(`Target High` = if_else(max(`Target High`) > `Target Low`, max(`Target High`), `Target Low`),
         Value = round(Value, digits = 2))

performance_stats <- temp_stats_daily %>% filter(!(grepl("Mean Reference", Metric)), 
                                                 zoo::as.yearqtr(Quarter) != zoo::as.yearqtr(Sys.time()),
                                                 !grepl("Period Qualifying Target", Metric)) %>%
  mutate(Quarter = zoo::as.yearqtr(Quarter, format = "%Y-Q%q"),
         Value = round(Value, digits = 2))

overall_targets <- temp_stats_daily %>%
  select(Quarter, `Performance Targets Met`, `Period Qualifying Targets Met`, `All Targets Met`) %>%
  pivot_longer(-Quarter, names_to = "Target", values_to = "Met?") %>%
  filter(zoo::as.yearqtr(Quarter) != zoo::as.yearqtr(Sys.time())) %>%
  arrange(Target) %>%
  mutate(Quarter = zoo::as.yearqtr(Quarter, format = "%Y-Q%q"),
         Met_binary = if_else(`Met?` == TRUE, 1, 0),
         Target = as.factor(Target))

qualifying_plot <- ggplotly(
  qualifying_stats %>%
    ggplot(data = ., aes(x = Quarter, y = Value, color = `Individual Target Met`)) +
    ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i])) +
    geom_point() +
    ylab("") +
    xlab("") +
    geom_ribbon(aes(ymax = `Target High`, ymin = `Target Low`), fill = "#2897ff", color = "#2897ff", alpha = .1) +
    facet_wrap(~ Metric, ncol = 1, scales = "free_y", strip.position = "top") + 
    theme_bw() +
    scale_x_yearqtr(format = "%Y-Q%q", n = length(unique(qualifying_stats$Quarter))) +
    theme(legend.title = element_blank(), legend.position = "none")
  #scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
) %>%
  layout(legend = list(y = .9, yanchor = "top",bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         # margin = m,
         # annotations = legendtitle,
         xaxis = list(title = "Year - Quarter", titlefont = title_font)) 


performance_plot <- ggplotly(
  performance_stats %>%
    ggplot(data = ., aes(x = Quarter, y = Value, color = `Individual Target Met`)) +
    ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i])) +
    geom_point() +
    ylab("") +
    xlab("") +
    facet_wrap(~ Metric, ncol = 1, scales = "free_y", strip.position = "top") + 
    theme_bw() +
    geom_ribbon(aes(ymax = `Target High`, ymin = `Target Low`), fill = "#2897ff", color = "#2897ff", alpha = .1) +
    # scale_x_date(labels = function(x) zoo::format.yearqtr(x, "%Y-Q%q"), date_breaks = "3 months") +
    scale_x_yearqtr(format = "%Y-Q%q", n = length(unique(performance_stats$Quarter))) +
    theme(legend.title = element_blank(), legend.position = "none")
  #scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
) %>%
  layout(legend = list(y = .9, yanchor = "top",bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         # margin = m,
         #annotations = legendtitle,
         xaxis = list(title = "Year - Quarter", titlefont = title_font)) 

all_targets_plot <- ggplotly(
  overall_targets  %>%
    ggplot(data = ., aes(x = Quarter, y = Target, fill = `Met?`)) +
    ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i])) +
    geom_tile(color = "grey") +
    ylab("") +
    xlab("") +
    theme_bw() +
    scale_x_yearqtr(format = "%Y-Q%q", n = length(unique(performance_stats$Quarter))) +
    theme(legend.title = element_blank(), legend.position = "none"),
  tooltip = "text"
) %>%
  layout(#legend = list(y = .9, yanchor = "top",bgcolor = 'rgba(0,0,0,0)'),
    # legend.title = list(y = -.5),
    paper_bgcolor = 'rgba(0,0,0,0)',
    plot_bgcolor = 'rgba(0,0,0,0)',
    # margin = m,
    # annotations = legendtitle,
    xaxis = list(title = "Year - Quarter", titlefont = title_font)) 

epa_metric_performance <- subplot(all_targets_plot, qualifying_plot, performance_plot, nrows = 3, shareX = T, heights = c(1/8,2/8,5/8))

saveWidget(epa_metric_performance, "epa_metric_performance_plot.HTML")


# subplot(all_targets_plot, performance_plot, nrows = 2, shareX = T, heights = c(3/8,5/8))

max_reference_daily <- max(pull(temp_calibrated_daily, selected_independent), na.rm = TRUE)
if (max_reference_daily < accuracy_range_cutpoint ) {
  accuracy_polygon <- data.frame(
    x = c(
      -5,
      -5,
      max_reference_daily,
      max_reference_daily),
    y = c(
      -accuracy_range_low,
      accuracy_range_low,
      max_reference_daily + accuracy_range_low,
      max_reference_daily - accuracy_range_low))
}  else {
  accuracy_polygon <- data.frame(
    x = c(-5,
          -5,
          accuracy_range_cutpoint,
          max_reference_daily,
          max_reference_daily,
          accuracy_range_cutpoint,
          accuracy_range_low),
    y = c(-accuracy_range_low,
          accuracy_range_low,
          accuracy_range_cutpoint + accuracy_range_low,
          max_reference_daily * (1 + accuracy_range_high_percent/100),
          max_reference_daily * (1 - accuracy_range_high_percent/100),
          accuracy_range_cutpoint - accuracy_range_low,
          -5))
}



ts_daily <- ggplotly(dynamicTicks = TRUE,
         temp_calibrated_daily  %>% drop_na(Date, Uncalibrated, Reference, Calibrated) %>%
           ggplot(data = .) +
           ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Daily Averages)")) +
           geom_line(aes(x = Date, y = Uncalibrated, color = "Uncalibrated"), alpha = .9) +
           geom_line(aes(x = Date, y = Reference, color = "Reference"), alpha = .9) +
           geom_line(aes(x = Date, y = Calibrated, color = "Calibrated"), alpha = .9) +
           # ylab("Nitrogen Dioxide (ppb)") +
           scale_color_manual(values = c("Calibrated" = "#2897ff", "Uncalibrated" = "azure3", "Reference" = "black")) + 
           theme_bw() +
           labs(color = ""), width = 942, height = 549) %>%
  layout(legend = list(orientation = "h", x = 0.6, y = -0.075, bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "PM<sub>2.5</sub> (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "Date", titlefont = title_font))

saveWidget(ts_daily, "ts_daily_plot.HTML")

scatter_daily <- ggplotly(dynamicTicks = TRUE,
         ggplot(temp_calibrated_daily) +
           geom_point(aes(x = Reference, y = Uncalibrated, label = Date, color = "Uncalibrated"), alpha = .6) +
           geom_point(aes(x = Reference, y = Calibrated, label = Date, color = "Calibrated"), alpha = .6) +
           geom_segment(aes(x = 0, y = 0, xend = max(Reference, na.rm = T), yend = max(Reference, na.rm = T)), color = "darkblue", linetype = "dotted")  +
           geom_polygon(data = accuracy_polygon, aes(x = x, y = y), alpha = .1, color = "#2897ff", fill = "#2897ff") +
           geom_smooth(aes(x = Reference, y = Uncalibrated, color = "Uncalibrated"), method = 'lm', formula = y~x, na.rm = TRUE, se = FALSE) +
           geom_smooth(aes(x = Reference, y = Calibrated, color = "Calibrated"), method = 'lm', formula = y~x, na.rm = TRUE, se = FALSE, fill = "black") +
           ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Daily Averages)")) +
           theme_bw() +
           scale_color_manual(values = c("Calibrated" = "#2897ff", "Uncalibrated" = "azure3")) +
           ylab("Nitrogen Dioxide - Clarity (ppb)") +
           xlab("Nitrogen Dioxide - Reference (ppb)") +
           labs(color = ""), width = 942, height = 549)  %>%
  layout(legend = list(orientation = "h", x = 0.7, y = -0.075, bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "PM<sub>2.5</sub> - Clarity (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "PM<sub>2.5</sub> - Reference (µg/m<sup>3</sup>)", titlefont = title_font))

saveWidget(scatter_daily, "scatter_daily_plot.HTML")

error_overtime_daily <- subplot(ggplotly(dynamicTicks = TRUE,  
                 temp_calibrated_daily %>% 
                   ggplot(data = ., aes(x = Date)) +
                   geom_point(aes(y = `Calibrated Error`,color = Reference, label = Calibrated)) +
                   ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Daily Averages)")) +
                   geom_ribbon(aes(ymax = `Target Accuracy`, ymin = -`Target Accuracy`), fill = "#2897ff", color = "#2897ff", alpha = .1) +
                   theme_bw() #+
                 # ylab("Calibrated Error (ppb)")
                 , width = 1050, height = 521
) %>%
  layout(legend = list(bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "Calibrated Error (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "Date", titlefont = title_font)),
ggplotly(dynamicTicks = TRUE,  
         temp_calibrated_daily %>% 
           ggplot(data = ., aes(x = Date)) +
           geom_point(aes(y = `Uncalibrated Error`,color = Reference, label = Calibrated)) +
           ggtitle(paste(eval_collos$`Site ID`[i], "at", eval_collos$`Reference Site Name`[i], "(Daily Averages)")) +
           geom_ribbon(aes(ymax = `Target Accuracy`, ymin = -`Target Accuracy`), fill = "#2897ff", color = "#2897ff", alpha = .1) +
           theme_bw() #+
         # ylab("Uncalibrated Error (ppb)")
         , width = 1050, height = 521
) %>%
  layout(legend = list(bgcolor = 'rgba(0,0,0,0)'),
         paper_bgcolor = 'rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)',
         yaxis = list(title = "Uncalibrated Error (µg/m<sup>3</sup>)", titlefont = title_font),
         xaxis = list(title = "Date", titlefont = title_font))
, nrows = 2, titleY = T, shareX = T)

saveWidget(error_overtime_daily, "error_overtime_daily_plot.HTML")

# Map of Collocations -----------------------------------------------------

ref_meta_raw <- get_ref_meta(INTERNAL_API_KEY = 'RTlt300inj5bgEEEKFhMj7rKFQMbQ8OR9ydnDYDz'
, ref_ids = unique(cal_collos$`Reference Site ID`), params = "pm25")

ref_meta <- ref_meta_raw %>%
  mutate(dataOrigin = recode(dataOrigin, airnow = "AirNow",
                             eu_airquality = "EEA",
                             openaq = "OpenAQ",
                             preformatted_csv = ".csv"),
         # `Data Source` = paste(sourceName, "via", dataOrigin),
         # Longitude = sapply(.$location.coordinates, `[[`, 1),
         # Latitude = sapply(.$location.coordinates, `[[`, 2),
         Name = if_else(siteName == displayName, siteName, paste0(siteName, " (", displayName, ")"))) %>%
  dplyr::rename(`ID` = `_id`,
                `Source` = dataOrigin,
                `Agency` = sourceName
  ) %>%
  select(-siteName, -displayName, -city, -country, -insertedAt, -parameters, -status, -active, -ID)

site_info <- ref_meta[,c(5,2,1,3,4)]

map_data <- st_as_sf(site_info, coords = c("Latitude","Longitude"), crs = 4326)

ref_map <- leaflet() %>%
  # addTiles() %>%
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
  addGlPoints(data = map_data, fillColor = "#5199FC", popup = T, group = "Name") %>%
  setView(lat = 41.992124402793934, lng = -119.99759538214425, zoom = 5) #%>%
  # addLegend(
  #   labels = unique(map_data$Agency),
  #   colors = apply(unique(cols), 1, function(x) rgb(x[1], x[2], x[3])),
  #   title = "Agency",
  #   position = "bottomleft"
  # ) 

saveWidget(ref_map, "reference_site_map.HTML")
