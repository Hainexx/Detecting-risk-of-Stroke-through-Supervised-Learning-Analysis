source("../utils/helpers.R")

factors_plot <- function(data=NULL, invert=FALSE, add_sums=TRUE, palette='Blues',
                          font_count_size=2.8, font_normalized_size=3.6, font_percentages_size=2.35,
                          font_categories_size=12) {
  
  # Check if data follow rules
  if (is.null(data))
    warning(paste0("Data cannot be NULL."))
  if (!is.data.frame(data))
    warning(paste0("Data MUST be a data.frame."))
  if (length(colnames(data)) != 3)
    warning(paste0("Data MUST have 3 columns."))
  if (!'n' %in% colnames(data))
    warning(paste0("Data MUST contain an 'n' column."))

  columns = colnames(data)[(colnames(data) != 'n')]
  if (!invert){
    row_family = columns[1]
    column_family = columns[2]
  }
  else{
    row_family = columns[2]
    column_family = columns[1]
  }
  
  # Define intensity measures (for colors (?))
  intensity_measures = list(
    "min" = min(data$n),
    "max" = max(data$n),
    "range" = max(data$n) - min(data$n)
  )

  # Declare necessary fonts
  font_counts <- update_font_setting(font(), defaults = list(
      "size" = font_count_size, "digits" = -1
    ), initial_vals = list(
        "nudge_y" = function(x) {x -0.2}
    )
  )
  
  font_row_percentages <- update_font_setting(font(), defaults = list(
      "size" = font_percentages_size, "prefix" = "",
      "suffix" = "%", "fontface" = "italic",
      "digits" = 1, "alpha" = 0.85
    ), initial_vals = list(
      "nudge_x" = function(x) {x + 0.41},
      "angle" = function(x) {x + 90}
    )
  )
  
  font_col_percentages <- update_font_setting(font(), defaults = list(
      "size" = font_percentages_size, "prefix" = "",
      "suffix" = "%", "fontface" = "italic",
      "digits" = 1, "alpha" = 0.85
    ), initial_vals = list(
      "nudge_y" = function(x) {x - 0.41}
    )
  )
  
  font_normalized <- update_font_setting(font(), defaults = list(
      "size" = font_normalized_size, "suffix" = "%", "digits" = 1
    ), initial_vals = list(
      "nudge_y" = function(x) {x}
    )
  )

  data[["N_text"]] <- preprocess_numeric(data[["n"]], font_counts)

  row_sums <- data %>%
    dplyr::group_by(.data[[row_family]]) %>%
    dplyr::summarize(row_n= sum(.data$n))

  column_sums <- data %>%
    dplyr::group_by(.data[[column_family]]) %>%
    dplyr::summarize(column_n = sum(.data$n))


  data <- data %>%
    dplyr::left_join(row_sums, by = row_family) %>%
    dplyr::mutate(
      row_percentage = 100 * (.data$n / .data$row_n),
      row_percentage_text = preprocess_numeric(
        .data$row_percentage, font_row_percentages
      )
    )

  data <- data %>%
  dplyr::left_join(column_sums, by = column_family) %>%
  dplyr::mutate(
    column_percentage = 100 * (.data$n / .data$column_n),
    column_percentage_text = preprocess_numeric(
      .data$column_percentage, font_col_percentages
    )
  )

  # Compute total percentages
  data[["total_percentage"]] <- 100 * (data[["n"]] / sum(data[["n"]]))
  
  data[["total_percentage_text"]] <- preprocess_numeric(
    data[["total_percentage"]],
    font_normalized,
    rm_zero_text = FALSE,
    rm_zeroes_post_rounding = FALSE # Only remove where N==0
  )

  arrow_icons <- list("up" = get_figure_path("caret_up_sharp.svg"),
                      "down" = get_figure_path("caret_down_sharp.svg"),
                      "left" = get_figure_path("caret_back_sharp.svg"),
                      "right" = get_figure_path("caret_forward_sharp.svg"))
  
  data <- set_arrows(data, place_x_axis_above = TRUE, icons = arrow_icons)
  
  text_geom_counts <- purrr::partial(
    ggplot2::geom_text,
    size = font_counts[["size"]],
    alpha = font_counts[["alpha"]],
    nudge_x = font_counts[["nudge_x"]],
    nudge_y = font_counts[["nudge_y"]],
    angle = font_counts[["angle"]],
    family = font_counts[["family"]],
    fontface = font_counts[["fontface"]],
    hjust = font_counts[["hjust"]],
    vjust = font_counts[["vjust"]],
    lineheight = font_counts[["lineheight"]]
  )
  
  text_geom_normalized <- purrr::partial(
    ggplot2::geom_text,
    size = font_normalized[["size"]],
    alpha = font_normalized[["alpha"]],
    nudge_x = font_normalized[["nudge_x"]],
    nudge_y = font_normalized[["nudge_y"]],
    angle = font_normalized[["angle"]],
    family = font_normalized[["family"]],
    fontface = font_normalized[["fontface"]],
    hjust = font_normalized[["hjust"]],
    vjust = font_normalized[["vjust"]],
    lineheight = font_normalized[["lineheight"]]
  )
  
  
  # TODO add sums
  # Add sums
  # if (isTRUE(add_sums)){
  #   
  #   # Create data frame with data for
  #   # the sum column, sum row, and total counts tile
  #   column_sums[[column_family]] <- "Total"
  #   row_sums[[row_family]] <-  "Total"
  #   column_sums <- dplyr::rename(column_sums, n = .data$column_n)
  #   row_sums <- dplyr::rename(row_sums, n = .data$row_n)
  #   column_sums <- 100 * (column_sums[["n"]] / sum(column_sums[["n"]]))
  #   row_sums <- 100 * (row_sums[["n"]] / sum(row_sums[["n"]]))
  #   total_count <- dplyr::tibble(
  #     row_family = "Total", column_family = "Total",
  #     "n" = sum(data$n), "percentage" = 100
  #   )
  #   sum_data <- dplyr::bind_rows(column_sums, row_sums, total_count)
  #   
  #   # Prepare text versions of the numerics
  #   sum_data[["total_text"]] <- preprocess_numeric(sum_data[["n"]], font_counts)
  #   sum_data[["percentage_total_text"]] <- preprocess_numeric(sum_data[["percentage"]], font_normalized)
  #   
  #   # Set total counts tile text
  #   sum_data[nrow(sum_data), "total_text"] <- ifelse(isTRUE(counts_on_top), sum_data[nrow(sum_data), "n"], "")
  #   sum_data[nrow(sum_data), "percentage_total_text"] <- ifelse(
  #     isTRUE(counts_on_top), "", paste0(sum_data[nrow(sum_data), "n"]))
  #   
  #   # Set color intensity metric
  #   sum_data[["Intensity"]] <- sum_data[["n"]]
  #   
  #   # Get min and max intensity scores and their range
  #   # We need to do this before adding sum_data
  #   sums_intensity_measures <- list(
  #     "min" = min(sum_data$n),
  #     "max" = max(sum_data$n),
  #     "range" = max(sum_data$n) - min(sum_data$n)
  #   )
  #   
  #   # Get color limits
  #   sums_color_limits <- get_color_limits(sums_intensity_measures, 0.9)
  #   
  #   sum_data[["image_skewed_lines"]] <- get_figure_path("empty_square.svg")
  #   
  #   # Set flag for whether a row is a total score
  #   sum_data[["is_sum"]] <- TRUE
  #   
  #   # Combine cm and sum_data
  #   data <-  dplyr::bind_rows(data, sum_data)
  #   
  #   # Set arrow icons to empty square image
  #   data[data[[row_family]] == "Total" | data[[column_family]] == "Total",] <- empty_tile_percentages(
  #     data[data[[row_family]] == "Total" | data[[column_family]] == "Total",])
  #   
  #   # Set class order and labels
  #   if (isTRUE(place_x_axis_above)){
  #     class_order <- c("Total", class_order)
  #   } else {
  #     class_order <- c(class_order, "Total")
  #   }
  #   class_labels <- class_order
  #   class_labels[class_labels == "Total"] <- sums_settings[["label"]]
  #   data[[row_family]] <- factor(data[[row_family]], levels = class_order, labels = class_labels)
  #   data[[column_family]] <- factor(data[[column_family]], levels = class_order, labels = class_labels)
  # }
  
  data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[row_family]],
      y = .data[[column_family]],
      fill = .data$n
    ))+
  ggplot2::geom_tile(
    colour = NA,
    size = 0.1,
    linetype = "solid",
    show.legend = FALSE
  ) +
  ggplot2::theme_minimal(base_size=font_categories_size) +
  ggplot2::coord_equal() +
  #Add fill colors that differ by N
  ggplot2::scale_fill_distiller(
    palette = palette,
    direction = 1,
    limits = get_color_limits(intensity_measures, 0.9)
  ) +
  ggplot2::theme(
    # Rotate y-axis text
    # axis.text.y = ggplot2::element_text(
    #   angle = 90,
    #   hjust = 0.5,
    #   vjust = 0.5
    # ),
    # Add margin to axis labels
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
    axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0))
  ) +
  # TODO add sums by row/columns
  # ggnewscale::new_scale_fill() +
  # ggplot2::geom_tile(
  #   data = cm[cm[["is_sum"]] & cm[["Target"]] != cm[["Prediction"]],],
  #   mapping = ggplot2::aes(fill = .data$Intensity),
  #   colour = sums_settings[["tile_border_color"]],
  #   size = sums_settings[["tile_border_size"]],
  #   linetype = sums_settings[["tile_border_linetype"]],
  #   show.legend = FALSE
  # ) +
  # ggplot2::scale_fill_distiller(
  #   palette = sums_settings[["palette"]],
  #   direction = 1,
  #   limits = sums_color_limits
  # ) +
  text_geom_counts(
    data = data[TRUE, ],
    ggplot2::aes(label = .data$N_text),
    color = font_counts[["color"]]
  ) +
  ggplot2::scale_x_discrete(
    position = "top",
    limits = rev(levels(data[[row_family]]))
  )+
    text_geom_normalized(
    data = data[TRUE, ],
    ggplot2::aes(label = .data$total_percentage_text),
    color = font_normalized[["color"]]
  )+
  ggplot2::geom_text(ggplot2::aes(label = .data$column_percentage_text),
                     size = font_row_percentages[["size"]],
                     color = font_row_percentages[["color"]],
                     alpha = font_row_percentages[["alpha"]],
                     nudge_x = font_row_percentages[["nudge_x"]],
                     nudge_y = font_row_percentages[["nudge_y"]],
                     angle = font_row_percentages[["angle"]],
                     family = font_row_percentages[["family"]],
                     fontface = font_row_percentages[["fontface"]],
                     hjust = font_row_percentages[["hjust"]],
                     vjust = font_row_percentages[["vjust"]],
                     lineheight = font_row_percentages[["lineheight"]]
  )+
  ggplot2::geom_text(ggplot2::aes(label = .data$row_percentage_text),
                     size = font_col_percentages[["size"]],
                     color = font_col_percentages[["color"]],
                     alpha = font_col_percentages[["alpha"]],
                     nudge_x = font_col_percentages[["nudge_x"]],
                     nudge_y = font_col_percentages[["nudge_y"]],
                     angle = font_col_percentages[["angle"]],
                     family = font_col_percentages[["family"]],
                     fontface = font_col_percentages[["fontface"]],
                     hjust = font_col_percentages[["hjust"]],
                     vjust = font_col_percentages[["vjust"]],
                     lineheight = font_col_percentages[["lineheight"]]
  )
  # TODO add small arrows to interpret percentages (by row/columns)
  #+
  # ggimage::geom_image(
  #   ggplot2::aes(image = .data$down_icon),
  #   by = "height",
  #   size = 0.048/ sqrt(nrow(data)),
  #   nudge_x = font_col_percentages[["nudge_x"]],
  #   nudge_y = font_col_percentages[["nudge_y"]] - 0.065
  # ) +
  # ggimage::geom_image(
  #   ggplot2::aes(image = .data$up_icon),
  #   by = "height",
  #   size = 0.048/ sqrt(nrow(data)),
  #   nudge_x = font_col_percentages[["nudge_x"]],
  #   nudge_y = font_col_percentages[["nudge_y"]] +
  #     0.065 - (0.048/ sqrt(nrow(data))/2)
  # )#+
  # ggimage::geom_image(
  #   ggplot2::aes(image = .data$right_icon),
  #   by = "height",
  #   size = 0.048/ sqrt(nrow(data_to_plot)),
  #   nudge_x = font_row_percentages[["nudge_x"]] +
  #     0.065 - (0.048/ sqrt(nrow(data_to_plot)) / 2),
  #   nudge_y = font_row_percentages[["nudge_y"]]
  # ) +
  # ggimage::geom_image(
  #   ggplot2::aes(image = .data$left_icon),
  #   by = "height",
  #   size = 0.048/ sqrt(nrow(data_to_plot)),
  #   nudge_x = font_row_percentages[["nudge_x"]] - 0.065,
  #   nudge_y = font_row_percentages[["nudge_y"]]
  # )
}
