update_settings_object <- function(settings, defaults, backup_defaults, initial_vals){
  new_settings <- list()
  for (opt in names(backup_defaults)) {
    if (is.null(settings[[opt]])) {
      if (opt %in% names(defaults) &&
          !is.null(defaults[[opt]])) {
        new_settings[[opt]] <- defaults[[opt]]
      } else {
        new_settings[[opt]] <- backup_defaults[[opt]]
      }
    } else {
      new_settings[[opt]] <- settings[[opt]]
    }
    
    # Apply initial values
    if (!is.null(initial_vals) && opt %in% names(initial_vals)) {
      new_settings[[opt]] <- initial_vals[[opt]](new_settings[[opt]])
    }
  }
  
  new_settings
}

update_font_setting <- function(settings, defaults, initial_vals = NULL) {
  
  # If defaults not provided,
  # here are some reasonable backup defaults
  backup_defaults <-
    font(
      size = 4,
      color = "black",
      alpha = 1.0,
      nudge_x = 0,
      nudge_y = 0,
      angle = 0,
      family = "",
      fontface = "plain",
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 1.2,
      digits = -1,
      prefix = "",
      suffix = ""
    )
  
  update_settings_object(
    settings = settings,
    defaults = defaults,
    backup_defaults = backup_defaults,
    initial_vals = initial_vals
  )
}

font <- function(size = NULL,
                 color = NULL,
                 alpha = NULL,
                 nudge_x = NULL,
                 nudge_y = NULL,
                 angle = NULL,
                 family = NULL,
                 fontface = NULL,
                 hjust = NULL,
                 vjust = NULL,
                 lineheight = NULL,
                 digits = NULL,
                 prefix = NULL,
                 suffix = NULL) {

  list(
    "size" = size,
    "color" = color,
    "alpha" = alpha,
    "nudge_x" = nudge_x,
    "nudge_y" = nudge_y,
    "angle" = angle,
    "family" = family,
    "fontface" = fontface,
    "hjust" = hjust,
    "vjust" = vjust,
    "lineheight" = lineheight,
    "digits" = digits,
    "prefix" = prefix,
    "suffix" = suffix
  )
}

preprocess_numeric <- function(vec, settings, rm_zero_text=FALSE, rm_zeroes_post_rounding=TRUE) {
  
  # Find the pre-rounding 0s
  is_zero <- vec == 0
  
  # Don't round if digits is negative
  if (settings[["digits"]] >= 0) {
    vec <- round(vec, settings[["digits"]])
  }
  out <- paste0(settings[["prefix"]], vec, settings[["suffix"]])
  
  # Remove text for zeroes
  # Potentially including elements zero after rounding
  if (isTRUE(rm_zero_text)){
    if (isTRUE(rm_zeroes_post_rounding)){
      out[vec == 0] <- ""
    } else {
      out[is_zero] <- ""
    }
  }
  out
}


get_figure_path <- function(fig_name, inst_dir = "images", pgk_name = "cvms") {
  dir_path <- system.file(inst_dir, package = pgk_name)
  fig_path <- paste0(dir_path, "/", fig_name)
  if (file.exists(fig_path))
    return(fig_path)
  warning("Could not find figure.")
  invisible()
}

# TODO complete w/ removing arrows from extremes
set_arrows <- function(cm, place_x_axis_above, icons,
                       empty_path = get_figure_path("empty_square.svg")){
  
  # Get the extreme levels
  # max_prediction_level <- max(as.character(levels(cm[["stroke"]])))
  # min_prediction_level <- min(as.character(levels(cm[["stroke"]])))
  # max_target_level <- max(as.character(levels(cm[["work_type"]])))
  # min_target_level <- min(as.character(levels(cm[["work_type"]])))
  
  # Set arrow icon names for all tiles
  cm[["right_icon"]] <- icons[["right"]]
  cm[["left_icon"]] <- icons[["left"]]
  cm[["up_icon"]] <- icons[["up"]]
  cm[["down_icon"]] <- icons[["down"]]
  
  # Remove arrows where Prediction is extreme level
  # cm[cm[["Prediction"]] == max_prediction_level, "up_icon"] <- empty_path
  # cm[cm[["Prediction"]] == min_prediction_level, "down_icon"] <- empty_path
  
  # Remove arrows where Target is extreme level
  # if (isTRUE(place_x_axis_above)){
  #   cm[cm[["work_type"]] == max_target_level, "left_icon"] <- empty_path
  #   cm[cm[["work_type"]] == min_target_level, "right_icon"] <- empty_path
  # } else {
  #   cm[cm[["work_type"]] == max_target_level, "right_icon"] <- empty_path
  #   cm[cm[["work_type"]] == min_target_level, "left_icon"] <- empty_path
  # }
  
  cm
}

get_color_limits <- function(intensity_measures, darkness){
  # To avoid the extremely dark colors
  # where the black font does not work that well
  # We add a bit to the range, so our max intensity
  # will not appear to be the extreme
  c(intensity_measures[["min"]],
    intensity_measures[["max"]] + 10 * (1 - darkness) * (
      intensity_measures[["range"]] / 5)
  )
}