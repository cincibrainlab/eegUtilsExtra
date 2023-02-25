#' Parse channel info from an EEGLAB set file
#'
#' Internal function to convert EEGLAB chan_info to eegUtils style
#'
#' @param chan_info Channel info list from an EEGLAB set file
#' @param drop If there are additional columns, remove all columns except
#'   electrode if TRUE, or just unexpected columns if FALSE.
#' @keywords internal
parse_chaninfo2 <- function(chan_info,
                           drop = FALSE) {

  # chan_info <- chan_info[sort(names(chan_info),
  #                             method = "shell")]
  expected <- c("labels", "radius",
                "ref", "sph.phi",
                "sph.radius", "sph.theta",
                "theta", "type",
                "urchan", "X",
                "Y", "Z")
  # Check for two things:
  # 1) Missing expected columns.
  # 2) Columns which are non-standard.
  if (!all(names(chan_info) %in% expected)) {
    if (drop) {
      warning("EEGLAB chan info has unexpected format, taking electrode names only.")
      out <- data.frame(chan_info["labels"])
      names(out) <- "electrode"
      return(validate_channels(out))
    } else {
      warning("EEGLAB chan info has unexpected format, taking only expected columns.")
      miss_cols <- expected[!(expected %in% names(chan_info))]
      #Why did I set this to NA? must have been a reason but it has unintended
      #consequences. Remember to build a test when I end up back here with an
      #example...
      chan_info[miss_cols] <- NA
    }
  }

  if (!all(expected %in% names(chan_info))) {
    miss_cols <- expected[!(expected %in% names(chan_info))]
    chan_info[miss_cols] <- NA

  }

  chan_info <- chan_info[expected]

  names(chan_info) <- c("electrode",
                        "radius",
                        "ref",
                        "sph_phi",
                        "sph_radius",
                        "sph_theta",
                        "angle",
                        "type",
                        "urchan",
                        "cart_x",
                        "cart_y",
                        "cart_z"
                        )

  chan_info <- chan_info[c("electrode",
                           "cart_x",
                           "cart_y",
                           "cart_z")]
  # EEGLAB co-ordinates are rotated 90 degrees compared to our coordinates,
  # and left-right flipped
  names(chan_info) <- names(chan_info)[c(1, 3, 2, 4)]
  chan_info <- chan_info[, c(1, 3, 2, 4)]
  chan_info$cart_x <- -chan_info$cart_x
  sph_coords <- cart_to_spherical(chan_info[, c("cart_x", "cart_y", "cart_z")])
  xy <- project_elecs(sph_coords)
  chan_info <- dplyr::bind_cols(electrode = as.character(chan_info$electrode),
                                sph_coords,
                                chan_info[, 2:4],
                                xy)
  chan_info
  }