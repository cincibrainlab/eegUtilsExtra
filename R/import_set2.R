#' Load `EEGLAB` .set files
#'
#' Load `EEGLAB` .set files and convert them to `eeg_epochs` objects. Supports
#' import of files saved in either Matlab v6.5 or Matlab v7.3 formats. Currently,
#' any ICA weights or decompositions are discarded.
#'
#' @param file_name Filename (and path if not in present working directory)
#' @param df_out Defaults to FALSE - outputs an object of class `eeg_epochs`. Set
#'   to TRUE for a normal data frame.
#' @param participant_id Character vector. By default, the filename will be used as the id of the
#'   participant.
#' @param recording Character vector. By default, the filename will be used as the name of the
#'   recording.
#' @param drop_custom Drop custom event fields. TRUE by default.
#' @param verbose Print informative messages. TRUE by default.
#' @author Matt Craddock \email{matt@@mattcraddock.com}
#' @importFrom dplyr group_by mutate rename
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr is_empty map_df
#' @examples
#' \dontrun{import_set("your_data.set")}
#' @return An object of class `eeg_epochs`
#' @export
import_set2 <- function(file_name,
                       df_out = FALSE,
                       participant_id = NULL,
                       recording = NULL,
                       drop_custom = FALSE,
                       verbose = TRUE) {

  checkpkgs <-
    unlist(
      lapply(c("R.matlab", "hdf5r"),
             requireNamespace,
             quietly = TRUE)
    )
  if (!all(checkpkgs)) {
    missing_pkg <- c("R.matlab", "hdf5r")[!checkpkgs]

    if (length(missing_pkg) == 1) {
      stop(paste("Package",
                 missing_pkg,
                 "needed. Please install it."),
           call. = FALSE)
    } else {
      stop(
        paste("Packages",
              missing_pkg[[1]],
              "&",
              missing_pkg[[2]],
              "needed. Please install them."
              ),
        call. = FALSE
        )
    }
  }

  if (verbose) {
    message("Importing from EEGLAB .set file.")
  }

  if (is.null(recording)) {
    recording <- basename(tools::file_path_sans_ext(file_name))
  }

  if (is.null(participant_id)) {
    participant_id <- basename(tools::file_path_sans_ext(file_name))
  }

  check_hdf5 <- hdf5r::is.h5file(file_name)

  if (check_hdf5) {
    if (verbose) {
      message("Matlab 7.3 file format detected.")
    }
    return(
      read_hdf5_set(file_name,
                    recording = recording,
                    participant_id = participant_id)
      )
  }


  temp_dat <- R.matlab::readMat(file_name)

  if (identical(names(temp_dat)[[1]], "EEG")) {
    temp_dat <- temp_dat$EEG[, 1, 1]
  }

  n_chans <- temp_dat[["nbchan"]]
  n_trials <- temp_dat[["trials"]]
  times <- temp_dat[["times"]]
  chan_info <- drop(Reduce(rbind,
                           temp_dat["chanlocs"]))
  #   var_names <- dimnames(temp_dat$EEG)[[1]]
  #
  # n_chans <- temp_dat$EEG[[which(var_names == "nbchan")]]
  # n_trials <- temp_dat$EEG[[which(var_names == "trials")]]
  # times <- temp_dat$EEG[[which(var_names == "times")]]
  # chan_info <- drop(Reduce(rbind, temp_dat$EEG["chanlocs",,]))

  pick_empties <-
    vapply(
      chan_info,
      function(x) is.null(x) | length(x) == 0,
      FUN.VALUE = logical(1)
    )
  chan_info[pick_empties] <- NA
  chan_info <- lapply(data.frame(t(chan_info)),
                      unlist) # unlist each data.frame column
  chan_info <- data.frame(chan_info) # turn back into data frame
  chan_info <- parse_chaninfo(chan_info)

  # check if the data is stored in the set or in a separate .fdt
  #if (is.character(temp_dat$EEG[[which(var_names == "data")]])) {
  if (is.character(temp_dat[["data"]])) {
    if (verbose) {
      message("Importing data from .fdt file.")
    }
    fdt_file <- paste0(tools::file_path_sans_ext(file_name),
                       ".fdt")
    fdt_file <- file(fdt_file, "rb")

    # read in data from .fdt
    # do this in chunks to avoid memory errors for large files...?
    signals <- readBin(fdt_file,
                       "double",
                       n = n_chans * n_trials * length(times),
                       size = 4,
                       endian = "little")
    close(fdt_file)

    dim(signals) <- c(n_chans, length(times) * max(n_trials, 1))
    times <- rep(times, max(n_trials, 1))

    if (n_trials == 1) {
      continuous <- TRUE
    } else {
      continuous <- FALSE
    }

  } else {

    # if the data is in the .set file, load it here instead of above
    #signals <- temp_dat$EEG[[which(dimnames(temp_dat$EEG)[[1]] == "data")]]
    signals <- temp_dat[["data"]]
    dim_signals <- dim(signals)

    if (length(dim_signals) == 3) {
      dim(signals) <- c(dim_signals[1], dim_signals[2] * dim_signals[3])
      times <- rep(times, n_trials)
      continuous <- FALSE
    } else {
      continuous <- TRUE
    }
  }

  signals <- t(signals)
  colnames(signals) <- unique(chan_info$electrode)
  signals <- as.data.frame(signals)
  signals$time <- as.numeric(times)

  #srate <- temp_dat$EEG[[which(var_names == "srate")]][[1]]
  srate <- temp_dat[["srate"]][[1]]

  if (!continuous) {
    signals <- dplyr::group_by(signals,
                               time)
    signals <- dplyr::mutate(signals,
                             epoch = 1:dplyr::n())
    signals <- dplyr::ungroup(signals)
  }

  #event_info <- temp_dat$EEG[[which(var_names == "event")]]
  event_info <- temp_dat[["event"]]

  event_table <- event_info
  dim(event_table) <- c(dim(event_info)[[1]],
                        dim(event_info)[[3]])

  event_table <- t(event_table)

  colnames(event_table) <- dimnames(event_info)[[1]]

  event_table <- tibble::as_tibble(event_table)
  event_table <-
    purrr::map_df(event_table,
                  ~unlist(
                    purrr::map(.,
                               ~ifelse(is.null(.),
                                       NA,
                                       .)))
                  )
  event_table <- lapply(event_table,
                        unlist)
  empty_entries <- unlist(lapply(event_table,
                                 rlang::is_empty))
  if (any(empty_entries)) {
    empty_cols <- names(event_table)[empty_entries]
    message(paste0("Removing empty event table column (s):",
                   empty_cols))
    event_table <- event_table[!empty_entries]
  }
  event_table <- tibble::as_tibble(event_table)

  # EEGLAB stores latencies in samples and allows non-integer samples (e.g.
  # through downsampling, or more rapidly sampled events than EEG signal)
  #
  if (any(event_table$latency %% 1 > 0)) {
    message("Rounding non-integer event sample latencies...")
    event_table$latency <- round(event_table$latency)
    # This can result in an event with a latency of zero in samples, which
    # causes problems with subsequent import steps - fix that and turn it into
    # sample 1
    event_table$latency <- ifelse(event_table$latency == 0,
                                  1,
                                  event_table$latency)
  }

  # EEGLAB stores latencies in samples starting from 1, my event_time is in
  # seconds, starting from 0

  event_table$event_time <- (event_table$latency - 1) / srate

  std_cols <- c("latency",
                "event_time",
                "type",
                "epoch")

  if (drop_custom & any(!colnames(event_table) %in% std_cols)) {
    message("Dropping custom columns...")
    event_table <- event_table[, std_cols]
  }

  col_check <-  colnames(event_table) %in% c("event_type", "event_onset")

  if (any(col_check)) {
    dupe_checks <- colnames(event_table)[col_check]
    dupe_labs <- paste0(dupe_checks, ".x")
  }

  #need to build in check for duplicate columns
  event_table <- dplyr::rename(event_table,
                               event_type = "type",
                               event_onset = "latency")
  if (df_out) {
    return(signals)
  } else {
    signals$time <- signals$time / 1000
    # convert to seconds - eeglab uses milliseconds
    if (continuous) {
      timings <- tibble::tibble(time = signals$time,                                #epoch = signal,
                                sample = 1:length(signals$time))
      n_epochs <- 1
    } else {
      timings <- tibble::tibble(time = signals$time,
                                epoch = signals$epoch,
                                sample = 1:length(signals$time))
      event_table$time <- NA
      event_table$time <- timings[which(timings$sample %in% event_table$event_onset,
                                        arr.ind = TRUE), ]$time

      n_epochs <- length(unique(timings$epoch))
    }



    epochs <-
      tibble::new_tibble(list(epoch = 1:n_epochs,
                              participant_id = rep(participant_id, n_epochs),
                              recording = rep(recording, n_epochs)),
                         nrow = n_epochs,
                         class = "epoch_info")
    if (continuous) {
      out_data <- eeg_data(signals[, 1:n_chans],
                           srate = srate,
                           timings = timings,
                           chan_info = chan_info,
                           events = event_table,
                           epochs = epochs)
    } else {
      out_data <- eeg_epochs(signals[, 1:n_chans],
                             srate = srate,
                             timings = timings,
                             chan_info = chan_info,
                             events = event_table,
                             reference = NULL,
                             epochs = epochs)
    }
    out_data
  }
}