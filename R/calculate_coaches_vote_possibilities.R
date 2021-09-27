#' Calculate Coaches Vote Possibilities
#'
#' @description
#' `calculate_coaches_vote_possibilities` returns all possible breakdowns of coaches votes between two coaches,
#' given a breakdown of AFLCA coaches votes
#'
#' @param df Requires the following column names: Player.Name, Coaches.Votes. These can be returned from the
#' function `fetch_coaches_votes`.
#' @param output_type One of "Coach View", "Player View". Defaults to "Coach View".
#'
#' @return
#' For output_type "Coach View" - A list of data frames with columns: Votes, C1, C2
#' For output_type "Player View" - A list of data frames with columns: Player, V1, V2
#' @export
#'
#' @examples
#' \dontrun{
#' # Return coaches votes for a particular match, then find the possibilities
#' df <- fetch_coaches_votes(comp = "AFLM", season = 2021, round = 24, team = "Western Bulldogs")
#' calculate_coaches_vote_possibilities(df, "Coach View")
#'
#' df <- fetch_coaches_votes(comp = "AFLW", season = 2021, round = 9, team = "Western Bulldogs")
#' calculate_coaches_vote_possibilities(df, "Player View")
#'
#' # Create a manual data frame to calculate possibilities
#' df <- data.frame(
#'   Player.Name = c(
#'     "Tom Liberatore", "Jack Macrae",
#'     "Marcus Bontempelli", "Cody Weightman",
#'     "Darcy Parish", "Aaron Naughton", "Jordan Ridley"
#'   ),
#'   Coaches.Votes = c(7, 6, 5, 5, 4, 2, 1)
#' )
#' calculate_coaches_vote_possibilities(df, "Player View")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
calculate_coaches_vote_possibilities <- function(df, output_type) {

  # error catching
  if (!output_type %in% c("Coach View", "Player View")) stop("Invalid Output Type")
  if (sum(names(df) %in% c("Player.Name", "Coaches.Votes")) != 2) stop("Input df has the wrong column names")
  if (length(unique(df$Player.Name)) < nrow(df)) stop("Duplicate Player Names")
  if (sum(df$Coaches.Votes %>% as.numeric()) != 30) stop("Coaches Vote total does not add up")
  if (nrow(df) < 5) stop("Not enough players")
  if (nrow(df) > 10) stop("Too many players")

  # start at the bottom for fewer options & reset row names
  df <- dplyr::arrange(df, .data$Coaches.Votes) %>% `rownames<-`(NULL)

  # template for votes
  template <- data.frame(Votes = c(5, 4, 3, 2, 1), C1 = NA, C2 = NA)

  # master votes list to add to
  master_votes <- list(template)

  # loop through each player and try differing combinations
  loop <- split(df, 1:nrow(df)) %>%
    lapply(function(row) {

      # assign variables
      cv <- row$Coaches.Votes %>% as.numeric()
      nm <- row$Player.Name
      min_cv <- max(0, cv - 5)
      max_cv <- min(cv, 5)

      # create list of options for this player
      options <- list()

      # loop through current eligible options
      for (opt in master_votes) {

        # loop through possible vote combinations for that player
        for (n in seq(min_cv, max_cv)) {

          # if that combination is blocked out for this eligible option, skip to the next option
          if (n == 0) {
            if (!is.na(dplyr::filter(opt, .data$Votes == cv - n)$C2)) next
          } else if (cv == n) {
            if (!is.na(dplyr::filter(opt, .data$Votes == n)$C1)) next
          } else if (!is.na(dplyr::filter(opt, .data$Votes == n)$C1) | !is.na(dplyr::filter(opt, .data$Votes == cv - n)$C2)) next

          # otherwise, update this option with this vote combination for this player
          opt_new <- opt
          opt_new[opt_new$Votes == n, "C1"] <- nm
          opt_new[opt_new$Votes == (cv - n), "C2"] <- nm
          options <- c(options, list(opt_new))
        }
      }

      # update the master options list
      assign("master_votes", options, envir = parent.frame(n = 2))

      # final list of combinations - only return if it's the last name on the list
      if (rownames(row) == nrow(df)) {
        return(options)
      }
    })

  # take the final item in the loop (last name on the list)
  final_outcomes <- loop[[nrow(df)]]

  # drop duplicate options (C1 = C2 and C2 = C1)
  # loop through outcomes
  if (length(final_outcomes) > 1) {
    for (i in 1:(length(final_outcomes) - 1)) {

      # skip if not possible
      if (i >= length(final_outcomes)) next

      # loop through pairing outcomes
      for (j in (i + 1):length(final_outcomes)) {

        # skip if not possible
        if (j > length(final_outcomes)) next

        # remove duplicate if it matches
        if (sum(final_outcomes[[i]]$C1 != final_outcomes[[j]]$C2) +
          sum(final_outcomes[[i]]$C2 != final_outcomes[[j]]$C1) == 0) {
          final_outcomes[[j]] <- NULL
        }
      }
    }
  }

  if (output_type == "Coach View") {
    return(final_outcomes)
  } else {
    # create player view from coach view
    player_view <- final_outcomes %>%
      # loop through each possibility
      lapply(function(i) {
        # source the player names
        c(i$C1, i$C2) %>%
          # drop duplicates
          unique() %>%
          # convert to list
          as.list() %>%
          # run function to...
          lapply(function(p) {
            # collect the votes that player received in this iteration
            player_votes <- c(
              dplyr::filter(i, .data$C1 == p)$Votes,
              dplyr::filter(i, .data$C2 == p)$Votes
            ) %>%
              sort()
            # control for zeroes
            if (length(player_votes) == 1) player_votes <- c(0, player_votes)
            # re-map this data into a new data frame
            data.frame(Player = p, V1 = player_votes[1], V2 = player_votes[2])
          }) %>%
          # combine all data
          {
            do.call(rbind, .)
          } %>%
          # arrange for consistency
          dplyr::arrange(-.data$V2, -.data$V1, .data$Player)
      })
    # drop duplicate options
    # loop through outcomes
    for (i in 1:(length(player_view) - 1)) {
      # skip if not possible
      if (i >= length(player_view)) next
      # loop through pairing outcomes
      for (j in (i + 1):length(player_view)) {
        # skip if not possible
        if (j > length(player_view)) next
        # remove duplicate if it matches
        if (identical(player_view[[i]], player_view[[j]])) player_view[[j]] <- NULL
      }
    }

    return(player_view)
  }
}

#' @rdname calculate_coaches_vote_possibilities
#' @export
