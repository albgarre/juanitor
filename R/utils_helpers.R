#' check_mismatch
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @noRd
check_mismatch <- function(this_data, col1, col2) {

  # browser()

  ## Entries with no data

  none <- this_data %>%
    filter(is.na(.data[[col1]]),
           is.na(.data[[col2]])
    )

  ## Entries with both columns and conflicting

  conflict <- this_data %>%
    filter(!is.na(.data[[col1]]),
           !is.na(.data[[col2]])
    ) %>%
    filter(.data[[col1]] != .data[[col2]])

  ## Return

  list(none = none,
       conflict = conflict)

}


#' AA
merge_columns <- function(this_data, col1, col2,
                          mismatch = "error",
                          remove_col = TRUE) {

  ## Check for mistmatches

  if (mismatch == "error") {

    if ( nrow(check_mismatch(this_data, col1, col2)$conflict) > 0 ) stop("There are mistmatches")

  } else if (mismatch == "warning") {

    if ( nrow(check_mismatch(this_data, col1, col2)$conflict) > 0 ) warning("There are mistmatches. The second column will overwrite the first one.")

  }

  ## Merge columns

  merged_col <- if_else(is.na(this_data[[col2]]),
                        this_data[[col1]],
                        this_data[[col2]]
  )

  this_data[[col1]] <- merged_col

  ## Remove if needed

  if (remove_col) {
    this_data <- this_data %>% select(-matches(col2))
  }

  ## Return

  this_data

}

#' Moves the header of the strings in address
#'
fix_address_start <- function(this_d, start = "^C[\\/\\. ]", end = "/C") {

  this_d %>%
    mutate(flag_C = grepl(start, adreca_establiment)) %>%  # identify that start with the regex
    mutate(adreca_establiment = if_else(flag_C,  # Remove the header
                                        gsub(start, "", adreca_establiment),
                                        adreca_establiment
    )
    ) %>%
    mutate(adreca_establiment = if_else(flag_C,  # Include the ending
                                        paste0(adreca_establiment, end),
                                        adreca_establiment
                                        )
    ) %>%
    mutate(adreca_establiment = str_trim(adreca_establiment)) %>%  # trim
    select(-flag_C)  # remove the flag and return

}





