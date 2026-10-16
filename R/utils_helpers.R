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

#' Calculates the outcome of the inspection based on the pre 01/01/2025 rules
#'
pre_2025_score <- function(resultat) {

  if_else(
    as.numeric(resultat) < 4,
    "green",
    if_else(as.numeric(resultat) > 26,
            "red",
            "orange"
            )
  )

}

#' Calculates the outcome of the inspection based on the after 01/01/2025 rules
#'
post_2025_score <- function(resultat) {

  if_else(
    as.numeric(resultat) < 4,
    "green",
    if_else(as.numeric(resultat) > 16,
            "red",
            "orange"
    )
  )

}

#' Checks that the motive of the inspection is consistent with the previous outcome
#'
check_sical_logic <- function(d) {

  d <- d |> arrange(on)

  if (nrow(d) < 2) {  # there is only one, so no need to do stuff

    tibble(prev_outcome = "just one",
           prev_motive = "just one",
           this_motive = "just one",
           issue = "just one",
           serial_number = 0,
           num_sical = "just one"
           )


  } else {

    lapply(2:nrow(d), function(i) {

      prev_outcome <- d$outcome[i-1]
      prev_motive <- d$motiu_de_la_inspeccio[i-1]

      this_motive <- d$motiu_de_la_inspeccio[i]

      ## Knowledge rules

      out <- NA

      if (is.na(prev_outcome)) {
        out <- "dont know"
      } else {

        if (this_motive == "Seguiment MC1") {  # ------

          if (prev_motive != "Seguiment requeriment") {
            out <- "Este es un MC1, pero el anterior no es un requeriment"
          } else {
            if (prev_outcome == "green") {
              out <- "Este es un MC1, pero el anterior daba verde"
            } else {
              out <- "OK"
            }
          }


        }

        if (this_motive == "Seguiment MC2") {  # ------

          if (prev_motive != "Seguiment MC1") {
            out <- "Este es un MC2, pero el anterior no es un MC1"
          } else {
            if (prev_outcome == "green") {
              out <- "Este es un MC2, pero el anterior daba verde"
            } else {
              out <- "OK"
            }
          }

        }

        if (this_motive == "Seguiment MC3") {  # ------

          if (prev_motive != "Seguiment MC2") {
            out <- "Este es un MC3, pero el anterior no es un MC2"
          } else {
            if (prev_outcome == "green") {
              out <- "Este es un MC3, pero el anterior daba verde"
            } else {
              out <- "OK"
            }
          }


        }

        if (this_motive == "Seguiment millora necessària") {  # ------

          if ( !(prev_motive %in% c("Inicial", "Sobrevinguda", "Per freqüència"))) {
            out <- "Este es una millora, pero el anterior no es inicio de ciclo"
          } else {
            if (prev_outcome != "orange") {
              out <- "Este es una millora, pero el anterior no daba naranja"
            } else {
              out <- "OK"
            }
          }

        }

        if (this_motive == "Seguiment requeriment") {  # ------

          if ( prev_motive == "Seguiment millora necessària") {

            if ( prev_outcome == "green" ) {
              out <- "Este es una requeriment tras millora, pero el requeriment salio verde"

            } else {
              out <- "OK"
            }

          } else if ( prev_motive %in% c("Inicial", "Sobrevinguda", "Per freqüència") ) {

            if (prev_outcome == "red") {
              out <- "OK"
            } else {
              out <- "Este es un requerimiento tras apertura, pero la apertura no dio roja"
            }

          } else {
            out <- "Parece que el motivo anterior no tiene sentido"
          }

        }

        if (this_motive == "Per freqüència") {  # ------

          if (prev_motive == "Visita prèvia informativa") {

            out <- "OK"

          } else {

            if (prev_outcome != "green") {
              out <- "Esta es por frecuencia, pero el anterior daba naranja o rojo"
            } else {
              out <- "OK"
            }

          }



        }

        if (this_motive == "Inicial") {  # ------
          if (prev_motive == "Visita prèvia informativa") {
            out <- "OK"
          } else {
            out <- "Inicial pero no es el primero de todos"
          }

        }

        if (this_motive == "Sobrevinguda") {  # ------

          if (prev_motive == "Visita prèvia informativa") {
            out <- "OK"
          } else {
            if (prev_outcome != "green") {
              out <- "Es sobrevinguda, pero el anterior daba naranja o rojo"
            } else {
              out <- "OK"
            }
          }

        }

      }

      tibble(prev_outcome = prev_outcome,
             prev_motive = prev_motive,
             this_motive = this_motive,
             issue = out)

      # out

    }) |> bind_rows() |>
      mutate(serial_number = d$serial_number[-1],
             num_sical = d$num_sical[-1])
  }

}














