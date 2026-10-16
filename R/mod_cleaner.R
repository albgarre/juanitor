## TODO: visit previa informativa es inicio válido


#' cleaner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_column_wrap card card_header card_footer navset_card_tab
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom readxl read_excel
#' @importFrom lubridate year
#'
mod_cleaner_ui <- function(id) {
  ns <- NS(id)
  tagList(

    layout_column_wrap(  # Data loading
      width = 1/2,
      card(
        fileInput(ns("fileDef"), "Consulta actas"),
        DTOutput(ns("summary_data"))
      )
    ),

    layout_column_wrap(  # Missing metadata
      width = 1,
      navset_card_tab(
        full_screen = TRUE,
        title = "Info faltante",
        nav_panel(
          title = "Report",
          DTOutput(ns("cols_noinfo")),
          card_footer(
            downloadButton(ns("down_noInfo"), "Download")
          )
        ),
        nav_panel(
          title = "Correccion",
          fileInput(ns("file_noInfo"), "Report info faltante"),
          tableOutput(ns("summary_noInfo")),
          h3("Filas que faltarian:"),
          tableOutput(ns("warnings_noInfo")),
          actionButton(ns("apply_noInfo"), "Aplicar")
        ),
        nav_panel(
          title = "Resultado",
          downloadButton(ns("down_outputInfo"), "Download"),
          DTOutput(ns("outcome_noInfo"))
        ),
        nav_panel(
          title = "Saltarse el paso",
          actionButton(ns("skip_noInfo"), "No hace falta")
        )
      )

    ),
    layout_column_wrap(  # Conflicts between columns
      width = 1,
      navset_card_tab(
        full_screen = TRUE,
        title = "Conflictos metadata",
        nav_panel(
          title = "Report",
          DTOutput(ns("cols_conflict")),
          card_footer(
            downloadButton(ns("down_conflict"), "Download")
          )
        ),
        nav_panel(
          title = "Correccion",
          fileInput(ns("file_conflicts"), "Report conflictos"),
          tableOutput(ns("summary_conflicts")),
          # h3("Filas que faltarian:"),
          # tableOutput(ns("warnings_noInfo")),
          actionButton(ns("apply_conflict"), "Aplicar")
        ),
        nav_panel(
          title = "Resultado",
          downloadButton(ns("down_outputConflicts"), "Download"),
          DTOutput(ns("outcome_conflicts"))
        ),
        nav_panel(
          title = "Saltarse el paso",
          actionButton(ns("skip_conflicts"), "No hace falta")
        )
      )
    ),
    layout_column_wrap(  # Num de acta

      width = 1/2,
      card(
        card_header("Numero de acta"),
        selectInput(ns("type_of_acta"),
                    "Tipo de acta",
                    choices = list(`Restauracion comercial (REC)` = "REC",
                                   `Minoristas (MIN)` = "MIN",
                                   `Restauracion social (RES)` = "RES"
                    )
        ),
        numericInput(ns("n_digits"), "Numero de digitos", 5),
        actionButton(ns("apply_n_d_acta"), "Aplicar")
      )
    ),

    layout_column_wrap(  # Same place same time
      width = 1,
      navset_card_tab(
        full_screen = TRUE,
        title = "Coincidentes en el tiempo",
        nav_panel(
          title = "Report",
          DTOutput(ns("report_paradox")),
          card_footer(
            downloadButton(ns("down_paradox"), "Download")
          )
        ),
        nav_panel(
          title = "Correccion",
          fileInput(ns("file_paradoxes"), "Report paradoxes"),
          tableOutput(ns("summary_paradoxes")),
          actionButton(ns("apply_paradoxes"), "Aplicar")
        ),
        nav_panel(
          title = "Saltarse el paso",
          actionButton(ns("skip_paradox"), "No hace falta")
        ),
        nav_panel(
          "Resultado"
        )
      )
    ),
    layout_column_wrap(  # Same SICAL with different people
      width = 1,
      navset_card_tab(
        full_screen = TRUE,
        title = "Mismo SICAL - diferente persona",
        nav_panel(
          title = "Report",
          DTOutput(ns("report_multisical")),
          card_footer(
            downloadButton(ns("down_multisical"), "Download")
          )
        ),
        nav_panel(
          title = "Correccion",
          fileInput(ns("file_multisical"), "Report paradoxes"),
          tableOutput(ns("summary_multisical")),
          actionButton(ns("apply_multisical"), "Aplicar")
        ),
        nav_panel(
          title = "Saltarse el paso",
          actionButton(ns("skip_multisical"), "No hace falta")
        ),
        nav_panel(
          "Resultado"
        )
      )
    ),

    layout_column_wrap(  # Check of the right start for the cycles
      width = 1,
      navset_card_tab(
        full_screen = TRUE,
        title = "Ciclos - check inicio",
        nav_panel(
          title = "Report",
          DTOutput(ns("report_ciclos")),
          card_footer(
            downloadButton(ns("down_ciclos"), "Download")
          )
        ),
        nav_panel(
          title = "Correccion",
          fileInput(ns("file_ciclos"), "Report paradoxes"),
          tableOutput(ns("summary_ciclos")),
          actionButton(ns("apply_ciclos"), "Aplicar")
        ),
        nav_panel(
          title = "Saltarse el paso",
          actionButton(ns("skip_ciclos"), "No hace falta")
        ),
        nav_panel(
          "Resultado"
        )
      )
    ),

    layout_column_wrap(  # Check for the consistency of the cycles
      width = 1,
      navset_card_tab(
        full_screen = TRUE,
        title = "Ciclos - check consistencia",
        nav_panel(
          title = "Report",
          DTOutput(ns("report_cyclelogic")),
          card_footer(
            downloadButton(ns("down_cyclelogic"), "Download")
          )
        ),
        nav_panel(
          title = "Correccion",
          fileInput(ns("file_cyclelogic"), "Report paradoxes"),
          tableOutput(ns("summary_cyclelogic")),
          actionButton(ns("apply_cyclelogic"), "Aplicar")
        ),
        nav_panel(
          title = "Saltarse el paso",
          actionButton(ns("skip_cyclelogic"), "No hace falta")
        ),
        nav_panel(
          "Resultado"
        )
      )
    ),

    layout_column_wrap(  # Download the outcome
      width = 1,
      card(
        card_header("Descarga"),
        DTOutput(ns("final_data")),
        card_footer(
          downloadButton(ns("down_final"), "Download")
        )
      )
    )

  )
}

#' cleaner Server Functions
#'
#' @importFrom janitor clean_names
#' @importFrom purrr %>% map imap imap_dfr
#' @importFrom dplyr select mutate arrange summarize n pull if_else bind_rows left_join if_else
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble as_tibble
#' @importFrom lubridate interval int_start int_end
#' @importFrom writexl write_xlsx
#' @importFrom stringr str_to_upper str_trim
#' @importFrom rlang set_names
#' @noRd
mod_cleaner_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## Importing data

    def_frame <- reactive({

      validate(need(input$fileDef, message = "Sube la tabla Excel"))

      ## Read the data

      d <- read_excel(input$fileDef$datapath,
                 sheet = "Submissions",
                 col_types = "text") %>%
        clean_names()

      ## Read it again because I need to make the On column a date

      d_puto_on <- read_excel(input$fileDef$datapath,
                              sheet = "Submissions")

      d$on <- d_puto_on$On

      ## return

      d

    })

    output$summary_data <- renderDT({  # Feedbak for data import
      d <- def_frame()

      print(paste("Number of columns:", ncol(d)))

      my_int <- interval(min(d$on), max(d$on))

      tibble(
        `Primera acta` = int_start(my_int),
        `Ultima acta` = int_end(my_int),
        `Numero actas` = nrow(d),
        `Numero SICALs` = length(unique(d$num_sical))
      ) %>%
        datatable(filter = "none",
                  selection = "none"
                  )

    })

    ## Identifying missing meta-data

    output$cols_noinfo <- renderDT({

      validate(need(def_frame(), message = "Sube la tabla antes"))

      d <- def_frame()

      all_mis <- list(
        numero = check_mismatch(d, "establiment_inspeccionat_numero", "numero"),
        adreca_establiment = check_mismatch(d, "establiment_inspeccionat_carrer", "adreca_establiment"),
        # altres_pis = check_mismatch(d, "establiment_inspeccionat_altres_pis", "altres_pis_local"),
        codi_postal = check_mismatch(d, "establiment_inspeccionat_cp", "codi_postal"),
        num_sical = check_mismatch(d, "establiment_inspeccionat_sical", "num_sical"),
        titular_rao_social = check_mismatch(d, "establiment_inspeccionat_titular", "titular_rao_social"),
        nif_dni = check_mismatch(d, "establiment_inspeccionat_nif", "nif_dni"),
        nom_comercial_de_lestabliment = check_mismatch(d, "establiment_inspeccionat_nomestabliment", "nom_comercial_de_lestabliment")
      )

      out <- all_mis %>%
        map(., ~ .$none) %>%
        imap_dfr(., ~ mutate(.x, conflict = .y)) %>%
        select(serial_number, conflict) %>%
        mutate(aa = "X") %>%
        pivot_wider(names_from = conflict, values_from = aa) %>%
        arrange(serial_number)

      ## Add columns if missing

      for (this_name in c("numero", "adreca_establiment", "codi_postal",
                          "num_sical", "titular_rao_social", "nif_dni",
                          "nom_comercial_de_lestabliment") ) {

        if ( !( this_name %in% names(out) ) ) {
          out[[this_name]] <- NA
        }

      }

      out <- out %>%
        select("serial_number", "numero", "adreca_establiment", "codi_postal",
               "num_sical", "titular_rao_social", "nif_dni",
               "nom_comercial_de_lestabliment")

      ## Return

      out %>%
        datatable()

    })

    ## Downloading missing metadata report

    output$down_noInfo <- downloadHandler(
      filename = "noInfo.xlsx",
      content = function(file) {

        validate(need(def_frame(), message = ""))

        ## Check mismatches

        d <- def_frame()

        all_mis <- list(
          numero = check_mismatch(d, "establiment_inspeccionat_numero", "numero"),
          adreca_establiment = check_mismatch(d, "establiment_inspeccionat_carrer", "adreca_establiment"),
          # altres_pis = check_mismatch(d, "establiment_inspeccionat_altres_pis", "altres_pis_local"),
          codi_postal = check_mismatch(d, "establiment_inspeccionat_cp", "codi_postal"),
          num_sical = check_mismatch(d, "establiment_inspeccionat_sical", "num_sical"),
          titular_rao_social = check_mismatch(d, "establiment_inspeccionat_titular", "titular_rao_social"),
          nif_dni = check_mismatch(d, "establiment_inspeccionat_nif", "nif_dni"),
          nom_comercial_de_lestabliment = check_mismatch(d, "establiment_inspeccionat_nomestabliment", "nom_comercial_de_lestabliment")
        )

        ## Build the table to download

        out <- all_mis %>%
          map(., ~ .$none) %>%
          imap_dfr(., ~ mutate(.x, conflict = .y)) %>%
          select(serial_number, conflict) %>%
          mutate(aa = "X") %>%
          pivot_wider(names_from = conflict, values_from = aa) %>%
          arrange(serial_number)

        ## Add columns if missing

        for (this_name in c("numero", "adreca_establiment", "codi_postal",
                            "num_sical", "titular_rao_social", "nif_dni",
                            "nom_comercial_de_lestabliment") ) {

          if ( !( this_name %in% names(out) ) ) {
            out[[this_name]] <- NA
          }

        }

        out <- out %>%
          select("serial_number", "numero", "adreca_establiment", "codi_postal",
                       "num_sical", "titular_rao_social", "nif_dni",
                       "nom_comercial_de_lestabliment")

        ## Build the table for the corrections

        out2 <- matrix(data = "", nrow = 1, ncol = ncol(out)) %>% as_tibble()
        names(out2) <- names(out)

        ## Add the columns for decisions

        out <- out %>%
          mutate(decision = NA, observaciones = NA)

        ## Export

        write_xlsx(
          list(
            report = out, cambios = out2
            ),
          path = file
          )

      }
    )

    ## Uploading fixing metadata report

    noInfo_list <- reactive({

      validate(need(input$file_noInfo, message = "Sube el archivo de cambios"))

      list(report = read_excel(input$file_noInfo$datapath, sheet = "report", col_types = "text"),
           cambios = read_excel(input$file_noInfo$datapath, sheet = "cambios", col_types = "text")
           )

    })

    ## IDs marked as "to be changed" that are not changed

    output$warnings_noInfo <- renderTable({

      d <- noInfo_list()

      ids <- d$report %>% filter(decision == "CORREGIR") %>% pull(serial_number)

      tibble(serial_number = ids[ !(ids %in% d$cambios$serial_number) ])

    })

    ## Reactive after the filtering based on those without info

    d_noInfo <- reactiveVal()

    ## Skipping the missing info step

    observeEvent(input$skip_noInfo, {

      validate(need(def_frame(), message = "Sube la tabla antes"))

      d <- def_frame()

      d_noInfo(d)

    })

    ## Modals for messages

    modalMissingChanges <- function(failed = FALSE) {
      modalDialog(
        tagList(
          h3("Hay algunos numeros de serie marcados como CORREGIR pero no definidos"),
          "Por favor, define los cambios en la pestaña del Excel."
        ),
        footer = tagList(
          modalButton("Cancel")
        )
      )
    }

    ## Apply the filtering defined by the user

    observeEvent(input$apply_noInfo, {

      validate(need(noInfo_list(), message = ""))
      validate(need(def_frame(), message = ""))

      d <- def_frame()
      report <- noInfo_list()

      ## Check there are no missing ones

      ids <- report$report %>% filter(decision == "CORREGIR") %>% pull(serial_number)

      missing <- tibble(serial_number = ids[ !(ids %in% report$cambios$serial_number) ])

      if ( nrow(missing) > 0 ) {

        showModal(noInfo())
      }

      ## Remove the ELIMINAR

      ids_out <- report$report %>%
        filter(decision == "ELIMINAR") %>%
        pull(serial_number)

      d <- d %>%
        filter( !(serial_number %in% ids_out) )

      ## Modify the CORREGIR

      ids_change <- report$cambios %>%
        pull(serial_number)

      d_changed <- d %>%
        filter(serial_number %in% ids_change) %>%
        arrange(serial_number)

      d <- d %>% filter( !(serial_number %in% ids_change) )

      changes <- report$cambios %>%
        arrange(serial_number)

      d_changed <- d_changed %>%
        mutate(numero = if_else(is.na(changes$numero),
                                as.character(numero),
                                as.character(changes$numero)
                                ),
               adreca_establiment = if_else(is.na(changes$adreca_establiment),
                                            as.character(adreca_establiment),
                                            as.character(changes$adreca_establiment)
               ),
               codi_postal = if_else(is.na(changes$codi_postal),
                                     as.character(codi_postal),
                                     as.character(changes$codi_postal)
               ),
               titular_rao_social = if_else(is.na(changes$titular_rao_social),
                                 as.character(titular_rao_social),
                                 as.character(changes$titular_rao_social)
               ),
               nif_dni = if_else(is.na(changes$nif_dni),
                                 as.character(nif_dni),
                                 as.character(changes$nif_dni)
               ),
               nom_comercial_de_lestabliment = if_else(is.na(changes$nom_comercial_de_lestabliment),
                                         as.character(nom_comercial_de_lestabliment),
                                         as.character(changes$nom_comercial_de_lestabliment)
                                         )
               )

      # browser()

      d <- bind_rows(d, d_changed) %>%
        mutate(serial_number = as.numeric(serial_number))

      ## Update the reactive val

      d_noInfo(d)

    })

    ## Summary of the filtering

    output$summary_noInfo <- renderTable({  # Feedbak for data import

      validate(need(noInfo_list(), message = "Sube el archivo de correcciones"))

      # browser()

      d <- noInfo_list()

      d$report %>%
        summarize(n = n(), .by = "decision")

    })

    ## Download the outcome of the filter

    output$down_outputInfo <- downloadHandler(
      filename = "Actas_noInfo.xlsx",
      content = function(file) {

        validate(need(d_noInfo(), message = "Aplica el filtrado antes"))

        d <- d_noInfo()

        write_xlsx(
          d,
          path = file
        )

      }
    )

    ## Show the outcome of the filter

    output$outcome_noInfo <- renderDT({

      validate(need(d_noInfo(), message = "Aplica el filtrado antes"))

      d <- d_noInfo()

      print(paste("Number of columns:", ncol(d)))

      d


    })

    ## Identifying columns with conflicts

    output$cols_conflict <- renderDT({

      validate(need(d_noInfo(), message = "Filtra los 'No info' antes"))

      ## Get the reactive

      d <- d_noInfo()

      ## Do operations on the address

      d <- d %>%
        mutate(adreca_establiment = str_to_upper(adreca_establiment)) %>%  # capitalize
        mutate(adreca_establiment = str_trim(adreca_establiment)) %>%  # trim

        fix_address_start() %>%  # fix the C/s
        fix_address_start("^CARRER", "/C") %>%  # fix the CARRERS
        fix_address_start("^AVINGUDA", "/AV") %>%  # fix the AVINGUDAS
        fix_address_start("^AVDA[\\.]*", "/AV") %>%  # fix the AVDA
        fix_address_start("^AV[\\/\\.]*[ ]*", "/AV") %>%  # fix the short AVINGUDAS
        fix_address_start("^CTRA[\\.]*", "/CTRA") %>%  # fix the CTRA.
        fix_address_start("^PG[\\/\\.]*", "/PG") %>%  # fix the PG.
        fix_address_start("^PASSEIG", "/PG") %>%  # fix the PASSEIG
        fix_address_start("^PLAÇA", "/PL") %>%  # fix the PLAÇA
        fix_address_start("^PLAZA", "/PL") %>%  # fix the PLAZA
        fix_address_start("^PL[\\.]*", "/PL") %>%  # fix the PL.
        fix_address_start("^RAMBLA", "/RBLA") %>%  # fix the RAMBLA
        fix_address_start("^RONDA", "/RDA") %>%  # fix the RONDA
        fix_address_start("^RDA[\\.]*", "/RDA") %>%  # fix the RDA
        fix_address_start("^TRAVESSERA", "/TRAV") %>%  # fix the TRAVESSERA
        fix_address_start("^VIA", "/VIA") %>%  # fix the VIA
        fix_address_start("^VÍA", "/VIA")  # fix the VÍA

      ## Do operations on the titular

      d <- d %>%
        mutate(titular_rao_social = str_to_upper(titular_rao_social),
               titular_rao_social = str_trim(titular_rao_social))

      ## Do operations on the nif

      d <- d %>%
        mutate(nif_dni = str_to_upper(nif_dni),
               nif_dni = gsub(" ", "", nif_dni))

      ## Do operations on the nom comercial

      d <- d %>%
        mutate(nom_comercial_de_lestabliment = str_to_upper(nom_comercial_de_lestabliment),
               nom_comercial_de_lestabliment = gsub(" ", "", nom_comercial_de_lestabliment))

      ## Get the mistmatches

      all_mis <- list(
        numero = check_mismatch(d, "establiment_inspeccionat_numero", "numero"),
        adreca_establiment = check_mismatch(d, "establiment_inspeccionat_carrer", "adreca_establiment"),
        # altres_pis = check_mismatch(d, "establiment_inspeccionat_altres_pis", "altres_pis_local"),
        codi_postal = check_mismatch(d, "establiment_inspeccionat_cp", "codi_postal"),
        num_sical = check_mismatch(d, "establiment_inspeccionat_sical", "num_sical"),
        titular_rao_social = check_mismatch(d, "establiment_inspeccionat_titular", "titular_rao_social"),
        nif_dni = check_mismatch(d, "establiment_inspeccionat_nif", "nif_dni"),
        nom_comercial_de_lestabliment = check_mismatch(d, "establiment_inspeccionat_nomestabliment", "nom_comercial_de_lestabliment")
      )

      # all_mis$adreca_establiment$conflict %>% select(establiment_inspeccionat_carrer, adreca_establiment) %>% View()

      ## Display

      all_mis %>%
        map(., ~ .$conflict) %>%
        imap_dfr(., ~ mutate(.x, conflict = .y)) %>%
        select(serial_number, conflict) %>%
        mutate(aa = "X") %>%
        pivot_wider(names_from = conflict, values_from = aa) %>%
        arrange(serial_number) %>%
        datatable()

    })

    ## Downloading conflict metadata report

    output$down_conflict <- downloadHandler(
      filename = "conflicts.xlsx",
      content = function(file) {

        validate(need(d_noInfo(), message = "Filtra los 'No info' antes"))

        ## Get the reactive

        d <- d_noInfo()

        ## Do operations on the address

        d <- d %>%
          mutate(adreca_establiment = str_to_upper(adreca_establiment)) %>%  # capitalize
          mutate(adreca_establiment = str_trim(adreca_establiment)) %>%  # trim

          fix_address_start() %>%  # fix the C/s
          fix_address_start("^CARRER", "/C") %>%  # fix the CARRERS
          fix_address_start("^AVINGUDA", "/AV") %>%  # fix the AVINGUDAS
          fix_address_start("^AVDA[\\.]*", "/AV") %>%  # fix the AVDA
          fix_address_start("^AV[\\/\\.]*[ ]*", "/AV") %>%  # fix the short AVINGUDAS
          fix_address_start("^CTRA[\\.]*", "/CTRA") %>%  # fix the CTRA.
          fix_address_start("^PG[\\/\\.]*", "/PG") %>%  # fix the PG.
          fix_address_start("^PASSEIG", "/PG") %>%  # fix the PASSEIG
          fix_address_start("^PLAÇA", "/PL") %>%  # fix the PLAÇA
          fix_address_start("^PLAZA", "/PL") %>%  # fix the PLAZA
          fix_address_start("^PL[\\.]*", "/PL") %>%  # fix the PL.
          fix_address_start("^RAMBLA", "/RBLA") %>%  # fix the RAMBLA
          fix_address_start("^RONDA", "/RDA") %>%  # fix the RONDA
          fix_address_start("^RDA[\\.]*", "/RDA") %>%  # fix the RDA
          fix_address_start("^TRAVESSERA", "/TRAV") %>%  # fix the TRAVESSERA
          fix_address_start("^VIA", "/VIA") %>%  # fix the VIA
          fix_address_start("^VÍA", "/VIA")  # fix the VÍA

        ## Do operations on the titular

        d <- d %>%
          mutate(titular_rao_social = str_to_upper(titular_rao_social),
                 titular_rao_social = str_trim(titular_rao_social))

        ## Do operations on the nif

        d <- d %>%
          mutate(nif_dni = str_to_upper(nif_dni),
                 nif_dni = gsub(" ", "", nif_dni))

        ## Do operations on the nom comercial

        d <- d %>%
          mutate(nom_comercial_de_lestabliment = str_to_upper(nom_comercial_de_lestabliment),
                 nom_comercial_de_lestabliment = str_trim(nom_comercial_de_lestabliment))

        ## Do operations on the codi postal

        d <- d %>%
          mutate(establiment_inspeccionat_cp = format(establiment_inspeccionat_cp,
                                                      width = 5,
                                                      justify = "right"),
                 establiment_inspeccionat_cp = gsub(" ", "0")
                 )

        ##  Identify mistmatches and arrange

        out <- list(
          numero = check_mismatch(d, "establiment_inspeccionat_numero", "numero")$conflict %>% select(serial_number, "establiment_inspeccionat_numero", "numero"),
          adreca_establiment = check_mismatch(d, "establiment_inspeccionat_carrer", "adreca_establiment")$conflict %>% select(serial_number, "establiment_inspeccionat_carrer", "adreca_establiment"),
          codi_postal = check_mismatch(d, "establiment_inspeccionat_cp", "codi_postal")$conflict %>% select(serial_number, "establiment_inspeccionat_cp", "codi_postal"),
          num_sical = check_mismatch(d, "establiment_inspeccionat_sical", "num_sical")$conflict %>% select(serial_number, "establiment_inspeccionat_sical", "num_sical"),
          titular_rao_social = check_mismatch(d, "establiment_inspeccionat_titular", "titular_rao_social")$conflict %>% select(serial_number, "establiment_inspeccionat_titular", "titular_rao_social"),
          nif_dni = check_mismatch(d, "establiment_inspeccionat_nif", "nif_dni")$conflict %>% select(serial_number, "establiment_inspeccionat_nif", "nif_dni"),
          nom_comercial_de_lestabliment = check_mismatch(d, "establiment_inspeccionat_nomestabliment", "nom_comercial_de_lestabliment")$conflict %>% select(serial_number, "establiment_inspeccionat_nomestabliment", "nom_comercial_de_lestabliment")
        ) %>%
          map(., ~ set_names(., c("serial_number", "izq", "der"))) %>%
          map(., ~ mutate(.,
                          serial_number = as.character(serial_number),  # Empty tibbles were causing issues
                          izq = as.character(izq),
                          der = as.character(der))) %>%
          imap_dfr(., ~ mutate(.x, variable = .y)) %>%
          select(serial_number, variable, izq, der) %>%
          mutate(decision = NA) %>%
          arrange(serial_number)

        ## Output the file

        write_xlsx(
          list(report = out),
          path = file
        )

      }
    )

    ## Uploading fixing conflict actions

    conflict_actions <- reactive({

      validate(need(input$file_conflicts, message = "Sube el archivo de cambios"))

      read_excel(input$file_conflicts$datapath, sheet = "report", col_types = "text")

    })

    ## Summary of the conflicts report

    output$summary_conflicts <- renderTable({  # Feedbak for data import

      validate(need(conflict_actions(), message = "Sube el archivo de correcciones"))

      d <- conflict_actions()

      d %>%
        summarize(n = n(), .by = c("variable", "decision")) %>%
        pivot_wider(names_from = decision, values_from = n)

    })

    ## Reactive without conflicts

    d_noConflict <- reactiveVal()

    ## Skipping the conflicts step

    observeEvent(input$skip_conflicts, {

      validate(need(d_noInfo(), message = "Sube la tabla antes"))

      d <- d_noInfo()

      d_noConflict(d)

    })


    ## Corregir conflictos

    observeEvent(input$apply_conflict, {

      validate(need(d_noInfo(), message = "Corrige los sin info antes"))
      validate(need(conflict_actions(), message = "Sube el archivo de correcciones"))

      # browser()

      ## Load the reactives

      d <- d_noInfo()
      actions <- conflict_actions()

      ## Write the report into the right hand side columns (merge_columns() uses the right, so I can take this shortcut)

       new_actions <- actions %>%
        mutate(serial_number = as.numeric(serial_number),  # some issues when importing
               decision = str_to_upper(decision)
               ) %>%
        mutate(value = if_else(decision == "IZQ", izq, der)) %>%
        select(-izq, -der, -decision) %>%
        mutate(variable = paste0(variable, "_new")) %>%  # add a dummy for the join
        pivot_wider(names_from = "variable", values_from = "value")


       ## Add columns if missing

       for (this_name in c("numero_new", "adreca_establiment_new", "codi_postal_new",
                           "num_sical_new", "titular_rao_social_new", "nif_dni_new",
                           "nom_comercial_de_lestabliment_new") ) {

         if ( !( this_name %in% names(new_actions) ) ) {

           print("aa")
           new_actions[[this_name]] <- NA
         }

       }

       new_actions <- new_actions %>%
         select("serial_number", "numero_new", "adreca_establiment_new",
                "codi_postal_new",
                "num_sical_new", "titular_rao_social_new", "nif_dni_new",
                "nom_comercial_de_lestabliment_new")


      d <- new_actions %>%  # this puts NAs in those not defined
        left_join(d, .) %>%  # join with the large table
        mutate(
          numero = if_else(is.na(numero_new), numero, numero_new),  # we write when not NA
          adreca_establiment = if_else(is.na(adreca_establiment_new), adreca_establiment, adreca_establiment_new),
          codi_postal = if_else(is.na(codi_postal_new), codi_postal, codi_postal_new),
          num_sical = if_else(is.na(num_sical_new), num_sical, num_sical_new),
          titular_rao_social = if_else(is.na(titular_rao_social_new), titular_rao_social, titular_rao_social_new),
          nif_dni = if_else(is.na(nif_dni_new), nif_dni, nif_dni_new),
          nom_comercial_de_lestabliment = if_else(is.na(nom_comercial_de_lestabliment_new), nom_comercial_de_lestabliment, nom_comercial_de_lestabliment_new)
        ) %>%
        select(-numero_new, -adreca_establiment_new,  # remove the dummy columns
               -codi_postal_new,
               -num_sical_new, -titular_rao_social_new,
               -nif_dni_new, -nom_comercial_de_lestabliment_new)

      ## Merge the columns

      out <- d %>%
        merge_columns("establiment_inspeccionat_numero", "numero",
                      remove_col = FALSE, mismatch = "") %>%
        merge_columns("establiment_inspeccionat_carrer", "adreca_establiment",
                      remove_col = FALSE, mismatch = "") %>%
        merge_columns("establiment_inspeccionat_cp", "codi_postal",
                      remove_col = FALSE, mismatch = "") %>%
        merge_columns("establiment_inspeccionat_sical", "num_sical",
                      remove_col = FALSE, mismatch = "") %>%
        merge_columns("establiment_inspeccionat_titular", "titular_rao_social",
                      remove_col = FALSE, mismatch = "") %>%
        merge_columns("establiment_inspeccionat_nif", "nif_dni",
                      remove_col = FALSE, mismatch = "") %>%
        merge_columns("establiment_inspeccionat_nomestabliment", "nom_comercial_de_lestabliment",
                      remove_col = FALSE, mismatch = "")

      ## Update the reactive

      d_noConflict(out)

    })

    ## Showing the output of fixing conflicts

    output$outcome_conflicts <- renderDT({

      validate(need(d_noConflict(), message = "Dale al boton"))

      print(paste("Number of columns:", ncol(d_noConflict())))

      d_noConflict()
    })

    ## Download the outcome of fixing conflicts

    output$down_outputConflicts <- downloadHandler(
      filename = "Actas_noConflicts.xlsx",
      content = function(file) {

        validate(need(d_noConflict(), message = "Resuelve los coflictos antes"))

        d <- d_noConflict()

        write_xlsx(
          d,
          path = file
        )

      }
    )

    ## Calculating the n_d_acta ------------------------------------------------

    d_nActa <- reactiveVal()

    observeEvent(input$apply_n_d_acta, {

      validate(need(d_noConflict(), message = "Resuelve los conflictos antes"))

      d <- d_noConflict()

      codes <- format(d$serial_number,
             width = input$n_digits) %>%
        gsub(" ", "0", .) %>%
        paste0(input$type_of_acta, "_", .)

      d$n_d_acta <- codes

      d_nActa(d)

    })

    ## Same place same time ----------------------------------------------------

    d_paradox <- reactiveVal()

    ## Showing the time paradoxes

    output$report_paradox <- renderDT({

      validate(need(d_nActa(), message = "Define el num de acta antes"))

      print(paste("Number of columns:", ncol(d_nActa())))

      d_nActa() %>%
        summarize(n = n(),
                  .by = c(establiment_inspeccionat_sical, on)) %>%
        filter(n > 1)
    })

    ## Downloading the template for paradoxes

    output$down_paradox <- downloadHandler(
      filename = "time_paradox.xlsx",
      content = function(file) {

        validate(need(d_nActa(), message = "Da los pasos de antes"))

        d <- d_nActa() %>%  # identify the ones
          mutate(n = n(),
                    .by = c(establiment_inspeccionat_sical, on)) %>%
          filter(n > 1) %>%
          select(serial_number, establiment_inspeccionat_sical, on) %>%
          mutate(excluir = " ")

        ## write the file

        write_xlsx(
          d,
          path = file
        )

      }
    )

    ## Uploading fixing conflict actions

    paradox_actions <- reactive({

      validate(need(input$file_paradoxes, message = "Sube el archivo de cambios"))

      read_excel(input$file_paradoxes$datapath, col_types = "text")

    })

    output$summary_paradoxes <- renderTable({  # Feedbak for data import

      validate(need(paradox_actions(), message = "Sube el archivo de correcciones"))

      d <- paradox_actions()

      d %>%
        summarize(n = n(), .by = c("excluir"))

    })

    observeEvent(input$apply_paradoxes, {

      validate(need(d_nActa(), message = "Corrige los sin info antes"))
      validate(need(paradox_actions(), message = "Sube el archivo de correcciones"))

      d <- d_nActa()
      actions <- paradox_actions()

      if (nrow(actions) == 0) {
        out <- d
      } else {

        out_serials <- actions %>%
          filter(excluir)

        out <- d %>% filter(!serial_number %in% out_serials)

      }

      d_paradox(out)

      ## THIS IS TEMPORAL, DO NOT FORGET TO REMOVE THIS!!!! TODO ---------------

      d_final(out)

    })


    ## Skipping the paradoxes step

    observeEvent(input$skip_paradox, {

      validate(need(d_nActa(), message = "Sube la tabla antes"))

      d <- d_nActa()

      d_paradox(d)

    })

    ## SICALS with various people ----------------------------------------------

    d_multisical <- reactiveVal()

    ## TODO: add the NIF to the download

    ## Showing the time paradoxes

    output$report_multisical <- renderDT({

      validate(need(d_paradox(), message = "Define las paradoxes antes"))

      # browser()

      aa <- d_paradox() |>
        summarize(.by = c("num_sical", "nif_dni", "establiment_inspeccionat_titular"))

      bad_sicals <- aa |>
        summarize(n = n(), .by = "num_sical") |>
        filter(n > 1) |>
        pull(num_sical)

      aa |>
        filter(num_sical %in% bad_sicals)

    })

    ## Downloading the template for cases with many sicals

    output$down_multisical <- downloadHandler(
      filename = "multisicals.xlsx",
      content = function(file) {

        validate(need(d_paradox(), message = "Da los pasos de antes"))

        # browser()

        aa <- d_paradox() |>
          summarize(.by = c("num_sical", "nif_dni", "establiment_inspeccionat_titular"))

        bad_sicals <- aa |>
          mutate(n = n(), .by = "num_sical") |>
          filter(n > 1) |>
          pull(num_sical)

        d <- aa |>
          filter(num_sical %in% bad_sicals) |>
          mutate(remplazar_titular = "",
                 remplazar_nif_dni = ""
                 )

        # nrow(d)

        ## write the file

        write_xlsx(
          d,
          path = file
        )

      }
    )

    ## Uploading fixing conflict actions

    multisical_actions <- reactive({

      validate(need(input$file_multisical, message = "Sube el archivo de cambios"))

      read_excel(input$file_multisical$datapath, col_types = "text")

    })

    output$summary_multisical <- renderTable({  # Feedbak for data import

      validate(need(multisical_actions(), message = "Sube el archivo de correcciones"))

      d <- multisical_actions()

      # browser()

      ## Change the titular

      d %>%
        mutate(titular = if_else(is.na(remplazar_titular), "SIN CAMBIO", "CAMBIO"),
               nif_dni = if_else(is.na(remplazar_nif_dni), "SIN CAMBIO", "CAMBIO")
               ) |>
        summarize(n = n(),
                  .by = c("titular", "nif_dni")
                  ) |>
        arrange(desc(n))

    })

    observeEvent(input$apply_multisical, {

      validate(need(d_paradox(), message = "Corrige las paradojas antes"))
      validate(need(multisical_actions(), message = "Sube el archivo de correcciones"))

      d <- d_paradox()

      ## Corregir los titulares

      actions <- multisical_actions() |>
        summarize(.by = c("num_sical", "remplazar_titular")) |>
        filter(!is.na(remplazar_titular))

      if (nrow(actions) == 0) {
        out <- d

      } else {

        out <- left_join(
          d, actions
        ) |>
          mutate(establiment_inspeccionat_titular = if_else(
            is.na(remplazar_titular),
            establiment_inspeccionat_titular,
            remplazar_titular
          )) |>
          select(-remplazar_titular)

      }

      ## Corregir los dni_nif

      actions <- multisical_actions() |>
        summarize(.by = c("num_sical", "remplazar_nif_dni")) |>
        filter(!is.na(remplazar_nif_dni))

      if (nrow(actions) == 0) {
        out <- d

      } else {

        out <- left_join(
          d, actions
        ) |>
          mutate(nif_dni = if_else(
            is.na(remplazar_nif_dni),
            nif_dni,
            remplazar_nif_dni
          )) |>
          select(-remplazar_nif_dni)

      }

      ## Return

      d_multisical(out)

    })

    ## Skipping the step for many titulars per sical

    observeEvent(input$skip_multisical, {

      validate(need(d_paradox(), message = "Sube la tabla antes"))

      d <- d_paradox()

      d_multisical(d)

    })

    ## Check of the cycle start ------------------------------------------------

    d_ciclos <- reactiveVal()

    ## Showing the time paradoxes

    output$report_ciclos <- renderDT({

      validate(need(d_multisical(), message = "Define las paradoxes antes"))

      d_multisical() %>%
        # filter(num_sical %in% c("6800", "6747")) %>%
        mutate(min_data = min(on), .by = "num_sical") |>
        filter(on == min_data) |>
        filter(! (motiu_de_la_inspeccio %in% c("Inicial", "Sobrevinguda", "Per freqüència", "Visita prèvia informativa") )) |>
        select(serial_number, on, num_sical, motiu_de_la_inspeccio)

    })

    ## Downloading the template for paradoxes

    output$down_ciclos <- downloadHandler(
      filename = "ciclos_start.xlsx",
      content = function(file) {

        validate(need(d_multisical(), message = "Define las paradoxes antes"))

        d <- d_multisical() %>%
          # filter(num_sical %in% c("6800", "6747")) %>%
          mutate(min_data = min(on), .by = "num_sical") |>
          filter(on == min_data) |>
          filter(! (motiu_de_la_inspeccio %in% c("Inicial", "Sobrevinguda", "Per freqüència", "Visita prèvia informativa") )) |>
          select(serial_number, on, num_sical, motiu_de_la_inspeccio) |>
          mutate(remplazar = " ")

        ## write the file

        write_xlsx(
          d,
          path = file
        )

      }
    )

    ## Uploading fixing start of cycle

    ciclos_actions <- reactive({

      validate(need(input$file_ciclos, message = "Sube el archivo de cambios"))

      print("Se ha subido el archivo")

      read_excel(input$file_ciclos$datapath, col_types = "text")

    })

    output$summary_ciclos <- renderTable({  # Feedbak for data import

      validate(need(ciclos_actions(), message = "Sube el archivo de correcciones"))

      print("haciendo el resumen")

      d <- ciclos_actions()

      d %>%
        summarize(n = n(), .by = c("remplazar")) |>
        mutate(action = if_else(is.na(remplazar), "SIN CAMBIO", remplazar)) |>
        arrange(desc(n))

    })

    observeEvent(input$apply_ciclos, {

      validate(need(d_multisical(), message = "Corrige las paradojas antes"))
      validate(need(ciclos_actions(), message = "Sube el archivo de correcciones"))

      print("corrigiendo")

      d <- d_multisical()

      actions <- ciclos_actions() |>
        filter(!is.na(remplazar)) |>
        select(serial_number, remplazar) |>
        mutate(serial_number = as.numeric(serial_number))

      if (nrow(actions) == 0) {
        out <- d

      } else {

        out <- left_join(
          d, actions
        ) |>
          mutate(motiu_de_la_inspeccio = if_else(
            is.na(remplazar),
            motiu_de_la_inspeccio,
            remplazar
          )) |>
          select(-remplazar)

      }

      d_ciclos(out)

    })

    ## Skipping the missing info step

    observeEvent(input$skip_ciclos, {

      validate(need(d_multisical(), message = "Sube la tabla antes"))

      d <- d_multisical()

      d_ciclos(d)

    })

    ## Check of the cycle logic ------------------------------------------------

    d_cyclelogic <- reactiveVal()
    # d_logic_issues <- reactiveVal()
    #
    ## Showing the time paradoxes

    d_logic_issues <- reactive({

      validate(need(d_ciclos(), message = "Define las paradoxes antes"))   # TODO: do not forget to update this

      print("Calculando las logicas")

      logic_checks <- d_ciclos() |>
        mutate(outcome = if_else(
          lubridate::year(on) < 2025,
          pre_2025_score(resultat_inspeccio),
          post_2025_score(resultat_inspeccio),
        )) %>%
        split(.$num_sical) %>%
        # set_names(., .) |>
        map(check_sical_logic)

      logic_checks |>
        bind_rows() |>
        filter(issue != "OK", issue != "just one") |>
        select(serial_number, num_sical,
               `Motivo anterior` = prev_motive,
               `Resultado anterior` = prev_outcome,
               `Este motivo` = this_motive,
               `Problema potencial` = issue)

    })

    output$report_cyclelogic <- renderDT({

      # validate(need(d_logic_issues(), message = "Define las paradoxes antes"))

      d_logic_issues()

    })
    #
    ## Downloading the template for cycle logic

    output$down_cyclelogic <- downloadHandler(
      filename = "ciclos_logicos.xlsx",
      content = function(file) {

        # validate(need(d_logic_issues(), message = "Define las paradoxes antes"))

        out <- d_logic_issues() |>
          mutate(new_motive = " ")

        ## write the file

        write_xlsx(
          out,
          path = file
        )

      }
    )

    ## Uploading fixing for logic issues

    cyclelogic_actions <- reactive({

      validate(need(input$file_cyclelogic, message = "Sube el archivo de cambios"))

      read_excel(input$file_cyclelogic$datapath, col_types = "text")

    })

    output$summary_cyclelogic <- renderTable({  # Feedbak for data import

      validate(need(cyclelogic_actions(), message = "Sube el archivo de correcciones"))

      d <- cyclelogic_actions()

      d %>%
        summarize(n = n(), .by = c("new_motive")) |>
        mutate(action = if_else(is.na(new_motive), "SIN CAMBIO", new_motive)) |>
        arrange(desc(n))

    })

    observeEvent(input$apply_cyclelogic, {

      validate(need(d_ciclos(), message = "Corrige las paradojas antes"))
      validate(need(cyclelogic_actions(), message = "Sube el archivo de correcciones"))

      d <- d_ciclos()

      actions <- cyclelogic_actions() |>
        filter(!is.na(new_motive)) |>
        select(serial_number, new_motive) |>
        mutate(serial_number = as.numeric(serial_number))

      if (nrow(actions) == 0) {
        out <- d

      } else {

        out <- left_join(
          d, actions
        ) |>
          mutate(motiu_de_la_inspeccio = if_else(
            is.na(new_motive),
            motiu_de_la_inspeccio,
            new_motive
          )) |>
          select(-new_motive)

      }

      d_ciclos(out)

      d_final(out)

    })

    ## Skipping the missing info step

    observeEvent(input$skip_ciclos, {

      validate(need(d_ciclos(), message = "Sube la tabla antes"))

      d <- d_ciclos()

      d_cyclelogic(d)

      d_final(d)

    })

    ## -------------------------------------------------------------------------

    ## Reactive for the final outcome

    d_final <- reactiveVal()

    ## Download the final outcome

    output$down_final <- downloadHandler(
      filename = "final.xlsx",
      content = function(file) {

        validate(need(d_final(), message = "Da los pasos de antes"))

        d <- d_final()
        ## Do operations on the codi postal  TODO: check whether I need this

        d <- d %>%
          mutate(establiment_inspeccionat_cp = format(establiment_inspeccionat_cp,
                                                      width = 5,
                                                      justify = "right"),
                 establiment_inspeccionat_cp = gsub(" ", "0", establiment_inspeccionat_cp)
          )

        ## write the file

        write_xlsx(
          d,
          path = file
        )

      }
    )



    ## Showing the final outcome

    output$final_data <- renderDT({

      validate(need(d_final(), message = "Da los pasos de antes"))

      d <- d_final()
      ## Do operations on the codi postal  TODO: check whether I need this

      d <- d %>%
        mutate(establiment_inspeccionat_cp = format(establiment_inspeccionat_cp,
                                                    width = 5,
                                                    justify = "right"),
               establiment_inspeccionat_cp = gsub(" ", "0", establiment_inspeccionat_cp)
        )

      ## return
      d


    })

  })
}

## To be copied in the UI
# mod_cleaner_ui("cleaner_1")

## To be copied in the server
# mod_cleaner_server("cleaner_1")

















