#' title
#'
#' description
#'
#' @param token Agrammon REST-API access token. Can be provided as option entry 'agrammon.token'
#' @return  result from an Agrammon model run
#' @export
#' @examples
#' ## examples here
#' run_model('./tests/inputs-version6-rest.csv')
#' run_model('./tests/inputs-version6-rest.csv', language = 'de')
run_model <- function(input_file, simulation = format(Sys.time(), '%Y-%m-%d %H:%M'),
    farm_id = NULL, model_options = agrammon_options(...), ..., token = NULL) {

    # check if curl is installed
    if (!requireNamespace('curl')) {
        stop('package "curl" is not available!\n\n', 
            '    install.packages("curl")\n\n')
    }
    # check if data.table is installed
    if (!requireNamespace('data.table')) {
        stop('package "data.table" is not available!\n\n', 
            '    install.packages("data.table")\n\n')
    }

    # create handle
    hdl <- curl::new_handle()

    # set request option to post:
    curl::handle_setopt(hdl, customrequest = 'POST')

    # check token
    if (is.null(token) && ((token <- Sys.getenv('AGRAMMON_TOKEN')) == '')) {
        stop('no agrammon token available')
    } else if (!is.character(token) || token == '') {
        stop('input to argument "token" cannot be valid')
    }

    # read input file
    raw_input <- fread(file = input_file, showProgress = FALSE)

    # validate input
    input_data <- check_and_validate(raw_input, simulation, farm_id)

    # add header part
    curl::handle_setheaders(hdl,
        'Content-Type' = 'multipart/form-data',
        'Authorization' = paste0('Bearer ', token = token),
        'Accept' = 'text/csv'
        )

    # add body
    curl::handle_setform(hdl,
        variants = model_options[['variant']],
        model = model_options[['version']],
        technical = model_options[['tech_file']],
        simulation = labels[['simulation']],
        dataset = labels[['id']],
        language = model_options[['language']],
        'print-only' = model_options[['print']],
        inputs = form_data(input_data, "text/csv")
    )

    # call model

    # clean up model results
}

check_and_validate <- function(dt, simulation, farm_id) {
    # read input vars
    # check if ncol > 3
    browser()
    # if ncol > 3 -> do we find simulation and/or farm id in them?
    # or: search for input -> what is left?
    # find module
    valid_module <- dt[, {
        check_semicolon <- lapply(.SD, grepl, pattern = "::", fixed = TRUE)
        semicol <- which.max(sapply(check_semicolon, sum, USE.NAMES = FALSE))
        I(list(semicol, check_semicolon[[semicol]]))
    }]
    # remove invalid columns
    dt <- dt[which(valid_module[[2]]), ]
    # find variables, values and others
    var_val <- dt[, {
        # variables
        # find 'dilution_parts_water'? -> find a mandatory input variable
        # values
        # find which.max(!is.na(as.numeric))
    }, .SDcols = names(dt)[-valid_module[[1]]]]
}

# TODO:
#   - validate input file
#   - extract simulation & ... from input file, if given
#   - add option to use filters (animal cat)
# add check_input()
# check as csv:
# check mandatory input vars -> ok?
# check residual input -> does it match any optional vars?
# remove invalid input with message?
# add read_input (read multiple data sets from one csv)

#' title
#'
#' description
#'
#' @param 
#' @return  
#' @export
#' @examples
#' ## examples here
#' agrammon_options(show = TRUE)
#' agrammon_options(language = 'de', print = c('LivestockNH3', 'LivestockTAN'))
agrammon_options <- function(..., show = FALSE) {
    # assign defaults
    defaults <- list(
        # can be changed:
        free = list(
            language = list(
                help = 'language in output', 
                values = c('en', 'de', 'fr')
                ),
            print = list(
                help = 'output subset(s) to return\n    ("" == detailed model output)',
                values = c('', 
                'SummaryTotal', 'SummaryLivestock', 'SummaryPlantProduction',
                'ResultsTotal', 'ResultsLivestock', 'ResultsPlantProduction',
                'LivestockNH3', 'PlantNH3',
                'LivestockNtot', 'LivestockTAN',
                'LivestockN2', 'LivestockNO', 'LivestockN2O'
                )
            ),
            variants = list(
                help = 'model variant',
                values = c('Base', 'Kantonal_LU')
            )
        ),
        # fixed:
        fixed = list(
            model = c('version6', 'version4', 'version5'),
            technical = c('technical.cfg', 'technical1990.cfg', 'technical1995.cfg', 
                'technical2002.cfg', 'technical2007.cfg', 'technical2010.cfg')
        )
    )
    # check show
    if (show) {
        sp <- function(x) paste0(rep(' ', x), collapse = '')
        # list options here
        cat('~~~~ agrammon options:\n(defaults are marked by *)\n')
        for (lnms in names(defaults[['free']])) {
            cat(lnms, ':\n    ', sep = '')
            cat(defaults[['free']][[lnms]][['help']], '\n    ')
            cat('options:\n', sp(7), '*"', 
                paste0(defaults[['free']][[lnms]][['values']], 
                    collapse = paste0('"\n', sp(8), '"')), 
                '"\n', sep = '')
        }
        cat('~~~~\n')
    }
    # prepare output
    out <- c(
        lapply(defaults[['free']], function(x) x[['values']][1]),
        lapply(defaults[['fixed']], '[[', 1)
        )
    # get dots
    dots <- list(...)
    # loop over free options
    for (fnms in names(defaults[['free']])) {
        if (fnms %in% names(dots)) {
            if (fnms == 'print' & length(dots[[fnms]]) > 1) {
                # check for empty string
                if (any(dots[[fnms]] %in% c('', ' '))) {
                    out[[fnms]] <- ''
                } else {
                    out[[fnms]] <- paste(dots[[fnms]], collapse = ',')
                }
            } else {
                out[[fnms]] <- dots[[fnms]]
            }
        }
    }
    # return invisible output
    if (show) {
        invisible(out)
    } else {
        out
    }
}
