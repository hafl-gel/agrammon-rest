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
    if (!require('curl')) {
        stop('package "curl" is not available!\n\n', 
            '    install.packages("curl")\n\n')
    }
    # check if data.table is installed
    if (!require('data.table')) {
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
    temp <- create_template(TRUE)
    # has instance?
    temp[, has_instance := grepl('[', module, fixed = TRUE)]
    # add instance parent
    temp[, module_wo := module]
    temp[(has_instance), module_wo := {
        unlist(lapply(strsplit(module, '[[]|[]]'), function(x) paste0(x[-2], collapse = '')))
    }]
    # find module
    valid_module <- dt[, {
        check_semicolon <- lapply(.SD, grepl, pattern = "::", fixed = TRUE)
        semicol <- names(.SD)[which.max(lapply(check_semicolon, sum))]
        I(list(semicol, check_semicolon[[semicol]]))
    }]
    # remove invalid columns
    dt <- dt[which(valid_module[[2]]), ]
    # find variable
    nm_var <- dt[, {
        names(.SD)[which.max(lapply(.SD, function(x) sum(x %in% temp[, variable])))]
    }, .SDcols = setdiff(names(dt), valid_module[[1]])]
    # find value
    browser()
    nm_val <- dt[, {
        suppressWarnings(
            names(.SD)[which.max(lapply(.SD, function(x) sum(!is.na(as.numeric(x)))))]
        )
    }, .SDcols = setdiff(names(dt), c(valid_module[[1]], nm_var))]
    # rename columns
    setnames(dt, c(valid_module[[1]], nm_var, nm_val), c('module', 'variable', 'value'))
    # get names without above columns
    nms_dt <- setdiff(names(dt), c('module', 'variable', 'value'))
    # has instance?
    dt[, has_instance := grepl('[', module, fixed = TRUE)]
    # add instance parent
    dt[, c('module_wo', 'instance') := list(module, '')]
    dt[(has_instance), c('module_wo', 'instance') := {
        spl <- strsplit(module, '[[]|[]]')
        list(
            unlist(lapply(spl, function(x) paste0(x[-2], collapse = ''))),
            unlist(lapply(spl, '[', 2))
            )
    }]
    # find
    # check mandatory
    # check residual if exist
    # validate -> loop (by) over temp col remarks
    #       \-> set names of temp to temp_ -> name collision
    # check for input columns + simulation/farm id
    loop over nms_dt
    # simulation id: unique over all valid entries
    # farm id: unique over within-farm valid entries
    # if ncol > 3 -> do we find simulation and/or farm id in them?
    # or: search for input -> what is left?
}

run_model('./tests/inputs-version6-rest.csv')

# Fun: fast %in% via C++ set..?
# lualine -> clone + change buffer function + change highlighting of alternate file (#)

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
