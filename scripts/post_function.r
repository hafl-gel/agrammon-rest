#' title
#'
#' description
#'
#' @param token Agrammon REST-API access token. Can be provided as option entry 'agrammon.token'
#' @return  result from an Agrammon model run
#' @export
#' @examples
#' ## examples here
#' run_model('tests/input-version6.json')
run_model <- function(input_file, simulation = format(Sys.time(), '%Y-%m-%d %H:%M'),
    id_labels = 'farm_count', agrammon_options = agrammon_options(), ..., token = NULL) {

    # check if curl is installed
    if (!requireNamespace('curl')) {
        stop('package "curl" is not available!\n\n', 
            '    install.packages("curl")\n\n')
    }

    # create handle
    hdl <- curl::new_handle()

    # set request option to post:
    curl::handle_setopt(hdl, customrequest = 'POST')

    browser()
    # get dots
    dots <- list(...)

    # check agrammon options
    #  - token
    if (!is.null(agrammon_options[['token']]) {
        if (!is.null(token)) {
            warning('argument token has been provided - ignoring token list entry in agrammon_options!')
            agrammon_options[['token']] <- NULL
        } else {
            token <- agrammon_options[['token']]
        }
    }
    #  - model options (read from options?)
    model_options <- list()
    # check for 
    # token
    # .token
    # variants
    # model
    # ...

    # check input file and pass as form_data instead of form_file!

    # check token
    if (missing(token) && is.null(token <- getOption('agrammon.token'))) {
        stop('agrammon token must be provided, either by argument "token" or by option "agrammon.token"')
    }

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
        inputs = form_file(input_file, "text/csv")
    )
}

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
                help = 'language in output (default: en)', 
                values = c('en', 'de', 'fr')
                ),
            print = list(
                help = 'output subset(s) to return (default: "" => complete model output)',
                values = c('', 
                'SummaryTotal', 'SummaryLivestock', 'SummaryPlantProduction',
                'ResultsTotal', 'ResultsLivestock', 'ResultsPlantProduction',
                'LivestockNH3', 'PlantNH3',
                'LivestockNtot', 'LivestockTAN',
                'LivestockN2', 'LivestockNO', 'LivestockN2O'
                )
            ),
            variants = list(
                help = 'model variant (default: Base)',
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
        sp8 <- paste0(rep(' ', 8), collapse = '')
        # list options here
        cat('~~~~ agrammon options:\n')
        for (lnms in names(defaults[['free']])) {
            cat(lnms, ':\n    ', sep = '')
            cat(defaults[['free']][[lnms]][['help']], '\n    ')
            cat('options:\n', sp8, '"', 
                paste0(defaults[['free']][[lnms]][['values']], 
                    collapse = paste0('"\n', sp8, '"')), 
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

# TODO:
#   - add function to write csv template with option to provide animal categories & storage tanks?
#   - validate input file
#   - extract simulation & ... from input file, if given
#   - add option to use filters (animal cat)
