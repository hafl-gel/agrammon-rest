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
    valid_data <- check_and_validate(raw_input)

    # prepare input 
    loop_data <- valid_data$data[, .(farm_id_, module, variable, value)]

    # add header part
    curl::handle_setheaders(hdl,
        'Content-Type' = 'multipart/form-data',
        'Authorization' = paste0('Bearer ', token = token),
        'Accept' = 'text/csv'
        )

    # loop over data
    res <- loop_data[, {
        # write to character input
        input_data <- paste(module, variable, value, sep = ';', collapse = '\n')
        # add body
        curl::handle_setform(hdl,
            variants = model_options[['variants']],
            model = model_options[['model']],
            technical = model_options[['technical']],
            simulation = format(Sys.time(), '%Y-%m-%d %H:%M'),
            dataset = as.character(.BY$farm_id_),
            language = model_options[['language']],
            'print-only' = model_options[['print']],
            inputs = form_data(input_data, "text/csv")
        )
        # call model
        req <- curl_fetch_memory("https://model.agrammon.ch/singleRest/api/v1/run", handle = hdl)
        # convert to data.table
        fread(text = rawToChar(req$content))[, -(1:2)]
    }, by = farm_id_]

    # set colnames
    setnames(res, paste0('V', 3:7), c('module', 'variable', 'filter', 'value', 'unit'))
    # get names
    nms <- copy(names(res))
    # add farm id
    if (is.null(valid_data$farm_id)) {
        res[, farm_id := 1L]
    } else {
        # get key
        farm_key <- unique(valid_data$data[, .(farm_id_, x = get(valid_data$farm_id))])[, setNames(x, farm_id_)]
        # replace entries
        res[, farm_id := farm_key[farm_id_]]
    }
    # move farm_id to front
    setcolorder(res, c('farm_id', nms))
    # remove farm_id_
    res[, farm_id_ := NULL]
    # add unique cols
    if (!is.null(valid_data$unique_cols)) {
        res[, (valid_data$unique_cols) := valid_data$data[1, valid_data$unique_cols, with = FALSE]]
    }
    # return
    res[]
}

# TODO: check all cases!!!
# file <- './tests/inputs-version6-rest.csv'
# file <- './tests/input_data/test_1farm_no_id.csv'
# file <- './tests/input_data/test_1farm_incl_id.csv'
# file <- './tests/input_data/test_3farms_no_id.csv'
file <- './tests/input_data/test_3farms_incl_id.csv'

run_model(file)

check_and_validate <- function(dt) {
    # read input vars
    temp <- create_template(TRUE)
    # remove Note: and NA rows???
    temp <- temp[!is.na(module)][!grepl('Note:', module, fixed = TRUE)]
    # has instance?
    temp[, has_instance := grepl('[', module, fixed = TRUE)]
    # add module without instance with variable
    temp[, module_var := paste(module, variable, sep = ';')]
    temp[(has_instance), module_var := {
        paste(
            unlist(lapply(strsplit(module, '[[]|[]]'), function(x) paste0(x[-2], collapse = ''))),
            variable,
            sep = ';'
            )
    }]
    # values from mandatory enums
    mand_enums <- temp[default == '' & !grepl(
        '^$|^between|^greater( or equal)? th(a|e)n|^smaller( or equal)? th(a|e)n', 
        remarks),
        unlist(strsplit(remarks, split = ',', fixed = TRUE))]
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
    # find value (mandatory entries)
    nm_val <- dt[, {
        names(.SD)[which.max(lapply(.SD, function(x) sum(x %in% mand_enums)))]
    }, .SDcols = setdiff(names(dt), c(valid_module[[1]], nm_var))]
    # rename columns
    setnames(dt, c(valid_module[[1]], nm_var, nm_val), c('module', 'variable', 'value'))
    # get names without above columns
    nms_extra_cols <- setdiff(names(dt), c('module', 'variable', 'value'))
    # has instance?
    dt[, has_instance := grepl('[', module, fixed = TRUE)]
    # add module without instance with variable
    dt[, c('module_var', 'instance') := list(paste(module, variable, sep = ';'), '')]
    dt[(has_instance), c('module_var', 'instance') := {
        spl <- strsplit(module, '[[]|[]]')
        list(
            paste(
                unlist(lapply(spl, function(x) paste0(x[-2], collapse = ''))),
                variable,
                sep = ';'
                ),
            unlist(lapply(spl, '[', 2))
            )
    }]
    # rename temp names
    setnames(temp, names(temp), paste0(names(temp), '_'))
    #########
    # check mandatory - without instance
    temp_no <- temp[!(has_instance_)][default_ %chin% '', ]
    check_no <- merge(temp_no, dt, by.x = 'module_var_', by.y = 'module_var', all.x = TRUE)[, {
        c(list(num = sum(!is.na(value))), .SD)
    }, by = module_var_]
    # prepare output farm id & unique cols
    fic_ <- NULL
    unique_cols <- NULL
    # check number of entries
    list_ids <- check_no[, {
        # get tab
        tab <- table(num)
        # get unique entries in num
        nms_tab <- as.integer(names(tab))
        # throw error if an entry is missing
        if (0 %in% nms_tab) {
            ind <- 0 %in% num
            stop('Following mandatory input is missing:\n',
                paste0('', module[ind], ' -> ', variable[ind], '\n')
            )
        }
        if (length(nms_tab) > 1) {
            # get max
            n_max <- nms_tab[which.max(tab)]
            # check smaller
            if (any(nms_tab < n_max)) {
                # missing entries
                ind <- which(num < n_max)
                stop(
                    'Missing entries!\n',
                    'The majority of mandatory input variables appear ', n_max, ' times.\n',
                    if (length(ind) > 1) {
                        paste0('The following mandatory input variable appears less than ', n_max,' times:\n')
                    } else {
                        paste0('The following mandatory input variables appear less than ', n_max,' times:\n')
                    },
                    paste0(module[ind], ' -> ', variable[ind], '\n')
                )
            }
            # check larger
            if (any(nms_tab > n_max)) {
                # duplicates
                ind <- which(num > n_max)
                stop(
                    'Too many entries!\n',
                    'The majority of mandatory input variables appear ', n_max, ' times.\n',
                    if (length(ind) > 1) {
                        paste0('The following mandatory input variable appears more than ', n_max,' times:\n')
                    } else {
                        paste0('The following mandatory input variables appear more than ', n_max,' times:\n')
                    },
                    paste0(module[ind], ' -> ', variable[ind], '\n')
                )
            }
        } else if(nms_tab != 1) {
            # check farm id in additional columns
            if (length(nms_extra_cols) == 0) {
                # missing farm id
                stop('A column containing the farm id is required\n',
                    '    if more than one farm is specified in the input!')
            }
            # get lengths of unique entries
            nc <- dt[, {
                lapply(.SD, function(x) {
                        lux <- length(unique(x))
                        # check unique col entries != ''
                        if (lux == 1 && x[1] == '') {
                            0L
                        } else {
                            lux
                        }
                    })
            }, .SDcols = nms_extra_cols]
            # get fic_ (farm id col) name (error if not found)
            if (any(nf <- nc == nms_tab)) {
                fic_ <- nms_extra_cols[nf]
                if (sum(nf) > 1) {
                    # farm id col not detectable
                    # could be solved by checking each set according to fic_ entries, but this is too much hassle...
                    stop('farm id column cannot be detected. Multiple columns contain ', nms_tab, ' unique entries.')
                }
            }
            # get unique cols
            if (any(nf <- nc == 1)) {
                unique_cols <- nms_extra_cols[nf]
            }
        } else {
            # check additional columns
            if (length(nms_extra_cols) > 0) {
                # find unique col entries != ''
                unique_cols <- dt[, {
                    ind <- which(unlist(lapply(.SD, function(x) {
                        ux <- unique(x)
                        length(ux) == 1 && ux != ''
                    })))
                    if (length(ind) > 0) {
                        names(.SD)[ind]
                    } else {
                        NULL
                    }
                }, .SDcols = nms_extra_cols]
            }
        }
        # return
        I(list(farm_id = fic_, unique_cols = unique_cols))
    }]
    # get/add fic_ (to loop by)
    if (is.null(list_ids$farm_id)) {
        dt[, farm_id_ := 1L]
    } else {
        dt[, farm_id_ := frank(factor(get(list_ids$farm_id)), ties.method = 'dense')]
    }
    # check mandatory - with instance
    temp_ins <- temp[(has_instance_)][default_ %chin% '', ]
    # select check with instance
    check_ins <- merge(temp_ins, dt, by.x = 'module_var_', by.y = 'module_var', all.x = TRUE)[, {
        c(list(num = sum(!is.na(value))), .SD)
    }, by = module_var_]
    # add module parent to check
    check_ins[, parent := tstrsplit(module_, '[[]|[]]', keep = 1)]
    # loop by farm id, parent
    check_ins[, {
        # all is.na -> not existing
        if (!all(is_na <- is.na(instance))) {
            if (any(is_na)) {
                # missing entry
                stop('Missing entry!',
                    if (sum(is_na) == 1) {
                        paste0('Input ', variable_[is_na], ' is missing\n')
                    } else {
                        paste0('Input variables ', paste(variable_[is_na], sep = ','), ' are missing\n')
                    },
                    'for Instance ', instance[!is_na][1]
                )
            }
        }
    }, by = .(farm_id_, parent)]
    # any invalid entries:
    dt[!(module_var %chin% temp[, module_var_]), {
        if (.N > 0) {
            stop(
                'Entries not valid!\n',
                if (.N == 1) {
                    'The following input entry is not valid:\n'
                } else {
                    'The following input entries are not valid:\n'
                },
                paste0(module, ' -> ', variable, '\n')
            )
        }
    }]
    # return
    c(list(data = dt), list_ids)
}


# Fun: fast %in%/%ex% (is in-/excluded?) via C++ set..?
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
