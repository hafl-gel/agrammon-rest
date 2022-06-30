
# TODO:
#   - don't export agrammon_options, run_model, ...
#   - add vignette (incl. data set)
# # TODO: check all cases!!!

#' title
#'
#' description
#'
#' @param token Agrammon REST-API access token. Can be provided as option entry 'agrammon.token'
#' @return  result from an Agrammon model run
#' @export
#' @examples
#' ## examples here
#' run_agrammon('./tests/inputs-version6-rest.csv')
#' run_agrammon('./tests/inputs-version6-rest.csv', report = 'NH3', filter = 'ex')
run_agrammon <- function(input_file, 
    report = c('summary', 'detailed', 'full', 'NH3', 'TAN', 'N', 'HAFL')[1],
    filter = c('total_only', 'existing_categories', 'all_categories')[1], 
    language = c('en', 'de', 'fr')[1], token = NULL, ...) {
    # check input_file
    if (!is.character(input_file)) {
        stop('argument "input_file" must be of type character')
    }
    if (!file.exists(input_file)) {
        stop('input file does\'nt exist')
    }
    # process arguments
    dots <- list(...)
    # check 'print-only'
    if ('print-only' %in% names(dots)) {
        # assign print-only
        print_only <- dots[['print-only']]
        if (!is.character(print_only)) {
            stop('print-only must be of type character')
        }
        # check report
        if (!missing(report)) {
            print_only <- c(print_only, check_report(report))
        }
        # check empty 'print-only' (-> full report)
        if ('' %in% print_only) {
            print_only <- ''
        }
        # remove duplicates
        print_only <- unique(print_only)
    } else {
        # check report
        print_only <- check_report(report)
    }
    # filter
    if (!missing(filter)) {
        # check argument filter
        if (length(filter) != 1 || !is.character(filter) || 
            is.na(filter <- pmatch(filter, eval(formals(run_agrammon)[['filter']][[2]])))) {
            stop('argument "filter" is not valid')
        }
        # check 'include-filters' and 'all-filters'
        if (any(c('include-filters', 'all-filters') %in% names(dots))) {
            stop('either argument "filter" or arguments "include-filters"',
                ' and/or "all-filters" can be defined, but not both')
        }
        # assign filter
        filter <- switch(filter,
            total = c('false', 'false'),
            existing_categories = c('true', 'false'),
            all_categories = c('true', 'true')
            )
    } else {
        filter <- c('false', 'false')
        if ('include-filters' %in% names(dots)) {
            filter[1] <- dots[['include-filters']]
        }
        if ('all-filters' %in% names(dots)) {
            filter[2] <- dots[['all-filters']]
        }
    }
    # model_options
    model_options <- do.call(agrammon_options, 
        c(
            list(
                'print-only' = print_only,
                'include-filters' = filter[1],
                'all-filters' = filter[2],
                'language' = language
            ),
            dots[!(names(dots) %in% 
                c('print-only', 'include-filters', 'all-filters', 'language'))]
        )
    )
    # token
    token <- check_token(token)
    # run model
    run_model(input_file, model_options, token)
}

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
run_model <- function(input_file, model_options = agrammon_options(), token = NULL) {
    # check if curl is installed
    if (!require('curl')) {
        stop('package "curl" is not available!\n\n', 
            '    install.packages("curl")\n\n')
    }
    # check if jsonlite is installed
    if (!require('jsonlite')) {
        stop('package "jsonlite" is not available!\n\n', 
            '    install.packages("jsonlite")\n\n')
    }
    # check if data.table is installed
    if (!require('data.table')) {
        stop('package "data.table" is not available!\n\n', 
            '    install.packages("data.table")\n\n')
    }
    # help user
    message('\n~~~~ AGRAMMON REST call ~~~~\n')
    # create handle
    hdl <- curl::new_handle()
    # set request option to post:
    curl::handle_setopt(hdl, customrequest = 'POST')
    # help user
    message('validating input file... ', appendLF = FALSE)
    # read input file
    raw_input <- fread(file = input_file, showProgress = FALSE)
    # validate input
    valid_data <- check_and_validate(raw_input)
    # help user
    message('ok')
    # print input (be verbose)
    print_input(valid_data)
    # prepare input 
    loop_data <- valid_data$data[, .(farm_id_, module, variable, value)]
    # add header part
    curl::handle_setheaders(hdl,
        'Content-Type' = 'multipart/form-data',
        'Authorization' = paste0('Bearer ', token = token),
        'Accept' = 'text/csv'
        )
    # help user
    message('sending data ...')
    # loop over data (and re-order rows)
    res <- loop_data[, {
        # write to character input
        input_data <- paste(module, variable, value, sep = ';', collapse = '\n')
        # add body
        curl::handle_setform(hdl,
            simulation = format(Sys.time(), '%Y-%m-%d %H:%M'),
            dataset = as.character(.BY$farm_id_),
            inputs = form_data(input_data, "text/csv"),
            .list = model_options
        )
        # help user
        message('  farm #', .BY$farm_id_, appendLF = FALSE)
        # call model
        req <- curl_fetch_memory(sprintf('%s/run', Sys.getenv('agrammon_rest_url')), 
            handle = hdl)
        # check return
        char <- check_request(req)
        # help user
        message(' done')
        # convert to data.table
        out <- fread(text = char, colClasses = 'character')[, -(1:2)]
    }, by = farm_id_]
    # help user
    message('cleaning up...')
    # set colnames
    setnames(res, paste0('V', 3:7), c('module', 'variable', 'filter', 'value', 'unit'))
    # get names and reorder
    nms <- copy(names(res))
    # add numeric value column
    suppressWarnings(res[, value_num := as.numeric(value)])
    # remove replace value by value_num if all numeric
    if (res[, !anyNA(value_num)]) {
        res[, value := value_num]
        res[, value_num := NULL]
    } else {
        res[, value_chr := value]
        res[!is.na(value_num), value_chr := NA_character_]
        nms <- c(nms, 'value_num', 'value_chr')
    }
    # add farm id
    if (is.null(valid_data$farm_id)) {
        res[, farm_id := 1L]
    } else {
        # get key
        farm_key <- unique(valid_data$data[, .(farm_id_, x = get(valid_data$farm_id))])[, setNames(x, farm_id_)]
        # replace entries
        res[, farm_id := farm_key[farm_id_]]
    }
    # add 'stage' (top module)
    res[, stage := sub('^([a-zA-Z]*)::.*', '\\1', module)]
    # add tracer column
    res[, tracer := sub('((nh3|n2|no|n2o|n|tan)_)?.*', '\\2', variable)]
    # add type column
    res[, variable_type := 'internal'][tracer %in% c('n', 'tan'), variable_type := 'flow']
    res[tracer %in% c('nh3', 'n2', 'no', 'n2o'), variable_type := 'loss']
    # add unique cols
    if (!is.null(valid_data$unique_cols)) {
        res[, (valid_data$unique_cols) := valid_data$data[1, valid_data$unique_cols, with = FALSE]]
        if (length(valid_data$unique_cols) == 1) {
            # rename unique column
            setnames(res, valid_data$unique_cols, 'simulation')
            # reorder columns
            setcolorder(res, c('simulation', 'farm_id', 'stage', 'variable_type', 'tracer',  nms))
        } else {
            # reorder columns
            setcolorder(res, c('farm_id', 'stage', 'variable_type', 'tracer', nms, valid_data$unique_cols))
        }
    } else {
        # reorder columns
        setcolorder(res, c('farm_id', 'stage', 'variable_type', 'tracer', nms))
    }
    # reorder rows
    stages_out <- c('Livestock', 'Storage', 'Application', 'PlantProduction', 'Total')
    res <- res[order(farm_id_, match(stage, stages_out, nomatch = 999))]
    # remove farm_id_
    res[, farm_id_ := NULL]
    # help user
    message('\n~~~~ finished ~~~~\n')
    # return
    res[]
}

#' title
#'
#' description
#'
#' @param token Agrammon REST-API access token. Can be provided as option entry 'agrammon.token'
#' @return  result from an Agrammon model run
#' @export
#' @examples
#' ## examples here
#' wide_output(res)
wide_output <- function(x) {
    # set sorting
    sort_tracer <- c('nh3', 'n', 'tan', 'n2', 'no', 'n2o')
    sort_stage <- c('Livestock', 'Storage', 'Application', 'PlantProduction', 'Total')
    sort_filter <- c('dairy_cows', 'heifers_1st_yr', 'heifers_2nd_yr', 'heifers_3rd_yr',
        'suckling_cows', 'calves_suckling_cows', 'beef_cattle', 'fattening_calves', 'dry_sows',
        'nursing_sows', 'weaned_piglets_up_to_25kg', 'boars', 'fattening_pigs', 'growers', 'layers',
        'broilers', 'turkeys', 'other_poultry', 'horses_older_than_3yr', 'horses_younger_than_3yr', 
        'ponies_and_asses', 'fattening_sheep', 'milksheep', 'goats')
    # check if data.table
    if (isdf <- !is.data.table(x)) {
        setDT(x)
    } else {
        x <- copy(x)
    }
    # check if any var type != 'internal'
    if (x[, all(variable_type == 'internal')]) {
        stop('no loss or flow variables in results')
    }
    # sort by tracer & stage
    y <- x[variable_type != 'internal' & filter != 'total'][, c('s_tracer', 's_stage') := {
        list(
            frank(factor(tracer, levels = sort_tracer), ties.method = 'dense'),
            frank(factor(stage, levels = sort_stage), ties.method = 'dense')
            )
    }]
    # dcast to wide
    out <- dcast(y, farm_id + filter ~ s_tracer + s_stage + variable, value.var = 'value')
    # remove sorting indices
    setnames(out, names(out), sub('^[0-9]+_[0-9]+_', '', names(out)))
    # remove filter if '' or sort
    if (out[, filter[1] == '']) {
        out[, filter := NULL]
    } else {
        out <- out[order(farm_id, match(filter, sort_filter, nomatch = 9999))]
    }
    # change back to data.frame
    if (isdf) {
        setDF(out)
    }
    # return
    out[]
}

#' title
#'
#' description
#'
#' @param token Agrammon REST-API access token. Can be provided as option entry 'agrammon.token'
#' @return  result from an Agrammon model run
#' @export
#' @examples
#' ## examples here
save_excel <- function(x, file, wide_format = FALSE, asTable = TRUE) {
    if (!require(openxlsx)) {
        stop('package "openxlsx" is not available!\n\n', 
            '    install.packages("openxlsx")\n\n')
    }
    # check if data.table/.frame
    if (!is.data.frame(x)) {
        stop('argument "x" must be an object of class data.frame or data.table')
    }
    # wide format?
    if (wide_format) {
        x <- wide_output(x)
    }
    write.xlsx(x, file, asTable = asTable, overwrite = TRUE)
}


# check input file
# possible different approach: 
#   read_input (+ validate)
#   print.agrammon_input
#   run_model with input instead of file
print_input <- function(input) {
    # copy data
    in_data <- copy(input$data)
    # be verbose
    message('input data:')
    # check farm_id
    if (is.null(input$farm_id)) {
        # be verbose
        message('  1 farm')
        # assign farm id column
        in_data[, id := 1L]
    } else {
        # assign farm id
        in_data[, id := get(input$farm_id)]
        # number of ids
        fid <- in_data[, length(unique(id))]
        # be verbose
        message('  ', fid, ' unique farm ID', if(fid > 1) 's')
    }
    # summarize instances by farm id
    in_data[instance != '', {
        # structure
        message('---')
        # key
        key <- setNames(value, instance)
        # animalcategory
        acat <- key[variable %chin% 'animalcategory']
        # animals
        num <- key[variable %chin% 'animals']
        # convert to numeric
        mode(num) <- 'numeric'
        # volume
        vol <- key[variable %chin% 'volume']
        # convert to numeric
        mode(vol) <- 'numeric'
        # contains cattle manure?
        mcattle <- key[variable %chin% 'contains_cattle_manure']
        # contains pig manure?
        mpig <- key[variable %chin% 'contains_pig_manure']
        # extend dairy_cows and fattening_pigs
        missed <- setdiff(unique(instance), unique(c(names(acat), names(vol))))
        for (nm in missed) {
            # check if dairy_cows or fp
            if ('milk_yield' %in% variable[instance == nm]) {
                acat <- c(acat, setNames('dairy_cows', nm))
            } else {
                acat <- c(acat, setNames('fattening_pigs', nm))
            }
        }
        # indicate farm id
        message('"', .BY[[1]], '":')
        # loop over livestock instances
        if (any(num > 0)) message('    livestock instances:')
        for (lsi in names(num[num > 0])) {
            message('      "', lsi, '": ', num[lsi], ' animals (', acat[lsi], ')')
        }
        # loop over tank instances
        if (any(vol > 0)) message('    slurry tanks:')
        for (sti in names(vol[vol > 0])) {
            message('      "', sti, '": ', vol[sti], ' m3 (cattle: ', mcattle[sti],', pig: ', mpig[sti],')')
        }
        # empty line
        if (any(c(num, vol) == 0)) {
            message('')
            # add 'empty' animal cats
            if (any(num == 0)) {
                message('        -> instances with 0 animals: ', paste(names(num)[num == 0], collapse = ','))
            }
            # add 'empty' animal cats
            if (any(vol == 0)) {
                message('        -> tanks with volume == 0: ', paste(names(vol)[vol == 0], collapse = ','))
            }
        }
    }, by = id]
    # be verbose
    message('---')
}

#' Check Report Argument
#'
#' check report argument and return corresponding 'print-only' labels
#'
#' @param report string. Name of the report to generate.
#' @return a vector of 'print-only' labels
check_report <- function(report) {
    # define valid reports 
    #   -> see End.nhd for valid reports incl. print-only
    #       labels
    valid <- list(
        summary = c(
                'SummaryLivestock', 
                'SummaryPlantProduction',
                'SummaryTotal'
                ),
        detailed = c(
            'LivestockNH3',
            'PlantNH3',
            'SummaryTotal',
            'LivestockN2O',
            'LivestockNO',
            'LivestockN2',
            'LivestockNtot',
            'LivestockTAN'
            ),
        full = '',
        NH3 = c(
            'LivestockNH3',
            'PlantNH3',
            'SummaryTotal'
            ),
        TAN = 'LivestockTAN',
        N = c(
            'LivestockN2O',
            'LivestockNO',
            'LivestockN2',
            'LivestockNtot'
            ),
        HAFL = c(
            'LivestockNH3',
            'PlantNH3',
            'SummaryTotal',
            'LivestockTAN'
            )
    )
    # check argument report
    if (length(report) != 1 || !is.character(report) || 
        is.na(report <- pmatch(report, names(valid)))) {
        stop('argument "report" is not valid')
    }
    # return print-only
    valid[[report]]
}

# helper
check_token <- function(token) {
    if (is.null(token) && ((token <- Sys.getenv('AGRAMMON_TOKEN')) == '')) {
        stop('no token available')
    } else if (!is.character(token) || token == '') {
        stop('token not valid')
    }
    token
}

#' Check and Validate Agrammon Input
#'
#' check and validate Agrammon input
#'
#' @param dt a \code{data.table} of Agrammon input data
#' @return a list with entries 'input data', 'farm_id column', 'simulation column'
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
        dt[, farm_id_ := frank(factor(get(list_ids$farm_id), levels = unique(get(list_ids$farm_id))), ties.method = 'dense')]
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


#' Agrammon options
#'
#' return a list of valid Agrammon REST interface options
#'
#' @param show logical. If \code{TRUE} an explanatory summary on the 
#'          options that can be edited will be printed to the console. 
#'          Defaults to \code{FALSE}.
#' @param \dots agrammon REST interface options
#' @return a list of agrammon REST interface options
#' @examples
#' ## examples here
#' agrammon_options(show = TRUE)
#' agrammon_options(language = 'de', print = c('LivestockNH3', 'LivestockTAN'))
agrammon_options <- function(show = FALSE, ...) {
    # assign defaults
    defaults <- list(
        # can be changed:
        free = list(
            language = list(
                help = 'language in output', 
                values = c('en', 'de', 'fr')
                ),
            'print-only' = list(
                help = paste0('output subset(s) to return\n',
                    '    option "print-only" has preceedence over "report-selected"\n',
                    '    if neither "print-only" nor "report-selected" are set, \n',
                    '     all available model output will be returned'),
                values = c('', 
                    'SummaryTotal', 'SummaryLivestock', 'SummaryPlantProduction',
                    'ResultsTotal', 'ResultsLivestock', 'ResultsPlantProduction',
                    'LivestockNH3', 'PlantNH3',
                    'LivestockNtot', 'LivestockTAN',
                    'LivestockN2', 'LivestockNO', 'LivestockN2O'
                )
            ),
            'report-selected' = list(
                help = paste0('output report(s) to return\n',
                    '    option "print-only" has preceedence over "report-selected"\n',
                    '    if neither "print-only" nor "report-selected" are set, \n',
                    '     all available model output will be returned\n',
                    '    Reports "DetailReport", "DetailReportTAN", "DetailReportN" and "HAFLReport"\n',
                    '    are not available in variants == "Kantonal_LU"'),
                values = c('',
                    'Summary', 'DetailReport', 'DetailReportNH3', 'DetailReportTAN',
                    'DetailReportN', 'HAFLReport')
            ),
            'include-filters' = list(
                help = 'show each animal category seperately?',
                values = c('false', 'true')
            ),
            'all-filters' = list(
                help = 'show all exisiting animal categories? (only if include-filters = true)',
                values = c('false', 'true')
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
            cat('valid entries:\n', sp(7), '*"', 
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
    # define option names to loop over
    loop_options <- names(defaults[['free']])
    # add hidden option to change fixed
    if (isTRUE(dots[['change_fixed']])) {
        loop_options <- c(loop_options, names(defaults[['fixed']]))
    }
    # loop over free options
    for (fnms in loop_options) {
        if (fnms %in% names(dots)) {
            if (fnms == 'print-only' & length(dots[[fnms]]) > 1) {
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
