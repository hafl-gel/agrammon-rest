
# # TODO: check all cases!!!

#' Run Agrammon
#'
#' run the Agrammon model via its REST interface.
#'
#' @param input either the path to a CSV file containing model input data or
#'  the model input data provided as a \code{data.table} as produced by 
#'  the function \code{\link{create_dataset}}.
#' @param report string defining the content of the returned model result. 
#' Check the table in the vignette to get more information on valid entries. 
#' Partial matching of argument.
#' @param filter string defining the detail with which the result is split up by animal category.
#' Valid entries are \code{'total_only'}, \code{'existing_categories'} or \code{'all_categories')}.
#' Partial matching of argument.
#' @param language language for the units in the model result. Valid entries are \code{'en'},
#' \code{'de'} or \code{'fr'}. Defaults to English ('en')
#' @param data.table logical. should the result be returned as \code{data.table} 
#' (\code{TRUE}; default) or \code{data.frame} (\code{FALSE}).
#' @param token Agrammon REST-API access token.
#' @return result from an Agrammon model run
#' @export
#' @examples
#' ## examples here
#' # path to example input data set
#' path_example <- system.file('extdata', 'example_data_set_3farms.csv',
#'   package = 'agrammon')
#' # return default report and extract tan flow results
#' res <- run_agrammon(path_example)
#' report(res, 'tan')
#' # return report on NH3 losses and split up the result by all 
#' # animal categories that exist in the input data set
#' run_agrammon(path_example, report = 'nh3', filter = 'ex')
run_agrammon <- function(input, 
    report = getOption('agrammon.report', 'full'),
    filter = getOption('agrammon.filter', 'total_only'), 
    language = getOption('agrammon.language', 'en'), 
    data.table = getOption('agrammon.datatable', TRUE), 
    token = NULL, ...) {
    # check input
    if (!is.data.frame(input) && !is.character(input)) {
        stop('argument "input" has wrong format')
    }
    if (is.character(input) && !file.exists(input)) {
        stop('input file is not accessible')
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
        # valid filter arguments
        valid_filters <- c('total_only', 'existing_categories', 'all_categories')
        # check argument filter
        if (length(filter) != 1 || !is.character(filter) || 
            is.na(filter <- valid_filters[pmatch(filter, valid_filters)])
            ) {
            stop('argument "filter" is not valid')
        }
        # check 'include-filters' and 'all-filters'
        if (any(c('include-filters', 'all-filters') %in% names(dots))) {
            stop('either argument "filter" or arguments "include-filters"',
                ' and/or "all-filters" can be defined, but not both')
        }
        # assign filter
        filter <- switch(filter,
            total_only = c('false', 'false'),
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
    # run model
    run_model(input, model_options, check_token(token))
}

#' Set Agrammon Options
#'
#' set agrammon package related options
#'
#' This function is helping to set user specific defaults
#' on a R session basis. 
#'
#' If no argument is provided, the current user defined defaults
#' will be returned. 
#'
#' User specific defaults can be cleared by assigning a value of \code{NULL}.
#'
#' @param report set the default for argument \code{report} in function \code{run_agrammon}.
#' @param filter set the default for argument \code{filter} in function \code{run_agrammon}.
#' @param language set the language of the template help and units
#' @param data.table should \code{run_agrammon} return a \code{data.table} or a \code{data.frame}?
#' @return nothing unless no argument is provided (see Details)
#' @export
#' @examples
#' # get current user specific defaults
#' agrammon_defaults()
#' # set default to 'full' report
#' agrammon_defaults(report = 'full')
#' agrammon_defaults()
#' # remove it again
#' agrammon_defaults(report = NULL)
#' agrammon_defaults()
agrammon_defaults <- function(report, filter,
    language, data.table) {
    # check missing
    arg_list <- list(
        report = if (missing(report)) 'missing' else report,
        filter = if (missing(filter)) 'missing' else filter,
        language = if (missing(language)) 'missing' else language,
        data.table = if (missing(data.table)) 'missing' else data.table
    )
    arg_names <- names(arg_list) <- paste0('agrammon.', names(arg_list))
    arg_list <- arg_list[!unlist(lapply(arg_list, function(x) is.character(x) && length(x) == 1 && x == 'missing'))]
    if (length(arg_list) == 0) {
        # print current options
        return(
            do.call(options, as.list(arg_names))
        )
    }
    # set arguments
    do.call(options, arg_list)
}

#' title
#'
#' description
#'
#' @param input either an input file name or an input data set as provided by create_dataset()
#' @param model_options options provided to the Agrammon model which have influence on model output results or
#' @param token Agrammon REST-API access token. Can be provided as option entry 'agrammon.token'
#' @return  result from an Agrammon model run
#' @examples
#' run_model('./tests/inputs-version6-rest.csv')
#' run_model('./tests/inputs-version6-rest.csv', language = 'de')
run_model <- function(input, model_options = agrammon_options(), token = NULL) {
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
    # read input file
    if (!is.data.frame(input)) {
        raw_input <- fread(file = input, showProgress = FALSE, header = FALSE, data.table = TRUE)
    } else {
        raw_input <- as.data.table(input)
    }
    # help user
    message('validating input... ', appendLF = FALSE)
    # validate input
    valid_data <- check_and_validate(raw_input, token = token)
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
    # reorder columns
    setcolorder(res, c('farm_id', 'stage', 'variable_type', 'tracer', nms))
    # add unique cols
    if (!is.null(valid_data$unique_cols)) {
        res[, (valid_data$unique_cols) := valid_data$data[1, valid_data$unique_cols, with = FALSE]]
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
#' @examples
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
        x <- as.data.table(x)
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
    # return
    if (isdf) {
        as.data.frame(out)
    } else {
        out[]
    }
}

#' title
#'
#' description
#'
#' @param token Agrammon REST-API access token. Can be provided as option entry 'agrammon.token'
#' @return  result from an Agrammon model run
#' @export
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
        full = '',
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
        'nh3-loss' = c(
            'LivestockNH3',
            'PlantNH3',
            'SummaryTotal'
                ),
        'tan-flow' = 'LivestockTAN',
        'ntot-flow' = 'LivestockNtot',
        'nxox-loss' = c(
            'LivestockN2O',
            'LivestockNO',
            'LivestockN2'
            ),
        'n-balance' = c(
            'LivestockNH3',
            'PlantNH3',
            'LivestockN2O',
            'LivestockNO',
            'LivestockN2',
            'LivestockNtot'
            ),
        hafl = c(
            'LivestockNH3',
            'PlantNH3',
            'SummaryTotal',
            'LivestockTAN'
            )
    )
    # return names of report without arguments
    if (missing(report) || is.null(report)) return(names(valid))
    # check argument report
    if (length(report) != 1 || !is.character(report) || 
        is.na(report <- pmatch(tolower(report), names(valid)))) {
        stop('argument "report" is not valid. Valid report names:\n', paste(names(valid), collapse = ', '))
    }
    # return print-only
    valid[[report]]
}

#' Check Agrammon Access Token
#'
#' check and return a valid Agrammon access token
#'
#' @param token valid access token string of \code{NULL}
#' @return a valid access token string
check_token <- function(token = NULL) {
    if (is.null(token) && ((token <- Sys.getenv('AGRAMMON_TOKEN')) == '')
        && ((token <- getOption('agrammon.token', '')) == '')) {
        # try to read from permanent file
        environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")
        if (!file.exists(file.path(Sys.getenv("HOME"), ".Renviron"))) {
            stop('no token available')
        }
        # read file
        environ_lines <- readLines(environ_file)
        # find existing token entries and replace them
        token_line <- grep("AGRAMMON_TOKEN=", environ_lines, value = TRUE)
        if (!length(token_line)) stop('no token available')
        token <- sub('AGRAMMON_TOKEN=', '', token_line)
        Sys.setenv('AGRAMMON_TOKEN' = token)
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
check_and_validate <- function(dt, token = NULL) {
    # read input vars
    temp <- create_template(TRUE, token = token)
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
    mand_enums <- temp[!grepl(
        '^$|^between|^greater( or equal)? th(a|e)n|^smaller( or equal)? th(a|e)n', 
        remarks),
        unlist(strsplit(remarks, split = ',', fixed = TRUE)), by = module_var]
    # find module column and valid module entries
    valid_module <- dt[, {
        check_semicolon <- lapply(.SD, grepl, pattern = "::", fixed = TRUE)
        semicol <- names(.SD)[which.max(lapply(check_semicolon, sum))]
        I(list(semicol, check_semicolon[[semicol]]))
    }]
    # remove invalid rows in module column
    if (any(!valid_module[[2]])) {
        cat('Removing invalid module(s):\n')
        dt[which(!valid_module[[2]]), {
            cat(paste(V3, collapse = ', '), '\n')
        }]
        dt <- dt[which(valid_module[[2]]), ]
    }
    # find variable column
    nm_var <- dt[, {
        names(.SD)[which.max(lapply(.SD, function(x) sum(x %in% temp[, variable])))]
    }, .SDcols = setdiff(names(dt), valid_module[[1]])]
    # find value column (via mandatory entries)
    nm_val <- dt[, {
        names(.SD)[which.max(lapply(.SD, function(x) sum(x %in% mand_enums[, V1])))]
    }, .SDcols = setdiff(names(dt), c(valid_module[[1]], nm_var))]
    # rename columns
    setnames(dt, c(valid_module[[1]], nm_var, nm_val), c('module', 'variable', 'value'))
    # check NA in value
    if (dt[, anyNA(value)]) {
        dt[, 
            stop('input data set contains NA values!\n',
                '  -> check variables:\n     ', 
                paste(variable[is.na(value)], collapse = '\n     ')
            )
        ]
    }
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
        # get unique entries in num
        uniq_num <- unique(num)
        # throw error if an entry is missing
        if (0 %in% uniq_num) {
            ind <- 0 %in% num
            err_msg <- paste(unique(paste0('', module[ind], ' -> ', variable[ind], '\n')), collapse = '')
            # get max error message
            warn_length <- getOption('warning.length')
            # set max error 
            on.exit(options(warning.length = warn_length))
            options(warning.length = 8170)
            if (nchar(err_msg) > 8170) {
                err_msg <- paste0(strtrim(err_msg, 8170 - 60), '\n<error message truncated!>')
            }
            stop('Following mandatory input is missing:\n',
                err_msg
            )
        }
        if (length(uniq_num) > 1) {
            # get max
            n_max <- max(uniq_num)
            # check farm id in additional columns
            if (n_max > 1) {
                if (length(nms_extra_cols) == 0) {
                    # missing farm id
                    stop('A column containing the farm id is required\n',
                        '    if more than one farm is specified in the input!')
                }
                # -> check per module_var_ => no duplicated
                tab_mod_var <- table(module_var_)
                check_extra <- list(
                    overall = setNames(logical(length(nms_extra_cols)), nms_extra_cols),
                    dups = setNames(vector('list', length(nms_extra_cols)), nms_extra_cols),
                    fic = setNames(logical(length(nms_extra_cols)), nms_extra_cols)
                )
                for (check_col in nms_extra_cols) {
                    # get values
                    check <- get(check_col)
                    # check overall unique
                    check_extra[['overall']][[check_col]] <- uniqueN(check) > 1 && uniqueN(check) != .N
                    if (check_extra[['overall']][[check_col]]) {
                        # check unique per module_var_
                        check_extra[['dups']][[check_col]] <- sapply(names(tab_mod_var),
                            function(x) {
                                ind <- x == module_var_
                                uniqueN(check[ind]) == sum(ind)
                            })
                        # all unique?
                        check_extra[['fic']][[check_col]] <- all(check_extra[['dups']][[check_col]])
                    }
                }
                # get fic_ (farm id col) name (error if not found)
                num_fic <- sum(check_extra[['fic']])
                if (num_fic > 1) {
                    # multiple possible farm ids
                    stop('Multiple columns could serve as farm id column. Please check your input.')
                } else if (num_fic == 0) {
                    # no farm id column found
                    stop('farm id column is required but cannot be detected. Please check your farm id column!')
                }
                fic_ <- get(nms_extra_cols[check_extra[['fic']]])
                # check if farm id exists without mandatory input
                fall <- dt[, unique(get(nms_extra_cols[check_extra[['fic']]]))]
                # missing mandatory input for farm id
                fic_ <- c(fic_, setdiff(fall, fic_))
            } else {
                # only one data set
                fic_ <- rep('', .N)
            }
            # Error on new line
            cat('\n')
            # get max error message
            warn_length <- getOption('warning.length')
            # set max error 
            on.exit(options(warning.length = warn_length))
            options(warning.length = 8170)
            # get message
            err_msg <- missing_msg(num, fic_, module, variable, 50)
            if (nchar(err_msg) > 8170) {
                err_msg <- paste0(strtrim(err_msg, 8170 - 60), '\n<error message truncated!>')
            }
            # print error message
            stop(err_msg)
        } else if(uniq_num != 1) {
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
            if (any(nf <- nc == uniq_num)) {
                fic_ <- nms_extra_cols[nf]
                if (sum(nf) > 1) {
                    # farm id col not detectable
                    # could be solved by checking each set according to fic_ entries, but this is too much hassle...
                    stop('Multiple columns could serve as farm id column. Please check your input.')
                }
            } else {
                stop('farm id column is required but cannot be detected. Please check your farm id column!')
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
        list_ids$farm_id <- 'farm_id_'
    } else {
        dt[, farm_id_ := frank(factor(get(list_ids$farm_id), levels = unique(get(list_ids$farm_id))), ties.method = 'dense')]
    }
    # check mandatory - mandatory enums
    setnames(mand_enums, c('modvar', 'valid'))
    check_enums <- dt[module_var %chin% mand_enums[, unique(modvar)]][, {
        c(.SD, 
            list(
                check = value %chin% mand_enums[modvar == .BY[[1]], valid],
                farm_name = get(list_ids$farm_id)
                )
        )
    }, by = module_var]
    if (check_enums[, any(!check)]) {
        err_msg <- check_enums[!(check), {
            if (.N > 1) {
                paste0('Farm ID "', get(list_ids$farm_id)[1], '", ', .BY[[2]], ': variable ', .BY[[3]], ' has more than one entry!')
            } else {
                paste0('Farm ID "', get(list_ids$farm_id)[1], '": ', .BY[[2]], ';', .BY[[3]], ' is not valid -> ',
                    value, '\n  valid values are: \n  - ', 
                    mand_enums[modvar == module_var, paste(valid, collapse = '\n  - ')])
            }
        }, .(farm_name, module, variable)]
        wlen <- getOption('warning.length')
        on.exit(options(warning.length = wlen))
        options(warning.length = 8170)
        err_msg <- err_msg[, paste(V1, collapse = '\n\n')]
        if (nchar(err_msg) > 8170) {
            err_msg <- paste0(strtrim(err_msg, 8170 - 60), '\n<error message truncated!>')
        }
        stop('\n\n', err_msg)
    }
    # check limits 
    validator <- temp[grepl(
        '^between|^greater( or equal)? th(a|e)n|^smaller( or equal)? th(a|e)n', 
        remarks_), {
        # fix greater or equal than
        txt <- sub('greater or equal .*([-]?\\d) *$', '\\1 and Inf', remarks_)
        # fix between
        txt <- sub('between ', '', txt)
        # extract lower and upper
        .(
            module_var_,
            lower = as.numeric(sub(' *([-]?\\d+) and.*', '\\1', txt)),
            upper = as.numeric(sub('.*and ([-]?\\d+|Inf) *$', '\\1', txt))
        )
    }]
    check_vals <- merge(dt, validator, by.x = 'module_var', by.y = 'module_var_')[, 
        c('lower_notok', 'upper_notok') := {
            .(
                as.numeric(value) < lower,
                as.numeric(value) > upper
                )
    }]
    if (check_vals[, any(lower_notok | upper_notok)]) {
        err_msg <- check_vals[(lower_notok | upper_notok), {
            if (.N > 1) {
                paste0('Farm ID "', get(list_ids$farm_id)[1], '", ', .BY[[2]], ': variable ', .BY[[3]], ' has more than one entry!')
            } else {
                paste0('Farm ID "', get(list_ids$farm_id)[1], '": ', .BY[[2]], ';', .BY[[3]], 
                    ' -> value ', value, ' is ',
                    if (lower_notok) 'below lower' else 'above upper',
                    ' limit of ', if (lower_notok) lower else upper
                    )
            }
        }, by = .(farm_id_, module, variable)]
        wlen <- getOption('warning.length')
        on.exit(options(warning.length = wlen))
        options(warning.length = 8170)
        err_msg <- err_msg[, paste(V1, collapse = '\n\n')]
        if (nchar(err_msg) > 8170) {
            err_msg <- paste0(strtrim(err_msg, 8170 - 60), '\n<error message truncated!>')
        }
        stop('\n\n', err_msg)
    }
    # check mandatory - with instance
    temp_ins <- temp[(has_instance_)][default_ %chin% '', ]
    # get number per module
    temp_ins[, num_per_module := .N, by = module_]
    # get number of farms
    num_farms <- dt[, uniqueN(farm_id_)]
    # select check with instance
    check_ins <- na.omit(merge(temp_ins, dt, by.x = 'module_var_', by.y = 'module_var', all.x = TRUE), cols = 'farm_id_')[, {
        c(list(num = uniqueN(module_var_)), .SD)
    }, by = .(farm_id_, module)]
    # check missing/duplicated input
    if(check_ins[, any(num != num_per_module)]) {
        # missing entries
        dbg <- merge(temp_ins[, .(module_, module_var_)], 
            check_ins[num != num_per_module], all.x = TRUE)[, fid := get(list_ids$farm_id)]
        err_msg_dt <- dbg[, {
            if (!all(is.na(fid))) {
                em <- ''
                mv <- tstrsplit(module_var_, split = ';')
                tb <- table(fid, mv[[2]])
                rn <- row.names(tb)
                cn <- colnames(tb)
                nvars <- length(cn)
                for (i in seq_len(nrow(tb))) {
                    fi <- rn[i]
                    mod <- unique(module[which(fid == fi)])
                    # get unique instances
                    num_instances <- length(mod)
                    # add dummy farm for missing_msg to work!
                    em <- c(em, missing_msg(
                        number = tb[i, ],
                        farm_label = rep(fi, nvars),
                        module = rep(mod, nvars),
                        variable = cn,
                        single_check = TRUE
                        ))
                }
                em
            } else {
                NULL
            }
        }, by = .(module_)]
        wlen <- getOption('warning.length')
        on.exit(options(warning.length = wlen))
        options(warning.length = 8170)
        err_msg <- err_msg_dt[, paste(V1, collapse = '')]
        if (nchar(err_msg) > 8170) {
            err_msg <- paste0(strtrim(err_msg, 8170 - 60), '\n<error message truncated!>')
        }
        # print error message
        stop(err_msg)
    }
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
            wlen <- getOption('warning.length')
            on.exit(options(warning.length = wlen))
            options(warning.length = 8170)
            stop(
                'Entries not valid!\n',
                if (.N == 1) {
                    'The following input entry is not valid:\n'
                } else {
                    'The following input entries are not valid:\n'
                },
                paste0('Farm ID "', get(list_ids$farm_id), '": ', module, ' -> ', variable, '\n')
            )
        }
    }]
    # return
    c(list(data = dt), list_ids)
}

missing_msg <- function(number, farm_label, module, variable, width = 40, single_check = FALSE) {
    # get farm number
    farm_number <- uniqueN(farm_label)
    # get index
    ind <- which(number != farm_number)
    # get entries
    entries <- paste0(module[ind], ' -> ', variable[ind])
    # how many?
    tab_entries <- table(entries)
    if (single_check) {
        tab_entries[entries] <- number[ind]
    }
    # too few/many
    diff_entries <- tab_entries - farm_number
    # get signs
    sig_entries <- sign(diff_entries)
    # fix out
    if (single_check) {
        num_wrong <- rep('', length(diff_entries))
    } else {
        num_wrong <- abs(diff_entries)
    }
    # first line
    out <- paste0(
        switch(as.character(sum(unique(sig_entries))),
            '-1' = 'Missing ',
            '0' = 'Missing/Too many ',
            '1' = 'Too many '
        ), 'input variable entries!\n\n')
    # second line
    if (!single_check) {
        out <- c(out, paste0('  -> There are ', farm_number, ' input data sets (farms). '))
    }
    out <- c(out, 
        if (length(tab_entries) > 1) {
            'Check the following input variables:\n\n'
        } else {
            'Check the following input variable:\n\n'
        })
    # loop over unique entries
    for (i in seq_along(diff_entries)) {
        # which exist
        if (single_check) {
            farms <- ''
        } else {
            farms <- farm_label[ind[entries == names(tab_entries)[i]]]
        }
        # get missing/duplicates
        if (sig_entries[i] < 0) {
            all_farms <- unique(farm_label)
            print_farms <- paste(all_farms[!(all_farms %in% farms)], collapse = ', ')
        } else {
            print_farms <- paste(unique(farms[duplicated(farms)]), collapse = ', ')
        }
        # trim
        if (nchar(print_farms) > width) print_farms <- paste0(strtrim(print_farms, width), '...')
        # print variable
        out <- c(out, paste0('  ', names(tab_entries)[i], ': ', num_wrong[i], 
                if (sig_entries[i] < 0) ' missing (' else ' too many (',
                print_farms,
                ')\n'
                ))
    }
    # return
    paste(c(out, '\n'), collapse = '')
}

#' Agrammon Options
#'
#' return a list of valid Agrammon REST interface options
#'
#' @param show logical. If \code{TRUE} an explanatory summary on the 
#'          options that can be edited will be printed to the console. 
#'          Defaults to \code{FALSE}.
#' @param \dots agrammon REST interface options
#' @return a list of agrammon REST interface options
#' @examples
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
                help = paste0('output subset to return\n',
                    '     (default "": return full model output)'),
                values = c('', 
                    'SummaryTotal', 'SummaryLivestock', 'SummaryPlantProduction',
                    'ResultsTotal', 'ResultsLivestock', 'ResultsPlantProduction',
                    'LivestockNH3', 'PlantNH3',
                    'LivestockNtot', 'LivestockTAN',
                    'LivestockN2', 'LivestockNO', 'LivestockN2O'
                )
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

#' Extract Report
#'
#' extract report from Agrammon results
#'
#' @param x results from a call to run_agrammon with argument \code{report = 'full'}
#' @param report string defining the content of the returned model result. 
#' Partial matching of argument.
#' @return report based subset of results
#' @export
#' @examples
#' # path to example input data set
#' path_example <- system.file('extdata', 'example_data_set_3farms.csv',
#'   package = 'agrammon')
#' # run agrammon
#' res <- run_agrammon(path_example, report = 'full')
#' # return report on Ntot flow
#' report(res, 'ntot')
report <- function(x, report = 'nh3-loss') {
    # check on data.table
    if (is_df <- !is.data.table(x)) {
        x <- as.data.table(x)
    }
    # get names
    nms <- names(x)
    # check if x contains 'full' report
    if (x[, !('internal' %in% variable_type)]) stop('argument "x" must contain results from "full" report')
    # get available reports
    ok <- sub('.rds', '', dir(system.file('reports', package = 'agrammon')), fixed = TRUE)
    # partially match report argument
    report_matched <- pmatch(tolower(report), c('full', ok))
    # check
    if (anyNA(report_matched)) stop('argument "report" contains invalid or unrecognized report names.', 
        '\nValid report names are: ', paste0(check_report(), collapse = ', '))
    # get reports
    report <- c('full', ok)[report_matched]
    # return if 'full'
    if ('full' %in% report) return(x)
    # get rds file(s)
    rps <- unique(rbindlist(lapply(report, function(r) {
            readRDS(system.file('reports', paste0(r, '.rds'), package = 'agrammon'))
    })))
    # merge with x
    out <- merge(rps, x, all.x = TRUE)
    # reorder columns
    setcolorder(out, nms)
    # fix values
    out[, c('value', 'value_num', 'value_chr') := list(value_num, NULL, NULL)]
    # sort rows
    stages_out <- c('Livestock', 'Storage', 'Application', 'PlantProduction', 'Total')
    out <- out[order(farm_id, match(stage, stages_out, nomatch = 999))]
    # return
    if (is_df) {
        as.data.frame(out)
    } else {
        out[]
    }
}

