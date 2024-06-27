
#' get technical config parameters
#'
#' function to get parameters from the technical.cfg file via REST interface
#'
#' @return  technical config parameters as data.table
#' @examples
#' # get standard technical config file
#' tech <- get_tech_config()
get_tech_config <- function(file = 'technical.cfg', token = NULL) {
    # check if curl is installed
    if (!require('curl')) {
        stop('package "curl" is not available!\n\n', 
            '    install.packages("curl")\n\n')
    }
    if (!require('jsonlite')) {
        stop('package "jsonlite" is not available!\n\n', 
            '    install.packages("jsonlite")\n\n')
    }
    # check token
    token <- check_token(token)
    # create handle
    hdl <- curl::new_handle()
    # set request option to get:
    curl::handle_setopt(hdl, customrequest = 'GET')
    # add header part
    curl::handle_setheaders(hdl,
        'Content-Type' = 'multipart/form-data',
        'Authorization' = paste0('Bearer ', token = token),
        'Accept' = 'application/json'
        )
    # url
    url <- sprintf("%s/model/technical?technical=%s",
        Sys.getenv('agrammon_rest_url'), file[1])
    # send request
    req <- curl_fetch_memory(url, handle = hdl)
    # check return
    char <- check_request(req)
    # get lines
    char_lines <- trimws(strsplit(char, split = '\n')[[1]])
    # get sections
    char_secs <- grep('^\\+', char_lines)
    # get parameters
    char_pars <- grep('^[[:alpha:]]', char_lines)
    # get values
    out <- data.table(
        module = sub('^\\+', '', char_lines[char_secs[findInterval(char_pars, char_secs)]]),
        parameter = sub('\\s.*', '', char_lines[char_pars]),
        value = sub('\\S+\\s+=\\s+', '', char_lines[char_pars])
    )
    # get top module
    out[, top_module := sub(':.*', '', module)]
    # get sub module
    out[, sub_module := sub('^[^:]+::', '', module)]
    # set column order
    setcolorder(out, c('top_module', 'sub_module', 'module', 'parameter', 'value'))
    # add numeric values
    suppressWarnings(out[, value_num := as.numeric(value)])
    # return
    out[]
}

#' get model dump
#'
#' helper function to get model dump via REST interface
#'
#' @return  an input template as a string
#' @examples
#' mj <- get_input_template()
get_input_template <- function(format = c('json', 'csv', 'text')[1], language = c('en', 'de', 'fr')[1], 
    sort = c('model', 'calculation')[1], token = NULL) {
    # check if curl is installed
    if (!require('curl')) {
        stop('package "curl" is not available!\n\n', 
            '    install.packages("curl")\n\n')
    }
    if (!require('jsonlite')) {
        stop('package "jsonlite" is not available!\n\n', 
            '    install.packages("jsonlite")\n\n')
    }
    # create handle
    hdl <- curl::new_handle()
    # set request option to get:
    curl::handle_setopt(hdl, customrequest = 'GET')
    # add header part
    curl::handle_setheaders(hdl,
        'Content-Type' = 'multipart/form-data',
        'Authorization' = paste0('Bearer ', token = token),
        'Accept' = 'application/json'
        )
    # url
    url <- sprintf("%s/inputTemplate?sort=%s&format=%s&language=%s",
        Sys.getenv('agrammon_rest_url'), sort[1], format[1], language[1])
    # send request
    req <- curl_fetch_memory(url, handle = hdl)
    # check return
    char <- check_request(req)
    # parse answer and return
    switch(format[1],
        json = parse_json(char),
        csv = read.table(text = char, sep = ';', stringsAsFactors = FALSE),
        char
        )
}


#' Check Agrammon Return Value
#'
#' check the returned content of the Agrammon REST call (POST request)
#'
#' @param req a list object returned by the call to Agrammon REST interface
#' @return the Agrammon result as a string 
check_request <- function(req) {
    # convert to char
    char <- rawToChar(req$content)
    # return if ok
    if (req$status == 200) return(char)
    # capture request (http) errors
    cat('\n')
    switch (paste0('code_', char)
        # success
        , 'code_' = stop('http request status ', req$status, call. = FALSE)
        # otherwise
        , stop('http request - ', sub('.*<title>(.*)</title>.*', '\\1', char), call. = FALSE)
        )
}

#' process model dump
#'
#' helper function to process model dump
#'
#' @param x model dump as json
#' @return fix me
#' @examples
#' mj <- get_input_template()
#' x <- read_input_vars(mj)
read_input_vars <- function(x, language = 'en', module = '') {
    nms <- names(x)
    # capture instances
    if ('instances' %in% nms) {
        module <- paste0(module, '[INSTANCE_NAME]')
    }
    nms <- setdiff(nms, 'instances')
    if (module == '') {
        modules <- nms
    } else {
        modules <- paste(module, nms, sep = '::')
    }
    names(modules) <- nms
    out <- NULL
    if ('inputs' %in% nms) {
        y <- x[['inputs']]
        # loop over list
        for (i in seq_along(y)) {
            # extract enums
            if ('enums' %in% names(y[[i]])) {
                enum_index <- which(names(y[[i]]) %in% 'enums')
                enums <- y[[i]][['enums']]
                tmp <- cbind(
                    as.data.table(y[[i]][-enum_index]),
                    enums = list(names(enums)),
                    enums_help_text = list(sapply(enums, '[[', language))
                    )
            } else {
                tmp <- as.data.table(y[[i]])
            }
            # add instance
            tmp[, instances := grepl('[INSTANCE_NAME]', module, fixed = TRUE)]
            # get animal category and slurry phase
            if (grepl('Livestock', module, fixed = TRUE)) {
                tmp[, animal_cat := sub('Livestock::([a-zA-Z]*)[[].*', '\\1', module)]
            }
            # add top module
            tmp[, top_module := sub('^([a-zA-Z]*)::.*', '\\1', module)]
            # bind to out
            out <- rbind(out, cbind(module = module, tmp), fill = TRUE, use.names = TRUE)
        }
        # subtract 'inputs' from names
        nms <- setdiff(nms, 'inputs')
    }
    # loop over sub-modules
    for (nm in nms) {
        out <- rbind(out, read_input_vars(x[[nm]], language = language, module = modules[nm]), use.names = TRUE, fill = TRUE)
    }
    out
}


#' create template in R
#'
#' helper function to create template from model dump
#'
#' @param x model dump as json
#' @return a data.table containing all agrammon input variables
#' @examples
#'create_template(livestock = list(Equides = c('a' , 'b'), ponies_and_asses = c('c', 'd')))
#'x <- create_template(livestock = list(Equides = c('a' , 'b'), ponies_and_asses = c('c', 'd')), storage = c('tank1', 'tank2'))
#'
#'create_template()
#'create_template(TRUE)
#'create_template(FALSE)
#'create_template('test1')
#'create_template(list('test2a'))
#'create_template(list('test2a', dairy_cows = 'b'))
#'create_template(list(dc = 'test2b', dairy_cows = 'b'))
#'create_template(list(dc = 'test2b', bc = 'a', dairy_cows = 'b'))
create_template <- function(livestock = list(), storage = NULL, 
    language = c('en', 'de', 'fr')[1], token = NULL) {
    dump_all <- FALSE
    # check livestock
    if (!is.list(livestock) && !(dump_all <- is.logical(livestock) && livestock)) {
        stop('argument livestock must be either a named list or TRUE (= writing all instances).\n',
            '  Run create_template() for a list of valid list entry names.')
    }
    # check entries livestock
    if (!is.logical(livestock) && length(livestock) > 0) {
        # check for single values or vectors of type character 
        if (!all(sapply(livestock, is.character))) {
            stop('list entries (instance labels) in argument livestock must be single values or vectors of type character!')
        }
        # check != ''
        if (any(unlist(lapply(livestock, '==', '')))) {
            stop('list entries (instance labels) in argument livestock cannot contain empty (i.e."") characters!')
        }
        # check all unique
        if (any(duplicated(unlist(livestock)))) {
            stop('list entries (instance labels) in argument livestock cannot contain duplicated entries!')
        }
    }
    # check entries storage
    if (!is.null(storage)) {
        # warn if is.logical livestock
        if (is.logical(livestock)) warning('writing all instances, ignoring argument storage!')
        # check for single value or vector of type character 
        if (!is.character(storage)) {
            stop('argument storage must be a single value or a vector of type character!')
        }
        # check != ''
        if (any(storage == '')) {
            stop('argument storage cannot contain empty (i.e. "") characters as instance names!')
        }
        # check all unique
        if (any(duplicated(storage))) {
            stop('argument storage cannot contain duplicated instance names!')
        }
    }
    # get input dump
    inp_var <- read_input_vars(get_input_template(language = language[1], token = token), 
        language = language[1])
    # check names of livestock
    liv <- inp_var[top_module %in% 'Livestock'][variable %in% 'animalcategory'][order(animal_cat)]
    animal_cats <- liv[, c(unlist(enums), animal_cat)]
    nms_liv <- names(livestock)
    # if names don't match - print valid
    if (!dump_all && (length(livestock) == 0 || is.null(nms_liv) || any(!(nms_liv %in% animal_cats)))) {
        # print invalid livestock input
        if (length(livestock) > 0) {
            # unnamed
            if (is.null(nms_liv) || '' %in% nms_liv) {
                warning('Unnamed list entries in argument "livestock" are invalid!', immediate. = TRUE)
                nms_liv <- setdiff(nms_liv, '')
            }
            # wrong names
            if (length(nms_liv) > 0 && any(wn <- !(nms_liv %in% animal_cats))) {
                if (sum(wn) > 1) {
                    warning('list entry names "', paste(nms_liv[wn], collapse = '", "'),'" in argument "livestock" are invalid!', immediate. = TRUE)
                } else {
                    warning('list entry name "', nms_liv[wn],'" in argument "livestock" is invalid!', immediate. = TRUE)
                }
            }
        }
        # sort to print in order
        liv <- liv[order(match(animal_cat, 
                c('DairyCow', 'OtherCattle', 'Pig', 'FatteningPigs', 'Poultry', 
                    'Equides', 'SmallRuminants', 'RoughageConsuming')))]
        # print valid livestock input
        cat('\n***\nValid list entry names for argument "livestock".\nAnimal categories with corresponding parent class:\n***')
        liv[, {
            cat('\n  ', .BY[[1]], '\n      ')
            cat(paste(sort(enums[[1]]), collapse = '\n      '))
        }, by = animal_cat]
        cat('\n***\n\n')
        # return from function
        return(invisible(NULL))
    }
    ### start template here
    # human readable validator
    inp_var[!is.na(validator), validator := {
        x <- sub('[(]', ' ', validator)
        x <- gsub('[);]', '', x)
        x <- sub('[,]', ' and ', x)
        sub('ge', 'greater or equal than', x)
    }]
    # set default NA to empty char ''
    inp_var[is.na(default), default := '']
    # build remarks from enum entries and validators
    inp_var[, c('remarks', 'remarks_help') := .(sapply(enums, paste, collapse = ','), sapply(enums_help_text, paste, collapse = ','))][
        !is.na(validator) & lengths(enums) == 0, remarks := validator]
    # create template^2
    top_sorted <- c('Livestock', 'Storage', 'Application', 'PlantProduction')
    inp_template <- inp_var[order(match(top_module, top_sorted), module), 
        .(module, variable, value = '', unit, label, remarks, default, help = remarks_help, instances, animal_cat)]
    if (dump_all) {
        # create line with note
        note <- inp_template[1, module:help, with = FALSE][, lapply(.SD, function(x) "")]
        note[, c('module', 'variable') := list(
            'Note:', 
            paste(
                'Remove this line.',
                'Copy/Remove modules with instances as required for your input and',
                'replace all "INSTANCE_NAME" entries with corresponding instance names.'
                )
            )]
        # return all dummy instances
        return(rbind(note, inp_template[, module:help, with = FALSE]))
    }
    ### livestock instances
    # create cat key
    cat_key <- liv[variable %chin% 'animalcategory', {
        setNames(rep(animal_cat, lengths(enums)), unlist(enums))
    }]
    template <- NULL
    for (nm in nms_liv) {
        # check if nm is parent class
        if (liv[, nm %chin% animal_cat]) {
            # get template entries
            tmp <- inp_template[animal_cat %chin% nm, module:help, with = FALSE]
        } else {
            # get parent category
            parent_cat <- cat_key[nm]
            # get template entries
            tmp <- inp_template[animal_cat %chin% parent_cat, module:help, with = FALSE]
            # set value of animalcategory
            tmp[variable %chin% 'animalcategory', value := nm]
        }
        # loop over common livestock entries
        last_instance <- '[INSTANCE_NAME]'
        for (instance_name in livestock[[nm]]) {
            # add brackets
            repl <- paste0('[', instance_name, ']')
            # set instance name
            tmp[, module := sub(last_instance, repl, module, fixed = TRUE)]
            # add to existing template
            template <- rbind(template, tmp)
            # update last instance name
            last_instance <- repl
        }
    }
    ### storage instances
    # warning if no storage has been defined?
    if (is.null(storage)) {
        warning('No storage instances (slurry tanks) have been provided!',
        '\nAdding line with Warning to output...')
        # create line with warning
        warn <- inp_template[1, module:help, with = FALSE][, lapply(.SD, function(x) "")]
        warn[, c('module', 'variable') := list(
            'Warning!', 
            paste(
                'No storage instances (slurry tanks) have been provided!',
                'Remove this line if your input does indeed not include slurry storage.',
                'Otherwise, re-create this template with a storage instance (e.g. storage = "tank1")'
                )
            )]
        # add to template
        template <- rbind(warn, template)
    } else {
        # get template entries
        tmp <- inp_template[is.na(animal_cat) & instances, module:help, with = FALSE]
        # loop over storage instances
        last_instance <- '[INSTANCE_NAME]'
        for (instance_name in storage) {
            # add brackets
            repl <- paste0('[', instance_name, ']')
            # set instance name
            tmp[, module := sub(last_instance, repl, module, fixed = TRUE)]
            # add to existing template
            template <- rbind(template, tmp)
            # update last instance name
            last_instance <- repl
        }
    }
    ### residual input without instance
    template <- rbind(template, inp_template[!(instances), module:help, with = FALSE])
    # return unsorted template
    template
}


#' save template to file
#'
#' save a csv file which can be used as template for model input
#'
#' @details
#' If \code{livestock} is an empty list (default), all available animal
#' categories including their parent classes are printed to the console.
#'
#' If argument \code{livestock} is \code{TRUE}, entries for all available
#' parent classes will be written to the template file \code{file}.
#'
#' If \code{livestock} is a named list, all defined instances in the list will be
#' written to the template file |code{file}. The list entry names must
#' correspond to a valid animal category or parent class, whereas the list entries
#' (character vectors) will provide the instance names of the corresponding animal
#' category (or parent class) (see Examples).
#'
#' If \code{storage} is NULL, the first line of the template will contain a warning.
#'
#' A valid \code{token} can be requested from support@agrammon.ch
#'
#' @seealso [register_token()] for registering your personal access token, [run_agrammon()]
#' for calling the Agrammon model with a valid input data set.
#' @param file file path
#' @param livestock a named list or \code{TRUE}. The default is an empty list. See Details.
#' @param storage a character vector providing storage instance names (slurry tanks)
#' @param language the language that is used to provide help on input variables. Defaults to 
#' \code{'en'}.
#' @param token token which will be used to perform the REST call.
#' @export
#' @return NULL
#' @examples
#' \dontrun{
#'   # register token permanently
#'   register_token(my_token)
#'
#'   # save template to file
#'   save_template('model_template.csv', livestock = 
#'      list(Equides = c('Horses_1', 'Horses_2'), dairy_cows = 'DC'), 
#'      storage = 'Tank_1')
#' }
save_template <- function(file, livestock = list(), storage = NULL, 
    language = c('en', 'de', 'fr')[1], token = NULL) {
    # get template
    out <- create_template(livestock, storage, language[1], check_token(token))
    # proceed only if out != null
    if (!is.null(out)) {
        # replace secondary separator in columns remark & help
        out[grep(',', remarks, fixed = TRUE), c('remarks', 'help') := 
            lapply(.SD, gsub, pattern = ',', replacement = ' / ', 
                fixed = TRUE), 
            .SDcols = c('remarks', 'help')]
        # set default primary separator
        primary <- ','
        # check list separator on Windows
        if (.Platform[['OS.type']] == 'windows') {
            # get registry entry
            sList <- try(system('reg query "HKEY_CURRENT_USER\\Control Panel\\International" /v sList', intern = TRUE))
            if (!inherits(sList, 'try-error')) {
                # get entry
                sep_line <- grep('sList', sList, fixed = TRUE, value = TRUE)
                # extract last char
                primary <- sub('.* (\\S)$', '\\1', sep_line)
            }
        }
        # write to file
        if (language[1] != 'en' && .Platform[['OS.type']] == 'windows') {
            # fix encoding for extended latin characters
            enc <- "latin1"
            f <- file(file, open = 'w', encoding = enc)
            write.table(out, f, sep = primary, quote = FALSE, fileEncoding = enc)
            close(f)
        } else {
            fwrite(out, file, sep = primary, quote = FALSE)
        }
    }
    # return null
    invisible()
}

#' create an input data set
#'
#' create a valid input data set as R object (data.table)
#'
#' @export
#' @details
#' create new data set
#' input for livestock animal categories: animal-category-label = list(animalcategory = ., ...)
#'   -> mandatory list entries: "animals" (number of animals)
#' input for slurry storage tanks: tank-label = list(...)
#'   -> mandatory list entries: "volume", "depth" and possibly "contains_cattle_manure"/"contains_pig_manure"
#' general input is provided as list without an argument name
#' run get_variable() to get an overview on all possible list entries
create_dataset <- function(..., full_output = FALSE, data = NULL,
    dairy_cows = list(
        amount_summer = 1.5, 
        amount_winter = 2.5, 
        # shares according to personal communication with Thomas Kupper
        share_hay_summer = 100, 
        share_maize_silage_summer = 0, 
        share_maize_pellets_summer = 0, 
        share_maize_silage_winter = 100, 
        share_grass_silage_winter = 100, 
        share_maize_pellets_winter = 0, 
        share_potatoes_winter = 0, 
        share_beets_winter = 0, 
        milk_yield = 7500
    ),
    pigs = list(
        energy_content = 'defaults',
        crude_protein = 'defaults',
        feeding_phase_1_crude_protein = 151,
        feeding_phase_2_crude_protein = 151,
        feeding_phase_3_crude_protein = 151 
    ),
    livestock_general = list(
        housing_type = 'defaults', 
        mitigation_housing_floor = 'none', 
        # pigs
        air_scrubber = 'none',
        mitigation_housing_air = 'none',
        # poultry
        free_range = 'defaults',
        manure_removal_interval = 'defaults',
        drinking_system = 'defaults',
        # yard
        yard_days = 'defaults', 
        yard_hours = 'defaults',
        exercise_yard = 'available_roughage_is_not_supplied_in_the_exercise_yard', 
        floor_properties_exercise_yard = 'defaults',
        grazing_days = 'defaults', 
        grazing_hours = 'defaults'
    ),
    storage_liquid = list(
        mixing_frequency = '7_to_12_times_per_year',
        cover_type = 'solid_cover'
    ),
    storage_solid = list(
        share_applied_direct_poultry_manure = 12,
        share_covered_basin = 70,
        share_applied_direct_cattle_other_manure = 25,
        share_covered_basin_cattle_manure = 0,
        share_applied_direct_pig_manure = 0,
        share_covered_basin_pig_manure = 0
    ),
    application_liquid = list(
        dilution_parts_water = 1,
        appl_rate = 25,
        fermented_slurry = 0,
        appl_summer = 44,
        appl_autumn_winter_spring = 56,
        appl_evening = 20,
        appl_hotdays = 'sometimes',
        share_splash_plate = 0,
        share_trailing_hose = 100,
        share_trailing_shoe = 0,
        share_shallow_injection = 0,
        share_deep_injection = 0
    ),
    application_solid = list(
        incorp_lw1h = 0,
        incorp_lw4h = 0,
        incorp_lw8h = 0,
        incorp_lw1d = 0,
        incorp_lw3d = 0,
        incorp_gt3d = 0,
        incorp_none = 100,
        appl_summer = 30,
        appl_autumn_winter_spring = 70
    ),
    mineral_fertiliser = list(),
    recycling_fertiliser = list(),
    token = NULL
    ) {
    # bind arguments to single list
    all_dots <- list(...)
    nms_dots <- names(all_dots)
    # get all arguments
    matched_call <- as.list(match.call(expand.dots = FALSE))[-(1:2)]
    arg_list <- lapply(matched_call, eval)
    if (length(c(all_dots, arg_list)) == 0) {
        frmls <- formals()[4:10]
        cat('*** Default Input Arguments ***\n')
        for (f in names(frmls)) {
            cat('~~~\n')
            cat('* ', f, ':\n', sep = '')
            fl <- eval(frmls[[f]])
            for (a in names(fl)) {
                if (fl[[a]] == 'defaults') {
                    cat('   ', a, '=> *get', fl[[a]], 'from agrammon*\n')
                } else {
                    cat('   ', a, '=', fl[[a]], '\n')
                }
            }
        }
        return(invisible())
    }
    if (is.null(data)) {
        # check token
        token <- agrammon:::check_token(token)
        if (is.null(nms_dots)) stop('arguments must contain at least one animal category provided as label = list(animalcategory = ., ...)')
        # check named arguments
        has_label <- nms_dots != ''
        is_animalcat <- NULL
        ac <- NULL
        is_storage <- NULL
        # get all valid categories
        valid_categories <- get_categories(token = token)
        for (i in which(has_label)) {
            nms <- names(all_dots[[i]])
            if ('animalcategory' %in% nms) {
                # check for animalcategory
                is_animalcat <- c(is_animalcat, i)
                current_ac <- all_dots[[i]][['animalcategory']]
                # check if valid
                if (!(current_ac %in% unlist(valid_categories))) {
                    stop('label "', nms_dots[i], '": "', current_ac, '" is not a valid animal category!\n',
                        '  -> run get_categories() for valid animal categories')
                }
                # check if animals exists
                if (!('animals' %in% nms)) {
                    stop('label "', nms_dots[i], '": "animals" (number of animals) must be supplied!')
                }
                # remove animalcategory entry
                all_dots[[i]][['animalcategory']] <- NULL
                # append
                ac <- c(ac, current_ac)
            } else if (is.null(nms) || nms[1] == '') {
                # check for unnamed first
                is_animalcat <- c(is_animalcat, i)
                current_ac <- all_dots[[i]][[1]]
                # check if valid
                if (!(current_ac %in% unlist(valid_categories))) {
                    stop('label "', nms_dots[i], '": "', current_ac, '" is not a valid animal category!\n',
                        '  -> run get_categories() for valid animal categories')
                }
                # check if animals exists
                if (!('animals' %in% nms)) {
                    stop('label "', nms_dots[i], '": "animals" (number of animals) must be supplied!')
                }
                # remove animalcategory entry
                all_dots[[i]][[1]] <- NULL
                # append
                ac <- c(ac, current_ac)
            } else {
                is_storage <- c(is_storage, i)
                # check volume and depth
                for (what in c('volume', 'depth')) {
                    if (!(what %in% nms)) {
                        stop('label "', nms_dots[i], '": tank "', what, '" must be supplied!')
                    }
                }
            }
        }
        # check animal cats
        if (is.null(ac)) stop('arguments should contain at least one animal category provided as label = list(animalcategory = ., ...)')
        # prepare livestock
        livestock <- as.list(nms_dots[is_animalcat])
        names(livestock) <- ac
        # prepare storage labels
        storage <- nms_dots[is_storage]
        # check existing parents
        existing_parents <- lapply(names(valid_categories), 
            function(x) any(ac %in% valid_categories[[x]]))
        names(existing_parents) <- names(valid_categories)
        has_pigs <- any(unlist(existing_parents[c('FatteningPigs', 'Pig')]))
        has_cattle <- any(unlist(existing_parents[c('DairyCow', 'OtherCattle')]))
        # check if contains_* was provided
        for (nm in storage) {
            if (has_cattle && !has_pigs) {
                # only cattle
                all_dots[[nm]][['contains_cattle_manure']] <- 'yes'
                all_dots[[nm]][['contains_pig_manure']] <- 'no'
            } else if (!has_cattle && has_pigs) {
                # only pigs
                all_dots[[nm]][['contains_cattle_manure']] <- 'no'
                all_dots[[nm]][['contains_pig_manure']] <- 'yes'
            } else {
                nms <- names(all_dots[[nm]])
                # both cattle and pigs
                for (what in c('contains_cattle_manure', 'contains_pig_manure')) {
                    if (!(what %in% nms)) {
                        stop('label "', nms_dots[i], '": tank "', what, '" (yes/no) must be supplied!')
                    }
                }
            }
        }
        # get variables
        out <- get_variables(
            categories = ac, 
            livestock_labels = nms_dots[is_animalcat],
            storage_labels = nms_dots[is_storage],
            silent = TRUE,
            token = token
            )
        # add column with animal category
        out[, animal_category := '']
        out[top == 'Livestock', animal_category := value[variable == 'animalcategory'], by = cat_label]
        # fix defaults
        out[default != '', value := default]
        # add default arguments (storage, application, mineral)
        frmls <- formals()[4:10]
        default_args <- lapply(frmls, eval)
        defaults <- get_defaults()
        # loop and set defaults
        all_cats <- out[top == 'Livestock', unique(animal_category)]
        for (d_name in names(default_args)) {
            def <- default_args[[d_name]]
            if (length(def) != 0) {
                for (nm in names(def)) {
                    def_value <- def[[nm]]
                    # check value
                    if (def_value == 'defaults') {
                        def_list <- defaults[[nm]][all_cats]
                        l <- lengths(def_list)
                        if (any(l > 1)) {
                            # loop over yard_days & label
                            # ONLY occurs in livestock
                            out[top == 'Livestock', value := {
                                # copy value out
                                value_out <- value
                                # get list
                                def_sub <- def_list[[
                                    .BY[['animal_category']]
                                ]]
                                # search for housing type pattern
                                # (so far this is ONLY split by housing type)
                                # this might need to be adapted in later versions
                                if (length(def_sub) > 1) {
                                    value_out[variable %chin% nm] <- def_sub[sapply(names(def_sub), grepl,
                                        x = tolower(value[variable == 'housing_type']),
                                        fixed = TRUE
                                    )]
                                } else if (length(def_sub) != 0) {
                                    stop('Note to Dev: Fixme @ ~641')
                                    value_out[variable %chin% nm] <- def_sub
                                }
                                # return
                                value_out
                            }, by = .(cat_label, animal_category)]
                        } else if (!all(l == 0)) {
                            # get default values per category
                            def_cat <- unlist(def_list)
                            # set value
                            out[variable == nm & value == '', value := def_cat[animal_category]]
                        }
                    } else {
                        # set value
                        out[variable == nm & value == '', value := def[[nm]]]
                    }
                }
            }
        }
    } else {
        stop('not finished yet')
        out <- copy(data)
        if (is.null(nms_dots)) {
            nms_dots <- ''
        }
        if (!('cat_label' %in% names(out))) {
            out[, cat_label := sub('.*\\[([^]]+)\\].*', '\\1', module)]
            out[!grepl('[', module, fixed = TRUE), cat_label := '']
        }
    }
    # get names of arguments
    fnms <- names(formals())[c(6, 4:5, 7:10)]
    f_ind <- order(match(names(arg_list), fnms, nomatch = 0))
    # fix user provided arguments (except animal category and storage tank)
    for (nm in names(arg_list)[f_ind]) {
        # get pattern for correct modules
        grep_module <- switch(nm
            , livestock_general = 'Livestock'
            , dairy_cows = 'DairyCow'
            , pigs = 'Pig'
            , storage_liquid = 'Storage::Slurry'
            , storage_solid = 'Storage::SolidManure'
            , application_liquid = 'Application::Slurry'
            , application_solid = 'Application::SolidManure'
            , '.'
        )
        # loop over argument list
        for (var_name in names(arg_list[[nm]])) {
            # assign value
            out[grepl(grep_module, module) & variable == var_name, 
                value := arg_list[[nm]][[var_name]]
            ]
        }
    }
    # fix supplied animal category and storage tank arguments
    for (i in seq_along(all_dots)) {
        ac_label <- nms_dots[i]
        arg <- unlist(all_dots[[i]])
        # set values
        out[cat_label == ac_label & variable %chin% names(arg), value := arg[variable]]
    }
    # fix fix arguments
    # return
    if (full_output) {
        out
    } else {
        out[, .(module, variable, value)]
    }
}
# # create dataset
# xx <- create_dataset(
#     dc1 = list(animalcategory = 'dairy_cows', animals = 100),
#     dc2 = list('dairy_cows', animals = 1),
#     h1 = list('heifers_1st_yr', animals = 2),
#     eq = list('ponies_and_asses', animals = 1),
#     fp = list('fattening_pigs', animals = 50),
#     pig = list('boars', animals = 1),
#     sr = list('goats', animals = 5),
#     rc = list('red_deer', animals = 10),
#     py = list('broilers', animals = 5000),
#     tank1 = list(volume = 30, depth = 4, contains_cattle_manure = 'yes', contains_pig_manure = 'yes')
# )
# # extend data set
# yy <- create_dataset(list(compost = 5), data = xx)


# query animal categories from Agrammon

#' @export
get_categories <- function(parents = NULL, as_list = TRUE, token = NULL) {
    ds <- get_variables(categories = parents, token = token, silent = TRUE)
    if (as_list) {
        ds <- ds[variable == 'animalcategory', 
            unlist(lapply(strsplit(remarks, split = ','), sort)),
            by = second]
        out <- ds[, I(setNames(vector('list', uniqueN(second)), unique(second)))]
        for (p in names(out)) {
            out[[p]] <- ds[second == p, V1]
        }
        out
    } else {
        ds[variable == 'animalcategory', unlist(lapply(strsplit(remarks, split = ','), sort))]
    }
}
# get_categories()
# get_categories(c('Pig', 'FatteningPigs'))
# get_categories(c('Pig', 'FatteningPigs'), as_list = FALSE)

# helper function to obtain all default values which vary
#   between individual animal categories

#' @export
get_defaults <- function(x) {
    all <- list(
        # crude protein defaults
        crude_protein = list(
            nursing_sows = 164,
            dry_sows = 128,
            gilts = 151,
            boars = 128,
            weaned_piglets_up_to_25kg = 162
        ),
        energy_content = list(
            fattening_pigs = 14,
            nursing_sows = 13.9,
            dry_sows = 12.1,
            gilts = 14,
            boars = 12.1,
            weaned_piglets_up_to_25kg = 13.9
            ),
        # housing type from ProdTech 2019
        housing_type = list(
            dairy_cows = 'Loose_Housing_Slurry',
            suckling_cows = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            suckling_cows_lt600 = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            suckling_cows_gt700 = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            heifers_1st_yr = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            heifers_2nd_yr = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            heifers_3rd_yr = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            fattening_calves = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            calves_suckling_cows = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            beef_cattle = 'Loose_Housing_Slurry_Plus_Solid_Manure',
            fattening_pigs = 'Slurry_Label',
            nursing_sows = 'Slurry_Conventional',
            dry_sows = 'Slurry_Label',
            gilts = 'Slurry_Label',
            boars = 'Slurry_Label',
            weaned_piglets_up_to_25kg = 'Slurry_Conventional',
            growers = 'manure_belt_without_manure_belt_drying_system',
            layers = 'manure_belt_without_manure_belt_drying_system',
            broilers = 'deep_litter',
            turkeys = 'deep_litter',
            other_poultry = 'deep_litter'
        ),
        free_range = list(
            growers = 'no',
            layers = 'yes',
            broilers = 'no',
            turkeys = 'yes',
            other_poultry = 'yes'
        ),
        drinking_system = list(
            growers = 'drinking_nipples',
            layers = 'drinking_nipples',
            broilers = 'drinking_nipples',
            turkeys = 'bell_drinkers',
            other_poultry = 'bell_drinkers'
        ),
        manure_removal_interval = list(
            layers = 'more_than_4_times_a_month',
            growers = '3_to_4_times_a_month',
            broilers = 'no_manure_belt',
            turkeys = 'no_manure_belt',
            other_poultry = 'no_manure_belt'
        ),
        # grazing days defaults from Agrammon
        grazing_days = list(
            dairy_cows = 180,
            suckling_cows = 170,
            suckling_cows_lt600 = 170,
            suckling_cows_gt700 = 170,
            heifers_1st_yr = 140,
            heifers_2nd_yr = 170,
            heifers_3rd_yr = 160,
            fattening_calves = 0,
            calves_suckling_cows = 170,
            beef_cattle = 0,
            ponies_and_asses = 220,
            mules = 220,
            horses_older_than_3yr = 220,
            horses_younger_than_3yr = 220,
            milksheep = 220,
            fattening_sheep = 220,
            goats = 200,
            red_deer = 365,
            fallow_deer = 365,
            wapiti = 365,
            rabbit_young = 0,
            rabbit_doe_kits = 0,
            bison_older_than_3yr = 220,
            bison_younger_than_3yr = 220,
            lama_older_than_2yr = 220,
            lama_younger_than_2yr = 220,
            alpaca_older_than_2yr = 220,
            alpaca_younger_than_2yr = 220
        ),
        # grazing hours defaults from Agrammon
        grazing_hours = list(
            dairy_cows = 8.5,
            suckling_cows = 16,
            suckling_cows_lt600 = 16,
            suckling_cows_gt700 = 16,
            heifers_1st_yr = 12,
            heifers_2nd_yr = 17,
            heifers_3rd_yr = 15,
            fattening_calves = 0,
            calves_suckling_cows = 16,
            beef_cattle = 0,
            ponies_and_asses = 8,
            mules = 8,
            horses_older_than_3yr = 8,
            horses_younger_than_3yr = 8,
            milksheep = 15,
            fattening_sheep = 15,
            goats = 6,
            red_deer = 24,
            fallow_deer = 24,
            wapiti = 24,
            rabbit_young = 0,
            rabbit_doe_kits = 0,
            bison_older_than_3yr = 15,
            bison_younger_than_3yr = 15,
            lama_older_than_2yr = 15,
            lama_younger_than_2yr = 15,
            alpaca_older_than_2yr = 15,
            alpaca_younger_than_2yr = 15
        ),
        # yard days default values from Agrammon
        yard_days = list(
            dairy_cows = c(
                loose_housing = 270,
                tied_housing = 100
            ),
            suckling_cows_gt700 = c(
                loose_housing = 200,
                tied_housing = 145
            ),
            suckling_cows = c(
                loose_housing = 200,
                tied_housing = 145
            ),
            suckling_cows_lt600 = c(
                loose_housing = 200,
                tied_housing = 145
            ),
            heifers_1st_yr = c(
                loose_housing = 245,
                tied_housing = 100
            ),
            heifers_2nd_yr = c(
                loose_housing = 240,
                tied_housing = 105
            ),
            heifers_3rd_yr = c(
                loose_housing = 260,
                tied_housing = 110
            ),
            fattening_calves = c(
                loose_housing = 60,
                tied_housing = 0
            ),
            calves_suckling_cows = c(
                loose_housing = 200,
                tied_housing = 0
            ),
            beef_cattle = c(
                loose_housing = 160,
                tied_housing = 80
            ),
            ponies_and_asses = 220,
            mules = 220,
            horses_older_than_3yr = 250,
            horses_younger_than_3yr = 250
        ),
        # yard hours default values from Agrammon
        yard_hours = list(
            ponies_and_asses = 12,
            mules = 12,
            horses_older_than_3yr = 10,
            horses_younger_than_3yr = 10
        ),
        # yard floor property default values from Agrammon
        floor_properties_exercise_yard = list(
            dairy_cows = 'solid_floor',
            suckling_cows_gt700 = 'solid_floor',
            suckling_cows = 'solid_floor',
            suckling_cows_lt600 = 'solid_floor',
            heifers_1st_yr = 'solid_floor',
            heifers_2nd_yr = 'solid_floor',
            heifers_3rd_yr = 'solid_floor',
            fattening_calves = 'solid_floor',
            calves_suckling_cows = 'solid_floor',
            beef_cattle = 'solid_floor',
            ponies_and_asses = 'unpaved_floor',
            mules = 'unpaved_floor',
            horses_older_than_3yr = 'unpaved_floor',
            horses_younger_than_3yr = 'unpaved_floor'
        )
    )
    if (missing(x)) return(all)
    if (length(x) > 1) {
        all[x]
    } else {
        all[[x]]
    }
}

# - get template for all parents and list variables per parent

#' @export
get_variables <- function(categories = NULL, livestock_labels = NULL,
    storage_labels = 'tank1', silent = FALSE, token = NULL) {
    token <- agrammon:::check_token(token)
    # get all parents
    pcall <- capture.output(agrammon:::create_template(token = token))
    all_parents <- trimws(grep('^\\s+[A-Z]', pcall, value = TRUE))
    all_categories <- trimws(grep('^\\s+[a-z]', pcall, value = TRUE))
    pos_parents <- grep('^\\s+[A-Z]', pcall)
    pos_categories <- grep('^\\s+[a-z]', pcall)
    # get all parents if livestock is NULL
    if (is.null(categories)) {
        categories <- all_parents
    } else {
        # check type
        if (!length(categories) || !is.character(categories)) stop('argument categories is not valid')
        # check entries
        if (!all(ok <- categories %in% c(all_parents, all_categories))) {
            stop('invalid entries: ', 
                paste(categories[!ok], collapse = ', '),
                '\nValid parents are:\n',
                paste(all_parents, collapse = ', '),
                '\nValid categories are:\n',
                paste(all_categories, collapse = ', ')
                )
        }
    }
    # create livestock entry
    if (is.null(livestock_labels)) {
        livestock_labels <- categories
        for (lbl in unique(livestock_labels)) {
            livestock_labels[livestock_labels == lbl] <- paste0(lbl, '-', seq_len(sum(livestock_labels == lbl)))
        }
    } else if (length(livestock_labels) != length(categories)) {
        stop('arguments "livestock_labels" and "categories" must have equal lengths!')
    }
    livestock <- vector('list', uniqueN(categories))
    names(livestock) <- unique(categories)
    for (ca in names(livestock)) {
        livestock[[ca]] <- livestock_labels[categories == ca]
    }
    # fix numeric storage labels
    if (is.numeric(storage_labels)) {
        if (length(storage_labels) == 1) {
            storage_labels <- paste0('tank-', seq.int(storage_labels))
        } else {
            storage_labels <- paste0('tank-', storage_labels)
        }
    }
    # get template
    temp <- agrammon:::create_template(
        livestock = livestock,
        storage = storage_labels,
        token = token
        )
    # add module id column
    temp[, top_sec := sub('^([^:]+::[^:]+).*', '\\1', module)]
    # get label
    temp[, cat_label := sub('.*\\[([^]]+)\\].*', '\\1', top_sec)]
    temp[, has_label := grepl('[', top_sec, fixed = TRUE)]
    temp[!(has_label), cat_label := '']
    # remove label from top_sec
    temp[, top_sec_no_lab := sub('\\[.*', '', top_sec)]
    # get top module
    temp[, top := sub(':.*', '', top_sec)]
    # get second
    temp[, second := sub('.*::', '', top_sec_no_lab)]
    # loop over id column and print variables
    if (!silent) {
        temp[, {
            cat(
                '***\n',
                .BY[[1]], '\n',
                '  ~~ ', .BY[[2]], ':\n',
                if ('animalcategory' %in% variable) paste0(' <ANIMAL CATEGORY LABEL: "', .BY[[3]],'">\n'),
                if (.BY[[1]] == 'Storage' && .BY[[2]] == 'Slurry') paste0(' <TANK LABEL: "', .BY[[3]], '">\n'),
                sep = ''
            )
            for (i in seq_len(.N)) {
                cat(
                    '     - ', variable[i], ' \n', 
                    '         unit: "', unit[i], '"\n',
                    if (remarks[i] != '') paste0('         accepted values: "', remarks[i], '"\n'), 
                    if (tolower(help[i]) != gsub('_', ' ', remarks[i]) && help[i] != '') paste0('         help: "', help[i], '"\n'), 
                    if (default[i] != '') paste0('         default: "', default[i], '"\n'),
                    sep = ''
                )
            }
        }, by = .(top, second, cat_label)]
    }
    # return
    invisible(temp)
}
# x <- get_variables(c('DairyCow', 'dairy_cows', 'DairyCow'), storage_labels = 3)
# y <- get_variables(c('DairyCow', 'dairy_cows'), livestock_labels = c('DC-1', 'DC2'), storage_labels = 3)
# y <- get_variables(c('DairyCow', 'dairy_cows'), livestock_labels = c('DC-1', 'DC2'), storage_labels = 3)
# z <- get_variables()


