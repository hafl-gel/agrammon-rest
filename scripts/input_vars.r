
# TODO: 
# 
# - add function to get input dump, args: format, sort, language, token

## 0. header ----------------------------------------

library(curl)
library(jsonlite)
library(data.table)

path_file <- 'tests/inputs.json'

## 1. read model dump (input variables) ----------------------------------------

#' get model dump
#'
#' helper function to get model dump via REST interface
#'
#' @param format ...
#' @return  
# curl  \
#      -H "Content-Type: multipart/form-data" \
#      -H "Accept: application/json" \
#      -H 'Authorization: Bearer agm_N2SR5yjT4ydTnjAw/9yd+vnd/BxLD8/wr5TMcgEwATnHzr+4V7mLkxxwYYM=' \
#    "https://model.agrammon.ch/single/test/api/v1/inputTemplate?sort=model&format=json&language=en"
# format=csv und format=text geht auch. sort kann model oder calculation sein und language kann de, en, fr sein.
get_input_template <- function(format = c('json', 'csv', 'text')[1], language = c('en', 'de', 'fr')[1], 
    sort = c('model', 'calculation')[1], token = getOption('agrammon.token')) {

    # check if curl is installed
    if (!requireNamespace('curl')) {
        stop('package "curl" is not available!\n\n', 
            '    install.packages("curl")\n\n')
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
    url <- sprintf("https://model.agrammon.ch/single/test/api/v1/inputTemplate?sort=%s&format=%s&language=%s",
        sort[1], format[1], language[1])

    # send request
    req <- curl_fetch_memory(url, handle = hdl)

    # convert to char
    char <- rawToChar(req$content)

    # parse answer and return
    switch(format[1],
        json = parse_json(char),
        csv = read.table(text = char, sep = ';', stringsAsFactors = FALSE),
        char
        )
}

#' process model dump
#'
#' helper function to create template from model dump
#'
#' @param x model dump as json
#' @return  
read_input_vars <- function(x, language = 'de', module = '') {
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
        out <- rbind(out, read_input_vars(x[[nm]], module = modules[nm]), use.names = TRUE, fill = TRUE)
    }
    out
}

dj <- get_input_template()

x <- read_input_vars(dj)
x[!is.na(hasDefaultFormula), .(module, variable, hasDefaultFormula)]
x[!is.na(default), .(module, variable, default)]

x <- read_input_vars(dj)[, .(module, variable, enums, label, unit, instances, animal_cat, top_module)]

x[!is.na(animal_cat)]
x[lengths(enums) > 0, ]

x[top_module %in% 'Storage']
x[animal_cat %in% 'DairyCow']


## 2. create user template ----------------------------------------

# NOTE:
#   - function with arguments
#       - livestock = list(dairy_cows = c('DC_1', 'DC_2'), heifers_1yr = c('H1_1', 'H1_2'), ...)
#       - storage = list('tank1', 'tank2')
#       - print.livestock = FALSE
#       - oder print.livestock = TRUE, falls empty arguments

create_template <- function(livestock = list(), storage = NULL, token = getOption('agrammon.token')) {
    dump_all <- FALSE
    # check livestock
    if (!is.list(livestock) && !(dump_all <- is.logical(livestock) && livestock)) {
        stop('argument livestock must be a named list. Run create_template() to see valid options.')
    }
    # get input dump
    # inp_var <- read_input_vars(read_json('tests/inputs.json'))[, .(module, variable, enums, label, unit, instances, animal_cat, top_module)]
    inp_var <- read_input_vars(get_input_template(token = token))
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
        # print valid livestock input
        cat('\n*** Valid list entry names for argument "livestock"\nproviding the animal category or its parent class:')
        liv[, {
            cat('\n  ', .BY[[1]], '\n      ')
            cat(paste(enums[[1]], collapse = '\n      '))
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
        sub('ge', 'greater or equal then', x)
    }]
    # build remarks from enum entries and validators
    inp_var[, c('remarks', 'remarks_help') := .(sapply(enums, paste, collapse = ','), enums_help_text)][
        !is.na(validator) & lengths(enums) == 0, remarks := validator]
    # create template^2
    top_sorted <- c('Livestock', 'Storage', 'Application', 'PlantProduction')
    inp_template <- inp_var[order(match(top_module, top_sorted), module), 
        .(module, variable, value = '', unit, label, remarks, help = remarks_help, instances, animal_cat)]
    if (dump_all) {
        # create line with note
        note <- inp_template[1, 1:7, with = FALSE][, lapply(.SD, function(x) "")]
        note[, c('module', 'variable') := list(
            'Note:', 
            paste(
                'Remove this line.',
                'Copy/Remove modules with instances as required for your input and',
                'replace all "INSTANCE_NAME" entries with corresponding instance names.'
                )
            )]
        # return all dummy instances
        return(rbind(note, inp_template[, 1:7, with = FALSE]))
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
            tmp <- inp_template[animal_cat %chin% nm, 1:7, with = FALSE]
        } else {
            # get parent category
            parent_cat <- cat_key[nm]
            # get template entries
            tmp <- inp_template[animal_cat %chin% parent_cat, 1:7, with = FALSE]
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
        warn <- inp_template[1, 1:7, with = FALSE][, lapply(.SD, function(x) "")]
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
        tmp <- inp_template[is.na(animal_cat) & instances, 1:7, with = FALSE]
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
    template <- rbind(template, inp_template[!(instances), 1:7, with = FALSE])
    # return unsorted template
    template
}

create_template(livestock = list(Equides = c('a' , 'b'), ponies_and_asses = c('c', 'd')))
x <- create_template(livestock = list(Equides = c('a' , 'b'), ponies_and_asses = c('c', 'd')), storage = c('tank1', 'tank2'))
# TODO: 1) check if character vectors! 2) check if all unique!

create_template()
create_template('test1')
create_template(list('test2a'))
create_template(list('test2a', dairy_cows = 'b'))
create_template(list(dc = 'test2b', dairy_cows = 'b'))
create_template(list(dc = 'test2b', bc = 'a', dairy_cows = 'b'))

