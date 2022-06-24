
## 0. header ----------------------------------------

library(data.table)

## 1. read model dump (input variables) ----------------------------------------

#' get model dump
#'
#' helper function to get model dump via REST interface
#'
#' @return  
#' @examples
#' mj <- get_input_template()
get_input_template <- function(format = c('json', 'csv', 'text')[1], language = c('en', 'de', 'fr')[1], 
    sort = c('model', 'calculation')[1], token = Sys.getenv('AGRAMMON_TOKEN')) {
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
    # check token
    if (!is.character(token) || token == '') {
        stop('not agrammon token available')
    }
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
#' helper function process model dump
#'
#' @param x model dump as json
#' @return  
#' @examples
#' mj <- get_input_template()
#' x <- read_input_vars(mj)
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


## 2. create user template ----------------------------------------

#' create template in R
#'
#' helper function to create template from model dump
#'
#' @param x model dump as json
#' @return  
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
create_template <- function(livestock = list(), storage = NULL, token = Sys.getenv('AGRAMMON_TOKEN')) {
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
    # check token
    if (!is.character(token) || token == '') {
        stop('not agrammon token available')
    }
    # get input dump
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
#' If argument \code{livestock} is \code{TRUE} ...
#' If \code{livestock} is a named list ...
#' If \code{livestock} is an empty list (default) ...
#' A valid \code{token} can be required from support@agrammon.ch
#'
#' @param file file path
#' @param livestock a named list or \code{TRUE}. The default is an empty list. See Details.
#' @param storage a character vector providing storage instance names (slurry tanks)
#' @param token token which will be used to perform the REST call.
#' @export
#' @return NULL
#' @examples
#' \dontrun{
#'   # "register" token -> wie?
#'   register_token(my_token)
#'   # save template to file
#'   save_template('model_template.csv', 
#'      livestock = list(Equides = c('Horses_1', 'Horses_2'), dairy_cows = 'DC'), storage = 'Tank_1')
#' }
save_template <- function(file, livestock = list(), storage = NULL, token = Sys.getenv('AGRAMMON_TOKEN')) {
    # check token
    if (!is.character(token) || token == '') {
        stop('not agrammon token available')
    }
    # get template
    out <- create_template(livestock, storage, token)
    # proceed only if out != null
    if (!is.null(out)) {
        # write to file
        fwrite(out, file, sep = ';')
    }
    # return null
    invisible()
}
