
## 0. header ----------------------------------------

library(jsonlite)
library(data.table)

path_file <- 'tests/inputs.json'

## 1. read dump ----------------------------------------

dj <- read_json(path_file)


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

create_template <- function(livestock = list(), storage = NULL) {
    dump_all <- FALSE
    # check livestock
    if (!is.list(livestock) && !(dump_all <- is.logical(livestock) && livestock)) {
        stop('argument livestock must be a named list. Run create_template() to see valid options.')
    }
    # get input dump
    # inp_var <- read_input_vars(read_json('tests/inputs.json'))[, .(module, variable, enums, label, unit, instances, animal_cat, top_module)]
    inp_var <- read_input_vars(read_json('tests/inputs.json'))
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
    }
}

create_template()
create_template('test1')
create_template(list('test2a'))
create_template(list('test2a', dairy_cows = 'b'))
create_template(list(dc = 'test2b', dairy_cows = 'b'))
create_template(list(dc = 'test2b', bc = 'a', dairy_cows = 'b'))

