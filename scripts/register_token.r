
# TODO: replace option(agrammon.token) with Sys.getenv('AGRAMMON_TOKEN')
# register token in Renviron file (code thievery from ggmap::register_google)
register_token <- function(token, permanent = TRUE) {
    # check token
    if (!missing(token) && permanent) {
        # get Renviron path
        environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")
        if (!file.exists(file.path(Sys.getenv("HOME"), ".Renviron"))) {
            message(sprintf("Creating file %s", environ_file))
            file.create(environ_file)
        }
        environ_lines <- readLines(environ_file)
        token_line <- grep("AGRAMMON_TOKEN=", environ_lines)
        if (length(token_line) == 0) {
            message(sprintf("Adding token to %s", environ_file))
            environ_lines <- c(environ_lines, sprintf("AGRAMMON_TOKEN=%s", token))
            writeLines(environ_lines, environ_file)
        } else {
            old_key <- sub('AGRAMMON_TOKEN=(\\w+)', '\\1', environ_lines[token_line])
            if (old_key == token) {
                message("new key and old registered key are identical")
                return(invisible())
            }
            message(sprintf("Replacing old token (%s) with new token in %s", old_key, environ_file))
            environ_lines[token_line] <- sprintf("AGRAMMON_TOKEN=%s", token)
            writeLines(environ_lines, environ_file)
        }
        Sys.setenv(AGRAMMON_TOKEN = token)
    }
    else if (!missing(token) && !permanent) {
        Sys.setenv(AGRAMMON_TOKEN = token)
    }
    invisible()
}
