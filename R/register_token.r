#' Register token
#'
#' register an Agrammon REST interface access token for the current R session
#' or permanently
#'
#' @details
#' This function registers the \code{token} to access Agrammons REST interface in the 
#' current R session. 
#' If the argument \code{permanent} is \code{TRUE} (default), the 
#' provided \code{token} will be saved to the user Renviron file in the path 
#' \code{file.path(Sys.getenv("HOME"), ".Renviron")}.
#' The idea and the utilized code how to keep the token permanently is to a large part 
#' taken from the function \code{register_google} in the package \code{ggmap}.
#' 
#' Personal token: 
#' 
#' You can request a personal access token at support@agrammon.ch
#'
#' @seealso [save_template()], [run_agrammon()]
#'
#' @param token Agrammon REST-API access token.
#' @param permanent store token permanently or only for current session. See Details.
#' @export
#' @examples
#' \dontrun{
#'   # register token permanently
#'   register_token(my_token)
#'
#'   # register token for current session only
#'   register_token(my_token, permanent = FALSE)
#' }
register_token <- function(token, permanent = TRUE) {
    # check arguments
    if (missing(token)) stop('argument "token" missing')
    if (!is.character(token)) stop('argument "token" is not of type character')
    if (length(token) > 1) stop('argument "token" has length > 1 - you can only register one single token')
    if (!is.logical(permanent)) stop('argument "permanent" must be of type logical')
    # premanent registering?
    if (permanent) {
        # get Renviron path
        environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")
        if (!file.exists(file.path(Sys.getenv("HOME"), ".Renviron"))) {
            message(sprintf("Creating file %s", environ_file))
            file.create(environ_file)
        }
        # read file
        environ_lines <- readLines(environ_file)
        # find existing token entries and replace them
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
    }
    # register token for current session
    Sys.setenv(AGRAMMON_TOKEN = token)
    options(agrammon.token = token)
    # return null
    invisible()
}
