#' @title Send text with Telnyx
#'
#' @description
#' Send a text with the Telnyx API. Specifically to the \code{"https://api.telnyx.com/v2/messages"} endpoint
#' Note that using \code{tl_send_messaging_profile} is the preferred way of sending multiple SMS as it is more feature rich.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/messaging/}{here} for more details.
#'
#' @param to number to send the text to
#' @param from number to send the text from
#' @param message character string of the text
#' @param key Telnyx API key
#' @param params optional further params to pass into the API call. Should be a named list.
#'
#' @export
tl_send_text <- function(
  to,
  from,
  message,
  key,
  params = NULL
) {
  body <- list(
    to = to, from = from,
    text = message
  )
  r <- ..tl_postv2(
    body, ep = 'messages',
    key, params
  )
  return(r)
}


#' @title Send SMS via a Number Pool attached to Telnyx Messaging Profil
#'
#' @description
#' Send an SMS through the Number Pool Feature of Telnyx. Specifically to the \code{"https://api.telnyx.com/v2/messages/number_pool"} endpoint.
#' This is the recommended way of sending texts because it takes advantage of all the Messaging Profile features
#' Which include things like matching locale and sticky sender. You also don't need need to remember which phone number to use as the sending number.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/messaging/Messaging-Profiles}{here} for more details.
#'
#' @param to the number to send the message to
#' @param message the message to send
#' @param profile_id messaging profile id
#' @param key Telnyx API key
#' @param params optional further params to pass into the API call. Should be a named list.
#'
#' @export
tl_send_msg_profile <- function(
  to,
  message,
  profile_id,
  key,
  params = NULL
) {
  body <- list(
    to = to, text = message,
    messaging_profile_id = profile_id
  )
  r <- ..tl_postv2(
    body, ep = 'messages/number_pool',
    key, params
  )
  return(r)
}

#' @title Get SMS messages.
#'
#' @description
#' Get SMS messages from the  \code{"https://developers.telnyx.com/docs/api/v1/reports"} endpoint.
#' This reporting works as an asynchronous call to the report api and returns a CSV parsed into a data table using \code{fread}
#'
#' See \href{https://www.twilio.com/docs/sms/services}{here} for more details.
#'
#' @param start_time start time of report. Required. Must include the timezone.
#' @param end_time optional time for end of report. Must include the timezone.
#' @param profile_ids an optional vector of Messaging Profile ids to filter down to.
#' @param verbose defaults to FALSE.
#' @param v1_token Telnyx API v1 Token
#' @param v1_user Telnyx API v1 user, either an email or the preferred masked API user id.
#' @param params optional further arguments to pass into API query. Should be a named list.
#'
#' @export

tl_get_messages <- function(
  start_time,
  end_time    = NULL,
  profile_ids = NULL,
  verbose     = FALSE,
  v1_token,
  v1_user,
  params = NULL
) {

  body <- list(
    connections          = list(),
    directions           = list(),
    record_types         = list(),
    include_message_body = TRUE,
    start_time           = format(lubridate::with_tz(start_time, 'UTC'), '%Y-%m-%dT%H:%M:%S+00:00'),
    timezone             = 'UTC',
    profiles             = as.list(profile_ids)
  )

  if(!is.null(end_time)){
    body <- c(body, list(end_time = format(lubridate::with_tz(start_time, 'UTC'), '%Y-%m-%dT%H:%M:%S+00:00')))
  }

  # Queue the report
  r <- ..tl_postv1(
    body,
    ep = 'reporting/mdr_requests',
    v1_token,
    v1_user
  )

  # Retry it until its done
  id <- httr::content(r, encoding = 'UTF-8')[['id']]
  if(verbose){
    message(stringr::str_glue("Messages report queued for generation id: {id}"))
  }

  # Forcing a while loop here- not sure if I'm supposed to though lol
  status <- list()
  slp <- 0
  while(is.null(status[['report_url']])) {
    if(verbose){
      message(stringr::str_glue("Checking status of report: {id}"))
    }
    Sys.sleep(slp)
    r_status <- ..tl_getv1(
      ep = stringr::str_glue('reporting/mdr_requests/{id}'),
      v1_token,
      v1_user
    )
    status <- httr::content(r_status, encoding = 'UTF-8')
    slp <- 1
  }

  # Download report and parse dataframe
  r_report <- httr::GET(status[['report_url']], httr::write_memory())
  txt <- httr::content(r_report, as = 'text', encoding = 'UTF-8')
  txt <- stringr::str_replace_all(txt, ',\\"\\{', ",\\'{") # clean up quoting problem
  txt <- stringr::str_replace_all(txt, '\\}\\"\n', "\\}\\'\n")
  dt <- data.table::fread(txt, quote = "'")

  return(dt)
}

#' @describeIn POST to Telnyx API v2 (thats why its called v2)
#' @export
..tl_postv2 <- function(
  body,
  ep,
  apikey,
  p = NULL
) {
  authentication <- stringr::str_glue('Bearer {apikey}')
  resp <- httr::POST(
    url  = stringr::str_glue('https://api.telnyx.com/v2/{ep}'),
    body = jsonlite::toJSON(c(body, p), auto_unbox = T),
    httr::add_headers('Authorization' = authentication),
    httr::content_type_json()
  )
  httr::stop_for_status(resp)
  return(resp)
}

#' @describeIn POST to Telnyx API v1 (thats why its called v1)
#' @export
..tl_postv1 <- function(
  body,
  ep,
  token,
  user,
  p = NULL
) {
  resp <- httr::POST(
    url  = stringr::str_glue('https://api.telnyx.com/{ep}'),
    body = jsonlite::toJSON(c(body, p), auto_unbox = T),
    httr::add_headers(
      'x-api-token' = token,
      'x-api-user'  = user
    ),
    httr::content_type_json()
  )
  httr::stop_for_status(resp)
  return(resp)
}

#' @describeIn GET to Telnyx API v1 (thats why its called v1)
#' @export
..tl_getv1 <- function(
  ep,
  token,
  user,
  query = NULL
) {
  resp <- httr::GET(
    url  = stringr::str_glue('https://api.telnyx.com/{ep}'),
    httr::add_headers(
      'x-api-token' = token,
      'x-api-user'  = user
    ),
    query
  )
  httr::stop_for_status(resp)
  return(resp)
}


