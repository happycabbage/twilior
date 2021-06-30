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
#' Get SMS messages from the MDR reports endpoint.
#' This reporting works as an asynchronous call to the report api and returns a CSV parsed into a data table using \code{fread}
#'
#' See \href{https://developers.telnyx.com/docs/api/v1/reports}{here} for more details.
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


#' @title Create or update a messaging profile
#'
#' @description
#' Create a Telnyx Messaging Profile via the Messgaging Profiles endpoint.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/messaging/Messaging-Profiles}{here} for more details.
#'
#' @param key Required. Telnyx API key
#' @param name Optional. A user friendly name for the messaging profile.
#' @param geomatch Should numbers match area codes? Defaults to TRUE.
#' @param skip_unhealthy Should numbers that are blocked not be used? Defaults to FALSE.
#' @param sticky_sender Should contacts receive messages from the same number? Defaults to TRUE.
#' @param long_code_weight Probability of using Long Code (as a share of the sum of the weights). Defaults to 1.
#' @param toll_free_weight Probability of using Toll Free (as a share of the sum of the weights). Defaults to 1.
#' @param whitelisted_destinations Vector of country codes numbers can send to. Defaults to 'US'. Should be ISO 3166-1 alpha-2 country codes.
#' @param webhook_url Optional. URL for webhook response.
#' @param webhook_failover_url Optional. Backup URL for webhook response.
#' @param profile_id The messaging profile to update. Required for \code{tl_update_msg_profile}.
#' @param params Optional further arguments to pass into API body. Should be a named list.
#'  Required if using \code{tl_update_msg_profile}, use this to pass in updated values as a named list.
#' @param ... Unused at this time.
#'
#' @export
tl_create_msg_profile <- function(
  key,
  name             = NULL,
  geomatch         = TRUE,
  skip_unhealthy   = FALSE,
  sticky_sender    = TRUE,
  long_code_weight = 1,
  toll_free_weight = 1,
  whitelisted_destinations = 'US',
  webhook_url              = "",
  webhook_failover_url     = "",
  params                   = NULL,
  ...
) {
  body <- list(
    "number_pool_settings" = list(
      "geomatch"         = geomatch,
      "skip_unhealthy"   = skip_unhealthy,
      "sticky_sender"    = sticky_sender,
      "long_code_weight" = long_code_weight,
      "toll_free_weight" = toll_free_weight
    ),
    "whitelisted_destinations" = as.list(whitelisted_destinations),
    "webhook_url"              = webhook_url,
    "webhook_failover_url"     = webhook_failover_url
  )
  if(!is.null(name)) {
    body <- c(body, list("name" = name))
  }
  r <- ..tl_postv2(
    body   = body,
    ep     = 'messaging_profiles',
    apikey = key,
    params
  )
  httr::stop_for_status(r)
  return(httr::content(r, as = 'parsed',encoding = 'UTF-8'))
}

#' @rdname tl_create_msg_profile
#' @export
tl_update_msg_profile <- function(
  key,
  profile_id,
  params
) {
  r <- ..tl_postv2(
    body   = params,
    ep     = stringr::str_glue('messaging_profiles/{profile_id}'),
    apikey = key,
    fun    = httr::PATCH
  )
  httr::stop_for_status(r)
  return(httr::content(r, as = 'parsed',encoding = 'UTF-8'))
}

#' @title Purchase numbers and optionally assign to Messaging Profile.
#'
#' @description
#' Purchase Telnyx numbers and optionally assign them to a Messaging Profile.
#' This leverages a combination of the Number Search API and Number Order API to find
#' numbers based on a series of search parameters.
#'
#' Currently only configured to purchase US phone numbers.
#'
#' See \href{https://developers.telnyx.com/docs/v2/numbers/quickstarts/number-search}{here} for more details.
#'
#'
#' @param key Required. Telnyx API key
#' @param n Required. Integer number of phone numbers to purchase. Maximum is 100 If less than 100 are available
#' for the given filter parameters it will purchase all available numbers.
#' @param search_only Boolean. If \code{TRUE} will return only the results of the number search and not purchase the numbers. Defaults to \code{FALSE}.
#' @param incl_best_effort Boolean. If \code{TRUE} will include 'best effort' results,
#'   i.e. results that are close to the filtered parameters (e.g. geographically) but are not exactly within the filter.
#'   Defaults to \code{FALSE} in order to make this decision explicit by the user.
#' @param type the type of phone number to purchase. Options are 'toll-free' and 'long-code'. Defaults to 'long-code'.
#' @param area_code Optional. Specify the 3 digit area code the phone number should be associated with as a character string.
#' @param state Optional. Two character state abbreviation that the phone number should be in.
#' @param city Optional. City that the phone number should be in. It is recommended to use this in conjunction with the \code{state} parameter.
#' @param profile_id Optional. The messaging profile to assign the numbers to.
#' @param params Optional further arguments to pass into API body. Should be a named list.
#' @param ... Unused at this time.
#'
#' @export
tl_order_numbers <- function(
  key,
  n,
  search_only      = FALSE,
  incl_best_effort = FALSE,
  type        = 'long-code',
  area_code   = NULL,
  city        = NULL,
  state       = NULL,
  profile_id  = NULL,
  params      = NULL,
  ...
) {

  if(!type %in% c("long-code", "toll-free")) {
    stop("Invalid type parameter. Options are 'long-code' and 'toll-free'")
  }

  if(n < 1 | n > 100){
    stop("n must be an integer between 1 and 100 inclusive.")
  }

  # Prepare filters for search
  query <- list(
    "filter[country_code]" = "US",
    "filter[features]"     = "sms",
    "filter[limit]"        = n,
    "filter[best_effort]"  = incl_best_effort
  )
  if(type == 'toll-free'){
    query <- c(query, list('filter[number_type]' = 'toll-free'))
  }
  if(!is.null(area_code)) {
    area_code <- as.character(area_code)
    if(nchar(area_code) != 3){
      warning("Likely invalid area_code. Use a 3 character string to improve results.")
    }
    query <- c(query, list('filter[national_destination_code]' = area_code))
  }
  if(!is.null(state)){
    if(nchar(state) > 2) {
      warning("State parameter is not a recognized state abbreviation. Recommend using 2 character state abbreviations.")
    }
    query <- c(query, list('filter[administrative_area]' = state))
  }
  if(!is.null(city)) {
    if(is.null(state)){
      warning("City set without state parameter state. It is recommended to set the state parameter with the city parameter.")
    }
    query <- c(query, list('filter[locality]' = city))
  }

  # Perform search and parse results
  r <- ..tl_getv2(
    ep = 'available_phone_numbers',
    apikey = key,
    query = query
  )
  httr::stop_for_status(r)
  avail_nums <- httr::content(r, 'parsed', encoding = 'UTF-8')
  if(avail_nums[['meta']][['total_results']] == 0) {
    stop("No numbers for provided filters. Adjust parameters to widen search.")
  }
  if(search_only) {
    return(avail_nums)
  }

  # Now order the numbers
  order <- purrr::map_chr(avail_nums$data, `[[`, 'phone_number')
  order <- purrr::map(order, ~list('phone_number' = .x))
  bdy <- list(
    'phone_numbers' = order
  )
  if(!is.null(profile_id)){
    bdy <- c(bdy, list('messaging_profile_id' = profile_id))
  }
  r_order <- ..tl_postv2(
    ep = 'number_orders',
    apikey = key,
    body = bdy
  )
  httr::stop_for_status(r_order)
  return(httr::content(r_order, 'parsed', encoding = 'UTF-8'))
}

#' @title List numbers associated with a Messaging Profile
#'
#' @description
#' Lists all phone numbers associated with a Messaging Profile and their settings.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/messaging/Messaging-Profiles#listMessagingProfilePhoneNumbers}{here} for more details.
#'
#'
#' @param key Required. Telnyx API key.
#' @param profile_id Required. Messaging Profile id to update all numbers.
#'
#' @export
tl_get_msg_profile_nums <- function(
  key,
  profile_id
) {
  nums <- list()
  curr_pg <- 0
  tot_pg <- 1
  while(curr_pg < tot_pg){
    r <- ..tl_getv2(
      ep = stringr::str_glue('messaging_profiles/{profile_id}/phone_numbers'),
      apikey = key,
      query = list(
        "page[number]" = curr_pg + 1,
        "page[size]"   = 250
      )
    )
    httr::stop_for_status(r, stringr::str_glue("Paginating, at page {curr_pg +1} out of {tot_pg}"))
    res <- httr::content(r, 'parsed', encoding = 'UTF-8')
    nums <- c(nums, list(res))
    curr_pg <- res[['meta']][['page_number']]
    tot_pg <- res[['meta']][['total_pages']]
  }

  output <- purrr::map(nums, `[[`, 'data')
  output <- purrr::flatten(output)
  return(output)
}

#' @title Setup call forwarding for a given Messaging Profile
#'
#' @description
#' Sets call forwarding up for a given Messaging Profile.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/numbers/Number-Configurations}{here} for more details.
#'
#'
#' @param key Required. Telnyx API key.
#' @param fwd_number Required. Phone number to forward to. Should be a string in the format +14156660420.
#' @param profile_id  Messaging Profile id to update all numbers. Required if \code{phone_ids} are not set.
#' @param phone_ids Vector of phone ids to update. Note these are not the phone numbers, but the Telnyx assigned ID. Required if \code{profile_id} is not set.
#' @param ... Unused at this time.
#'
#' @export
tl_set_call_fwd <- function(
  key,
  fwd_number,
  profile_id = NULL,
  phone_ids = NULL,
  ...
) {

  if(nchar(fwd_number) != 12 | stringr::str_sub(fwd_number, 1, 1) != "+") {
    stop("Forwarding number must be a valid 10 digit number with country code supplied. e.g. +14206660069.")
  }

  # Get the list of phone numbers for the messaging service
  if(!is.null(profile_id)) {
    if(!is.null(phone_ids)) warning("profile_id set, ignoring supplied phone_ids.")
    pids <- purrr::map_chr(tl_get_msg_profile_nums(key, profile_id), `[[`, 'id')
  } else if(!is.null(phone_ids)) {
    pids <- phone_ids
  } else {
    stop("Neither profile_id nor phone_ids supplied.")
  }

  # Now set the call forwarding for all of them
  bdy <- list(
    'call_forwarding' = list(
      'call_forwarding_enabled' = TRUE,
      'forwarding_type'         = 'always',
      'forwards_to'             = fwd_number
    )
  )
  r_ll <- purrr::map(pids, function(id){
    resp <- ..tl_postv2(
      body   = bdy,
      ep     = stringr::str_glue('phone_numbers/{id}/voice'),
      apikey = key,
      fun    = httr::PATCH
    )
    httr::stop_for_status(resp, task = "Assigning call forwarding to numbers")
    return(resp)
  })
  return(purrr::map(r_ll, httr::content, 'parsed', encoding = 'UTF-8'))
}

#' @title Setup Messaging Profile with numbers and Call Forwarding
#'
#' @description
#' Setup a messaging profile, purchase a set of numbers, and apply call forwarding.
#' This is a convenience wrapper around \code{tl_create_msg_profile}, \code{tl_order_numbers}, and \code{tl_set_call_fwd}
#' If you want more specific settings use those functions individually or pass in function parameters.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/numbers/Number-Configurations}{here} for more details.
#'
#' @param api_key Required. Telnyx API key.
#' @param profile_name Required. Messaging Profile name.
#' @param n_phone_numbers Required. Number of phone numbers to allocate to the Messaging Service.
#' @param profile_state Required. Abbreviated state name to look up phone numbers. e.g. CA, OR, CO, etc.
#' @param profile_city Optional. City name to narrow down the search to provision phone numbers.
#' @param call_fwd_number Optional. Phone number to forward to. Should be a string in the format +14156660420.
#' @param verbose Defaults to \code{FALSE}.
#' @param ... Optional further parameters to pass into other functions.
#'
#' @export

tl_setup_msg_profile <- function(
  api_key,
  profile_name,
  n_phone_numbers,
  profile_state,
  profile_city    = NULL,
  call_fwd_number = NULL,
  verbose = FALSE,
  ...
) {

  # Create Messaging Profile
  profile <- tl_create_msg_profile(
    key             = api_key,
    name            = profile_name,
    geomatch        = TRUE,
    skip_unhealthy  = TRUE,
    sticky_sender   = TRUE,
    ...
  )
  msg_profile_id <- profile[['data']][['id']]
  if(verbose){
    print(stringr::str_glue("Created Messaging Profile with name {profile$data$name} and id {msg_profile_id} ..."))
  }

  # Order and assign numbers
  safe_order <- purrr::safely(tl_order_numbers, otherwise = list(), quiet = verbose)
  num_order <- safe_order(
    key   = api_key,
    n     = n_phone_numbers,
    city  = profile_city,
    state = profile_state,
    incl_best_effort = TRUE,
    profile_id       = msg_profile_id,
    ...
  )

  # Handle when no numbers are successfully ordered for a given area code.
  if(!is.null(num_order$error)) {
    del <- ..tl_getv2(
      ep     = stringr::str_glue('messaging_profiles/{msg_profile_id}'),
      apikey = api_key,
      fun    = httr::DELETE
    )
    httr::stop_for_status(del, task = stringr::str_glue("Error in deleting messaging profile {msg_profile_id}"))
    stop(stringr::str_glue("Error in ordering numbers. Cleaning up by deleting messaging profile {msg_profile_id}. Maybe try a different region?"))
  } else {
    nums <- num_order$result
  }

  # Check how many we bought.
  n_purchased <- nums[['data']][['phone_numbers_count']]
  if(verbose){
    print(stringr::str_glue("Purchased {n_purchased} numbers ..."))
  }

  if(n_purchased != n_phone_numbers) {
    warning("Not enough available phone numbers in region, only purcahsed {n_purchased} phone numbers")
  }

  # to let the above finish processing.
  # TODO: Figure out latency between number allocation and order request
  Sys.sleep(2)

  # Setup call forwarding
  if(!is.null(call_fwd_number)) {
    fwd <- tl_set_call_fwd(
      key        = api_key,
      fwd_number = call_fwd_number,
      profile_id = msg_profile_id,
      ...
    )
  } else {
    fwd <- NULL
  }

  returnObject <- list(
    'msg_profile'        = profile,
    'ordered_phone_nums' = nums,
    'p_voice_settings'   = fwd
  )
  return(returnObject)
}


#' @describeIn POST to Telnyx API v2 (thats why its called v2)
#' @export
..tl_postv2 <- function(
  body,
  ep,
  apikey,
  p   = NULL,
  fun = httr::POST
) {
  authentication <- stringr::str_glue('Bearer {apikey}')
  args <- list(
    url  = stringr::str_glue('https://api.telnyx.com/v2/{ep}'),
    body = jsonlite::toJSON(c(body, p), auto_unbox = T),
    httr::add_headers('Authorization' = authentication),
    httr::content_type_json()
  )
  resp <- do.call(fun, args)
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
  ep, token, user, ...
) {
  resp <- httr::GET(
    url  = stringr::str_glue('https://api.telnyx.com/{ep}'),
    httr::add_headers(
      'x-api-token' = token,
      'x-api-user'  = user
    ),
    ...
  )
  httr::stop_for_status(resp)
  return(resp)
}

#' @describeIn GET to Telnyx API v2 (thats why its called v2)
#' @export
..tl_getv2 <- function(
  ep, apikey,
  fun = httr::GET,
  ...
) {
  authentication <- stringr::str_glue('Bearer {apikey}')
  args <- list(
    url  = stringr::str_glue('https://api.telnyx.com/v2/{ep}'),
    httr::add_headers('Authorization' = authentication),
    ...
  )
  resp <- do.call(fun, args)
  httr::stop_for_status(resp)
  return(resp)
}


