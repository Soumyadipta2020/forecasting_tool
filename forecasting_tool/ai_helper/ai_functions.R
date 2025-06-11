# ChatGPT ####
chat <- function(user_message,
                 history = NULL,
                 system_prompt = c("general", "code"),
                 api_key,
                 temp) {
  system <- get_system_prompt(system_prompt)
  prompt <- prepare_prompt(user_message, system, history)
  base_url <- "https://api.openai.com/v1"
  body <- list(model = "gpt-3.5-turbo", messages = prompt, temperature = temp)
  req <-
    resp <-
    request(base_url) |>
    req_url_path_append("chat/completions") |>
    req_auth_bearer_token(token = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_user_agent("Soumyadipta Das") |>
    req_body_json(body) |>
    req_retry(max_tries = 4) |>
    req_throttle(rate = 15) |>
    req_perform()

  openai_chat_response <-
    resp |> resp_body_json(simplifyVector = TRUE)

  return(openai_chat_response$choices$message$content)
}

# Nvidia chat ####
chat_nvidia <- function(user_message,
                        history = NULL,
                        api_key,
                        model_llm,
                        temp = 0.2,
                        topp = 0.7,
                        max_token = 1024) {
  user_prompt <- list(list(role = "user", content = user_message))
  prompt <- c(history, user_prompt) |> purrr::compact()

  base_url <- "https://integrate.api.nvidia.com/v1"
  body <- list(
    model = model_llm, messages = prompt, temperature = temp, top_p = topp,
    max_tokens = max_token
  )
  req <-
    resp <-
    request(base_url) |>
    req_url_path_append("chat/completions") |>
    req_auth_bearer_token(token = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_user_agent("Soumyadipta Das") |>
    req_body_json(body) |>
    req_retry(max_tries = 4) |>
    req_throttle(rate = 15) |>
    req_perform()

  openai_chat_response <-
    resp |> resp_body_json(simplifyVector = TRUE)

  return(openai_chat_response$choices$message$content)
}

# Google Gemini ####
gemini <- function(prompt,
                   temperature = 0.7,
                   api_key,
                   model = "gemini-2.0-flash",
                   max_retries = 3) {
  if (nchar(api_key) < 1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }

  model_query <- paste0(model, ":generateContent")

  # Add new message #####
  if (!exists("chatHistory")) {
    chatHistory <<- list(list(role = "user", parts = list(list(text = prompt))))
  } else {
    chatHistory <<- append(chatHistory, list(list(
      role = "user", parts = list(list(text = prompt))
    )))
  }

  for (i in 1:max_retries) {
    response <- tryCatch(
      {
        POST(
          url = paste0(
            "https://generativelanguage.googleapis.com/v1beta/models/",
            model_query
          ),
          query = list(key = api_key),
          content_type_json(),
          body = toJSON(
            list(
              contents = chatHistory,
              generationConfig = list(temperature = temperature)
            ),
            auto_unbox = T
          )
        )
      },
      error = function(e) {
        cat("Error: ", summary(e), "\n")
        NULL
      }
    )

    if (!is.null(response) && response$status_code <= 200) {
      answer <- content(response)$candidates[[1]]$content$parts[[1]]$text
      chatHistory <<- append(chatHistory, list(list(
        role = "model", parts = list(list(text = answer))
      )))
      return(answer)
    } else if (i < max_retries) {
      cat("API call failed. Retrying attempt ", i, "...\n")
      Sys.sleep(2^i) # Exponential backoff for retry delays
    } else {
      stop(paste(
        "Failed to access Gemini API after",
        max_retries,
        "retries."
      ))
    }
  }
}

# System Prompt type ####
get_system_prompt <- function(system = c("general", "code")) {
  rlang::arg_match(system)
  instructions <-
    switch(system,
      "general" = "You are a helpful assistant.",
      "code"    = "You are a helpful chat bot that answers questions for an R programmer working in the RStudio IDE."
    )
  list(list(role = "system", content = instructions))
}

# Prompt prepare ####
prepare_prompt <- function(user_message, system_prompt, history) {
  user_prompt <- list(list(role = "user", content = user_message))
  c(system_prompt, history, user_prompt) |> purrr::compact()
}

# History maintain ####
update_history <- function(history, user_message, response) {
  c(history, list(
    list(role = "user", content = user_message),
    list(role = "assistant", content = response)
  )) |> purrr::compact()
}
