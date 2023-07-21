#' @importFrom magrittr `%>%`
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
NULL

.onLoad <- function(libname, pkgname) {
  # Create a cache that expires every hour
  cache <- cachem::cache_mem(max_age = 60 * 60)

  # Re-assigning these into the package envir as memoised functions
  # i.e. the output gets cached after the first run, subsequent calls fetch
  # the cached result instead of pulling down the data again.
  get_testing <<- memoise::memoise(get_testing, cache = cache)
  get_onetable <<- memoise::memoise(get_onetable, cache = cache)
  get_covid_df <<- memoise::memoise(get_covid_df, cache = cache)
  get_gdeltnews <<- memoise::memoise(get_gdeltnews, cache = cache)
  get_vax <<- memoise::memoise(get_vax, cache = cache)
  get_vax_manufacturers <<- memoise::memoise(get_vax_manufacturers, cache = cache)
  get_vax_by_type <<- memoise::memoise(get_vax_by_type, cache = cache)

  # Check whether individual has data lake credentials loaded
  # (in which case, we'll use data lake as upstream source instead of directly connecting to web sources)
  if (Sys.getenv("AZURE_APP_ID") != "" && Sys.getenv("AZURE_APP_SECRET") != "" && Sys.getenv("AZURE_TENANT_ID") != "" && Sys.getenv("AZURE_DL_PATH") != "") {
    options(savir.use_datalake = TRUE)

    rlang::check_installed("AzureRMR")
    rlang::check_installed("AzureStor")

    azure_token <- AzureRMR::get_azure_token(
      "https://storage.azure.com",
      tenant = Sys.getenv("AZURE_TENANT_ID"),
      app = Sys.getenv("AZURE_APP_ID"),
      password = Sys.getenv("AZURE_APP_SECRET")
    )

    az_container <<- AzureStor::storage_container(Sys.getenv("AZURE_DL_PATH"), token = azure_token)
  } else {
    options(savir.use_datalake = FALSE)
  }


  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (getOption("savir.use_datalake", FALSE)) {
    packageStartupMessage(
      "CDC Datalake Credentials Detected.\n",
      "Case and death data will be pulled from this source."
    )
  } else {
    packageStartupMessage(
      "No CDC Datalake Credentials Detected.\n",
      "Case and death data will be pulled directly from web source"
    )
  }

}