#' @title Leaf Litter Decomposition Analysis for Stream Ecosystems
#'
#' @description Calculate AFDM
#'
#' @param data
#'
#' @param InitialWt
#'
#' @param FinalWt
#'
#' @param Frac.InitialWt
#'
#' @param Frac.FinalWt
#'
#' @param Treatment
#'
#' @param Day
#'
#' @param Replicate
#'
#' @return Remaining
#'
#' @examples AFDM(data,InitialWt,FinalWt,Frac.InitialWt,Frac.FinalWt,Treatment,Day,Replicate)
#'
#' @export AFDM
AFDM <- function(data,
                 InitialWt,
                 FinalWt,
                 Frac.InitialWt,
                 Frac.FinalWt,
                 Treatment,
                 Day,
                 Replicate) {
  # Calculate the control by manipulation
  control <- data %>%
    dplyr::filter(Treatment == "Control") %>%
    dplyr::select({{InitialWt}},{{FinalWt}}) %>%
    dplyr::mutate(Difference = {{FinalWt}}/{{InitialWt}})
  meanControl <- mean(control$Difference, na.rm = TRUE)

  # Corrects the initial dry mass by manipulation
  . <- data %>%
    dplyr::filter(Treatment != "Control") %>%
    dplyr::mutate(IDWc = {{InitialWt}} * meanControl) %>%  # Corrects dry mass (laboratory) for mass lost from handling

    # Calculate the AFDM in the subsample
    dplyr::mutate(AFDMFraction = ({{Frac.InitialWt}} - {{Frac.FinalWt}}) / {{Frac.InitialWt}}) %>%   # AFDM in the subsample

    # Calculate the AFDM in the corrected initial mass and in the final mass
    dplyr::mutate(AFDM_Initial = IDWc * AFDMFraction) %>% # AFDM in the initial sample
    dplyr::mutate(AFDM_Final = {{FinalWt}} * AFDMFraction) %>%  #AFDM in the Final sample

    # Calculate the percentage of remaining mass
    dplyr::mutate(AFDMrem = (AFDM_Final/AFDM_Initial) * 100)     # % AFDM Remaining

  AFDM1 <- dplyr::select(., {{Day}}, {{Replicate}}, {{Treatment}}, AFDMrem)
  AFDM2 <- dplyr::arrange(AFDM1, {{Treatment}}, {{Replicate}})

  # Calculate LN
  Remaining <- AFDM2 %>%
    group_by(grp = cumsum(Day == 2)) %>%
    complete(Day =  c(0, unique(Day)), fill = list(AFDMrem = meanControl * 100))%>%
    fill(Replicate, Treatment , .direction = 'updown')%>%
    dplyr::mutate(Ln.AFDMrem = log(AFDMrem))

  Remaining <- as.data.frame(Remaining)

  return(Remaining)
}
