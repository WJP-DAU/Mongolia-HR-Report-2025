## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Mongolia Subfactor Performance Charts
##
## Author(s):         Santiago Pardo
##
## Dependencies:      World Justice Project
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 0. Setup ------------------------------------------------------------------------------------------------

library(pacman)
p_load(char = c("tidyverse", "openxlsx", "ggtext", "sysfonts", "showtext"))

# Path configuration
if(Sys.info()["user"] == "santiagopardo"){
  path2DA <- "/Users/santiagopardo/Library/CloudStorage/OneDrive-WorldJusticeProject/Data Analytics/6. Country Reports/Mongolia-HR-Report-2025/data-viz"
} else {
  path2DA <- "INSERT_YOUR_PATH_HERE"
}

# Load fonts
path2fonts <- file.path(path2DA, "6. Country Reports/0. Fonts", fsep = "/")
if (file.exists(file.path(path2fonts, "Lato-Regular.ttf"))) {
  font_add(family = "Lato Full",
           regular = file.path(path2fonts, "Lato-Regular.ttf"),
           bold = file.path(path2fonts, "Lato-Bold.ttf"))
}
showtext_auto()

## 1. Define subfactors ------------------------------------------------------------------------------------

MAIN_COUNTRY <- "Mongolia"
REGION <- "East Asia and Pacific"

# Subfactors by factor with short labels
subfactors <- list(
  "Factor 1: Constraints on Government Powers" = list(
    cols = c(
      "1.1.Government.powers.are.effectively.limited.by.the.legislature",
      "1.2.Government.powers.are.effectively.limited.by.the.judiciary",
      "1.3.Government.powers.are.effectively.limited.by.independent.auditing.and.review",
      "1.4.Government.officials.are.sanctioned.for.misconduct",
      "1.5.Government.powers.are.subject.to.non-governmental.checks",
      "1.6.Transition.of.power.is.subject.to.the.law"
    ),
    labels = c(
      "1.1 Limits by the legislature",
      "1.2 Limits by the judiciary",
      "1.3 Independent auditing",
      "1.4 Sanctions for official\nmisconduct",
      "1.5 Non-governmental checks",
      "1.6 Lawful transition of power"
    )
  ),
  "Factor 2: Absence of Corruption" = list(
    cols = c(
      "2.1.Government.officials.in.the.executive.branch.do.not.use.public.office.for.private.gain",
      "2.2.Government.officials.in.the.judicial.branch.do.not.use.public.office.for.private.gain",
      "2.3.Government.officials.in.the.police.and.the.military.do.not.use.public.office.for.private.gain",
      "2.4.Government.officials.in.the.legislative.branch.do.not.use.public.office.for.private.gain"
    ),
    labels = c(
      "2.1 In the executive branch",
      "2.2 In the judiciary",
      "2.3 In the police/military",
      "2.4 In the legislature"
    )
  ),
  "Factor 3: Open Government" = list(
    cols = c(
      "3.1..Publicized.laws.and.government.data",
      "3.2.Right.to.information",
      "3.3.Civic.participation",
      "3.4.Complaint.mechanisms"
    ),
    labels = c(
      "3.1 Publicized laws and\ngov't data",
      "3.2 Right to information",
      "3.3 Civic participation",
      "3.4 Complaint mechanisms"
    )
  ),
  "Factor 4: Fundamental Rights" = list(
    cols = c(
      "4.1.Equal.treatment.and.absence.of.discrimination",
      "4.2.The.right.to.life.and.security.of.the.person.is.effectively.guaranteed",
      "4.3.Due.process.of.the.law.and.rights.of.the.accused",
      "4.4.Freedom.of.opinion.and.expression.is.effectively.guaranteed",
      "4.5.Freedom.of.belief.and.religion.is.effectively.guaranteed",
      "4.6.Freedom.from.arbitrary.interference.with.privacy.is.effectively.guaranteed",
      "4.7.Freedom.of.assembly.and.association.is.effectively.guaranteed",
      "4.8.Fundamental.labor.rights.are.effectively.guaranteed"
    ),
    labels = c(
      "4.1 No discrimination",
      "4.2 Right to life and security",
      "4.3 Due process of law",
      "4.4 Freedom of expression",
      "4.5 Freedom of religion",
      "4.6 Right to privacy",
      "4.7 Freedom of association",
      "4.8 Labor rights"
    )
  ),
  "Factor 5: Order and Security" = list(
    cols = c(
      "5.1.Crime.is.effectively.controlled",
      "5.2.Civil.conflict.is.effectively.limited",
      "5.3.People.do.not.resort.to.violence.to.redress.personal.grievances"
    ),
    labels = c(
      "5.1 Absence of crime",
      "5.2 Limit of civil conflict",
      "5.3 Absence of violent redress"
    )
  ),
  "Factor 6: Regulatory Enforcement" = list(
    cols = c(
      "6.1.Government.regulations.are.effectively.enforced",
      "6.2.Government.regulations.are.applied.and.enforced.without.improper.influence",
      "6.3.Administrative.proceedings.are.conducted.without.unreasonable.delay",
      "6.4.Due.process.is.respected.in.administrative.proceedings",
      "6.5.The.government.does.not.expropriate.without.lawful.process.and.adequate.compensation"
    ),
    labels = c(
      "6.1 Effective regulatory\nenforcement",
      "6.2 No improper influence",
      "6.3 No unreasonable delay",
      "6.4 Respect for due process",
      "6.5 No expropriation w/out\nadequate compensation"
    )
  ),
  "Factor 7: Civil Justice" = list(
    cols = c(
      "7.1.People.can.access.and.afford.civil.justice",
      "7.2.Civil.justice.is.free.of.discrimination",
      "7.3.Civil.justice.is.free.of.corruption",
      "7.4.Civil.justice.is.free.of.improper.government.influence",
      "7.5.Civil.justice.is.not.subject.to.unreasonable.delay",
      "7.6..Civil.justice.is.effectively.enforced",
      "7.7.Alternative.dispute.resolution.mechanisms.are.accessible,.impartial,.and.effective"
    ),
    labels = c(
      "7.1 Accessibility and\naffordability",
      "7.2 No discrimination",
      "7.3 No corruption",
      "7.4 No improper gov't influence",
      "7.5 No unreasonable delay",
      "7.6 Effective enforcement",
      "7.7 Impartial and effective\nADRs"
    )
  ),
  "Factor 8: Criminal Justice" = list(
    cols = c(
      "8.1.Criminal.investigation.system.is.effective",
      "8.2.Criminal.adjudication.system.is.timely.and.effective",
      "8.3.Correctional.system.is.effective.in.reducing.criminal.behavior",
      "8.4.Criminal.system.is.impartial",
      "8.5.Criminal.system.is.free.of.corruption",
      "8.6.Criminal.system.is.free.of.improper.government.influence",
      "8.7..Due.process.of.the.law.and.rights.of.the.accused"
    ),
    labels = c(
      "8.1 Effective investigations",
      "8.2 Timely and effective\nadjudication",
      "8.3 Effective correctional\nsystem",
      "8.4 No discrimination",
      "8.5 No corruption",
      "8.6 No improper gov't influence",
      "8.7 Due process of law"
    )
  )
)

## 2. Load data --------------------------------------------------------------------------------------------

master_data <- read.xlsx(
  file.path(path2DA, "inputs/FINAL_2025_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx"),
  sheet = "Historical Data",
  check.names = FALSE
)

# Get 2025 data
data_2025 <- master_data %>%
  filter(Year == "2025")

# Mongolia data
mongolia_data <- data_2025 %>%
  filter(Country == MAIN_COUNTRY)

# Regional average (East Asia and Pacific)
eap_data <- data_2025 %>%
  filter(Region == REGION)

## 3. Create charts ----------------------------------------------------------------------------------------

create_subfactor_chart <- function(factor_name, cols, labels) {

  # Get Mongolia scores
  mongolia_scores <- mongolia_data %>%
    select(all_of(cols)) %>%
    pivot_longer(everything(), names_to = "subfactor", values_to = "mongolia") %>%
    mutate(mongolia = as.numeric(mongolia))

 # Get regional average
  regional_avg <- eap_data %>%
    select(all_of(cols)) %>%
    summarise(across(everything(), ~mean(as.numeric(.), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "subfactor", values_to = "regional")

  # Combine data
  plot_data <- mongolia_scores %>%
    left_join(regional_avg, by = "subfactor") %>%
    mutate(
      label = labels,
      label = factor(label, levels = rev(labels))
    )

  # Create the chart
  chart <- ggplot(plot_data) +
    # Vertical grid lines
    geom_vline(xintercept = seq(0, 1, 0.2), color = "#d1cfd1", linewidth = 0.3) +

    # Regional average (red diamond)
    geom_point(aes(x = regional, y = label),
               shape = 18, size = 2.5, color = "#E63946") +

    # Mongolia (blue circle)
    geom_point(aes(x = mongolia, y = label),
               shape = 16, size = 2.5, color = "#2A2A94") +

    # Scales
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = c("0.00", "0.20", "0.40", "0.60", "0.80", "1.00"),
      position = "top",
      expand = c(0.02, 0)
    ) +

    # Labels
    labs(
      x = NULL,
      y = NULL
    ) +

    # Theme
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        family = "Lato Full",
        size = 6,
        color = "#524F4C"
      ),
      axis.text.y = element_text(
        family = "Lato Full",
        size = 7,
        color = "#524F4C",
        hjust = 0,
        lineheight = 0.85
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(5, 10, 5, 2),
      plot.background = element_blank(),
      panel.background = element_blank()
    )

  return(chart)
}

## 4. Generate and save all charts -------------------------------------------------------------------------

factor_nums <- 1:8

for (i in factor_nums) {
  factor_name <- names(subfactors)[i]
  cols <- subfactors[[i]]$cols
  labels <- subfactors[[i]]$labels

  chart <- create_subfactor_chart(factor_name, cols, labels)

  # Adjust height based on number of subfactors (compact for 4 per page)
  n_subfactors <- length(cols)
  height <- 8 + (n_subfactors * 6)

  ggsave(
    plot = chart,
    filename = file.path(path2DA, paste0("outputs/Subfactor_F", i, ".svg")),
    width = 85,
    height = height,
    units = "mm",
    dpi = 72,
    device = "svg"
  )

  message(paste0("Chart saved: Subfactor_F", i, ".svg"))
}

message("\nAll 8 subfactor charts saved to outputs/")
