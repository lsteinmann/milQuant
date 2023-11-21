# Set variables which are available at the beginning of all sessions

## Design

uhhcol <- c("#e2001a", "#3b515b")
uhhcol <<- colorRampPalette(uhhcol)

uhhcol_two <- c("#3b515b", "#b6bfc3", "#e2001a", "#f07f8c", "#80b8dd", "#0271bb")
uhhcol_two <<- colorRampPalette(uhhcol_two)


Plot_Base_Theme <<- theme(panel.background = element_blank(),
                          panel.grid.major = element_line(color = "grey60",
                                                          linetype = "dashed"),
                          panel.grid.minor = element_line(color = "grey80",
                                                          linetype = "dotted"),
                          axis.text.x = element_text(angle = 45,
                                                     hjust = 1,
                                                     vjust = 1,
                                                     size = 14),
                          axis.title = element_text(size = 16),
                          legend.position = "bottom")

Plot_Base_Guide <<- guides(fill = guide_legend(ncol = 10, byrow = TRUE))



## Global Variables

#operations <- c("all", sort(na.omit(unique(index$Place))))


drop_for_plot_vars <<- c("identifier", "shortDescription", "notes",
                         "processor",
                         "storagePlaceOther", "measuringPointID",
                         "localizationDescription", "conditionComment",
                         "comparison", "comparisonLit",
                         "MuseumInventoryNr", "OldInventoryNr",
                         "FotoNr", "DrawingNr", "vesselFormDescription",
                         "fabricStructure", "temperType", "temperTypeOther",
                         "temperAmount", "temperGrain", "surfaceTreatment",
                         "surfaceTreatmentDescription", "analysisMethodOther",
                         "analysisAim", "analysisActor", "analysisResult",
                         "otherNotes", "id", "relation.isRecordedIn",
                         "relation.isDepictedIn", "relation.liesWithin",
                         "relation.liesWithinLayer",
                         "relation.isSameAs",
                         "category", "workflow",
                         "analysisMethod", "localization")


periods <- readRDS(system.file(package = "milQuant", mustWork = TRUE,
                               "extdata/milet-periods.RDS"))
periods <- c(names(periods[[1]]), "multiple", "unbestimmt")
periods <<- factor(periods,
                   levels = periods,
                   ordered = TRUE)

#period_colors <- read.csv("external/period_colors.csv", header = FALSE)[,1]
period_colors <- viridis::viridis_pal(option = "D")(length(periods)-2)
period_colors <<- c(period_colors, "#cd2436", "#a6a6a6")

scale_fill_period <<- function(ncol = 9) {
  scale_fill_manual(values = period_colors,
                    breaks = periods,
                    limits = periods,
                    guide = guide_legend(name = "Period",
                                         ncol = ncol,
                                         byrow = TRUE))
}

find_categories <<- c("Find", "Pottery", "Lamp", "Loomweight", "Terracotta", "Brick",
                      "Bone", "Glass", "Metal", "Stone", "Wood", "Coin",
                      "PlasterFragment", "Mollusk", "Sculpture")
quant_categories <<- c("Quantification", "Brick_Quantification",
                       "Pottery_Quantification_A", "Pottery_Quantification_B",
                       "QuantMollusks", "PlasterQuantification")


read_settings <- function() {
  result <- try(readRDS(system.file(package = "milQuant", mustWork = TRUE,
                                    "app/www/settings/selection_settings.RDS")),
                silent = TRUE)
  if (inherits(result, "try-error")) {
    result <- list("selected_project" = NULL,
                   "selected_operations" = NULL,
                   "selected_trenches" = NULL)
  }
  return(result)
}
selection_settings <<- read_settings()
