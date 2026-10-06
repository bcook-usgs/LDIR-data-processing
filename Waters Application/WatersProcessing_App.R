#app.R

# --- Libraries ---
library(shiny)
library(tidyverse)
library(readxl)
library(openxlsx)
library(shinyjs)
library(rlang)
library(janitor)
library(stringr)
library(utils) 
library(DT)

# --- R Script Content: Processing Functions and Definitions ---

signif_pad <- function(x, digits) {
  if(is.numeric(x)) {
    non_zero_x <- abs(x[!is.na(x) & x != 0])
    if (length(non_zero_x) == 0) return(as.character(x))
    max_exponent <- floor(log10(max(non_zero_x, na.rm = TRUE)))
    padding_length <- max(0, digits - max_exponent - 1)
    sprintf(paste0("%.", padding_length, "f"), x)
  } else {
    as.character(x)
  }
}

LDIR_dictionary <- tibble(
  Column_ID = c("ID","width_um", "height_um", "diameter_um", "aspect_ratio", "area_um", "perimeter_um", "eccentricity", "circularity", "solidity", "identification", "HQI"),
  Definition = c("LDIR assigned particle number", "Width of the particle in µm", "Height of the particle in µm", "Calculated by equating the calculated particle area to the area of a circle. The diameter is calculated using equation A = π*(diameter^2)/4", "The ratio of width to height", "Geometric area in µm^2", "Length of the line in µm that makes up the boundary of the particle", "A metric which characterizes shape. A circle has an eccentricity of 0 while ellipses range from 0-1. A value close to 1 suggests high aspect ratio.", "Measure of how close the shape of the particle is to a circle. A perfect circle will have a circularity of 1. Other shapes will have value < 1.", "The ratio of the particle area over the area of its convex hull.", "Highest spectral library match", "Hit-Quality_Index. A hit quality of 1 is a perfect match. Our threshold for hit quality is set to 0.8.")
)

Microplastic_IDs <- c("Acrylates", "Acrylonitrile Butadiene", "Alkyd Varnish", "Ethylene Vinyl Acetate (EVA)", "PE", "Polyacetal", "Polybutadiene", "Polycaprolactone", "Polycarbonate (PC)", "Polyether", "Polyethylene (PE)", "Polyethylene Chlorinated", "Polyethylene Terephthalate (PET)", "Polyhydroxybutyric Acid", "Polyimide", "Polyisoprene Chlorinated", "Polylactic acid (PLA)", "Polymethylmethacrylate (PMMA)", "Polypropylene (PP)", "Polystyrene (PS)", "Polysulfone", "Polysulfones", "Polytetrafluoroethylene (PTFE)", "Polyurethane (PU)", "Polyvinyl alcohol", "Polyvinylchloride (PVC)", "Rubber", "Silicone")
OrganicMatter_Ids <- c("Natural Polyamide", "Polyamide (PA)", "Cellulose Acetate", "Cellulose chemically modified", "Cellulosic", "Chitin", "Coal")
Inorganics_IDs <- c("Silica")
new_col_names <- c("particle_number", "ID", "width_um", 'height_um', 'diameter_um', 'aspect_ratio', 'area_um', 'perimeter_um', 'eccentricity', 'circularity', 'solidity', 'identification', 'notes', 'match_type', 'HQI', 'is_valid')

# === FUNCTION 1: INITIAL PROCESSING & QA/QC ==============================
run_all_processing <- function(raw_files_list, setup_file_path, qc_tracking_path, minimum_acceptable_HQI, Smallest, Fraction1, Fraction2, Fraction3) {
  raw_data <- lapply(raw_files_list, read_xlsx, sheet = 2 , .name_repair = "universal")
  names(raw_data) <- str_remove(names(raw_files_list), ".xlsx")
  sample_setup <- read_xlsx(setup_file_path, sheet = "SAMPLE_INFO", .name_repair = "universal", range = "B1:W50") %>% 
    drop_na(Container.ID)
  
  QCS_setup <- read_xlsx(setup_file_path, sheet = "QUALITY_CONTROL", .name_repair = "universal", range = "B1:G50") %>% 
    drop_na(TYPE) %>% 
    dplyr::select(c(6,4,5)) %>% 
    filter(grepl("QCS", WORKING_SAMPLE_ID))
  
  if(nrow(QCS_setup) == 0) {
    QCS_Std_ID <- "QCS_DEFAULT_ID"
  } else {
    QCS_Std_ID <- QCS_setup[[1,2]] 
  }
  
  QCS_MP_types <- character(0)
  QCS_MP_sizes <- tibble(Identification = character(), SIZE_FRACTION = character())
  
  tryCatch({
    QCS_MP_types <- c(read_xlsx(qc_tracking_path, sheet = QCS_Std_ID, .name_repair = "universal", range = "B8:L16") %>% 
                        dplyr::select(5) %>% drop_na(1))[[1]]
    QCS_MP_sizes <- read_xlsx(qc_tracking_path, sheet = QCS_Std_ID, .name_repair = "universal", range = "B8:L16") %>% 
      dplyr::select(c(5, 6, 7)) %>% 
      drop_na(1) %>%
      rename(Identification = 1, SHAPE = 2, SIZE_FRACTION = 3)
  }, error = function(e) { warning("Could not read QCS Tracking file.") })
  
  filtered_data_AllMPs <- lapply(raw_data, function(x) filter(x, across(15) >= minimum_acceptable_HQI, Identification %in% Microplastic_IDs))
  
  if(length(QCS_MP_types) > 0) {
    filtered_data_QCS <- lapply(raw_data, function(x) filter(x, across(15) >= minimum_acceptable_HQI, Identification %in% QCS_MP_types))
  } else {
    filtered_data_QCS <- list()
  }
  
  if (length(filtered_data_AllMPs) > 0) {
    filtered_data_AllMPs <- filtered_data_AllMPs %>%  bind_rows(.id = "df_name") %>%
      mutate(parent_id = sub("(_\\d(?:_[A-Za-z]+)?)$", "", df_name)) %>%
      dplyr::select(-df_name) %>% group_by(parent_id) %>% nest() %>% deframe()
  } else {
    filtered_data_AllMPs <- list()
  }
  
  if (length(filtered_data_QCS) > 0) {
    filtered_data_QCS <- filtered_data_QCS %>%  bind_rows(.id = "df_name") %>%
      mutate(parent_id = sub("(_\\d(?:_[A-Za-z]+)?)$", "", df_name)) %>%
      dplyr::select(-df_name) %>% group_by(parent_id) %>% nest() %>% deframe()
  }
  
  QCS_samples <- list(); QCS_validation <- list(); 
  if (length(filtered_data_QCS) > 0) {
    for (id_name in names(filtered_data_QCS)){
      if (id_name %in% QCS_setup$WORKING_SAMPLE_ID) {
        spike_vol <- QCS_setup$QCS_SPIKE_VOL[QCS_setup$WORKING_SAMPLE_ID == id_name]
        spike_vol <- if(length(spike_vol) == 0 || is.na(spike_vol) || spike_vol == 0) 1 else spike_vol
        if (nrow(filtered_data_QCS[[id_name]]) > 0) {
          df_i <- filtered_data_QCS[[id_name]]
          if (grepl("_M", id_name)) {
            QCS_samples[[id_name]] <- df_i %>% group_by(`Identification`) %>% summarise(recovered_count_M = n(), .groups = 'drop') %>%
              mutate(recovered_concentration_cc_mL = recovered_count_M / spike_vol)
          } else if (grepl("_V", id_name)) {
            QCS_validation[[id_name]] <- df_i %>% group_by(`Identification`) %>% summarise(validated_count_V = n(), .groups = 'drop') %>%
              mutate(validated_concentration_cc_mL = validated_count_V / spike_vol)
          }
        }
      }
    }
  }
  
  if (length(QCS_validation) >= 1){
    if (length(QCS_validation) == 1){
      QCS_V_Summary <- QCS_validation[[1]] %>%
        mutate(Sample_Source = names(QCS_validation)[1])
    } else {
      QCS_V_Summary <- bind_rows(
        lapply(names(QCS_validation), function(name) {
          QCS_validation[[name]] %>% mutate(Sample_Source = name)
        })
      ) %>%
        group_by(Identification, Sample_Source) %>%
        summarise(validated_count_V = mean(validated_count_V),
                  validated_concentration_cc_mL = mean(validated_concentration_cc_mL), .groups = 'drop')
    }
  } else {
    tryCatch({
      default_validated_vals <- read_xlsx(qc_tracking_path, sheet = QCS_Std_ID, .name_repair = "universal", range = "F8:K16") %>% 
        rename(Identification = 1) %>% 
        drop_na(Identification) %>% filter(Identification != "#N/A") %>%
        rowwise() %>% mutate(validated_concentration_cc_mL = mean(c_across(3:6), na.rm = TRUE)) %>% ungroup() %>%
        dplyr::select(Identification, validated_concentration_cc_mL) %>%
        mutate(validated_count_V = NA_real_, Sample_Source = "Master_Tracking_Default")
      QCS_V_Summary <- default_validated_vals
    }, error = function(e) {
      QCS_V_Summary <- tibble(Identification = character(), validated_count_V = numeric(), validated_concentration_cc_mL = numeric(), Sample_Source = character())
    })
  }
  
  # --- Shape Validation ---
  QCS_V_list_forSHAPES <- list()
  v_counter <- 1
  if (nrow(QCS_setup) > 0 && length(QCS_MP_types) > 0) {
    for (i in 1:length(filtered_data_QCS)) {
      id_name <- names(filtered_data_QCS)[i]
      is_QCS_V <- id_name %in% QCS_setup$WORKING_SAMPLE_ID & grepl("_V", id_name)
      if (is_QCS_V) {
        QCS_V_list_forSHAPES[[v_counter]] <- filtered_data_QCS[[i]] %>% group_by(`Identification`)
        names(QCS_V_list_forSHAPES)[v_counter] <- id_name
        v_counter <- v_counter + 1
      }
    }
  }
  
  QCS_V_Shape_Summary <- NULL
  if (length(QCS_V_list_forSHAPES) >= 1) {
    QCS_SHAPE_details <- read_xlsx(qc_tracking_path, sheet = paste0(QCS_Std_ID), range = "F8:H16", .name_repair = "universal")
    combined_V_data <- bind_rows(QCS_V_list_forSHAPES, .id = "Sample_Source")
    QCS_V_Shape_Summary <- combined_V_data %>%
      mutate(PLASTIC_TYPE = as.factor(Identification)) %>%
      left_join(QCS_SHAPE_details, by = "PLASTIC_TYPE") %>%
      mutate(
        Aspect.Ratio = pmin(`Width..µm.`, `Height..µm.`) / pmax(`Width..µm.`, `Height..µm.`),
        determined_shape = case_when(
          Aspect.Ratio < 0.35 & SHAPE == "Fragment" ~ "Fiber",
          Aspect.Ratio >= 0.35 & Aspect.Ratio < 0.5 & SHAPE == "Fragment" ~ "Fragment",
          Aspect.Ratio >= 0.5 & Aspect.Ratio < 0.8 & SHAPE == "Fragment" ~ "Fragment",
          Aspect.Ratio >= 0.8 & Aspect.Ratio < 0.9 & SHAPE == "Fragment" ~ "Fragment",
          Aspect.Ratio >= 0.9 & SHAPE == "Fragment" ~ "Sphere",
          Aspect.Ratio < 0.35 & SHAPE == "Sphere" ~ "Fiber",
          Aspect.Ratio >= 0.35 & Aspect.Ratio < 0.5 & SHAPE == "Sphere" ~ "Fibrous Fragment",
          Aspect.Ratio >= 0.5 & Aspect.Ratio < 0.8 & SHAPE == "Sphere" ~ "Fragment",
          Aspect.Ratio >= 0.8 & Aspect.Ratio < 0.9 & SHAPE == "Sphere" ~ "Sphere",
          Aspect.Ratio >= 0.9 & SHAPE == "Sphere" ~ "Sphere",
          Aspect.Ratio < 0.35 & SHAPE == "Fiber" ~ "Fiber",
          Aspect.Ratio >= 0.35 & Aspect.Ratio < 0.5 & SHAPE == "Fiber" ~ "Fiber",
          Aspect.Ratio >= 0.5 & Aspect.Ratio < 0.8 & SHAPE == "Fiber" ~ "Fragment",
          Aspect.Ratio >= 0.8 & Aspect.Ratio < 0.9 & SHAPE == "Fiber" ~ "Spherical Fragment",
          Aspect.Ratio >= 0.9 & SHAPE == "Fiber" ~ "Sphere",
          TRUE ~ "Unknown"
        )
      ) %>%
      group_by(PLASTIC_TYPE, Sample_Source) %>%
      summarise(PERCENT_SHAPE_CORRECT = mean(determined_shape == SHAPE, na.rm = TRUE) * 100)
    
    QCS_V_Shape_Summary <- full_join(QCS_SHAPE_details, QCS_V_Shape_Summary, by = "PLASTIC_TYPE") %>% drop_na(PLASTIC_TYPE)
  } else {
    QCS_V_Shape_Summary <- tibble(PLASTIC_TYPE = character(), Sample_Source = character(), PERCENT_SHAPE_CORRECT = numeric())
  }
  
  QCS_results <- list()
  if(length(QCS_samples) > 0 && nrow(QCS_V_Summary) > 0) {
    for (i in 1:length(QCS_samples)){
      QCS_results[[i]] <- QCS_samples[[i]] %>% 
        full_join(QCS_V_Summary, by = c("Identification")) %>%
        mutate(recovered_concentration_cc_mL = coalesce(recovered_concentration_cc_mL, 0)) %>%
        group_by(Identification, validated_concentration_cc_mL, recovered_concentration_cc_mL) %>%
        summarise(
          percent_recovery = if_else(
            validated_concentration_cc_mL > 0 & !is.na(validated_concentration_cc_mL), 
            (recovered_concentration_cc_mL / validated_concentration_cc_mL) * 100, 
            0.0
          ), .groups = 'drop')
      names(QCS_results)[i] <- names(QCS_samples)[i]
    }
  }
  
  QCS_Summary <- tibble(Identification = character(), validated_concentration_cc_mL = double())
  if(length(QCS_results) > 0) {
    for (i in 1:length(QCS_results)){
      QCS_Summary <- full_join(QCS_Summary, QCS_results[[i]] %>% ungroup() %>%
                                 rename(!!paste0("percent_recovery_",names(QCS_results[i])) := "percent_recovery",
                                        !!paste0("recovered_conc_",names(QCS_results[i])) := "recovered_concentration_cc_mL"),
                               by = c( "Identification", "validated_concentration_cc_mL"))
    }
    QCS_Summary <- QCS_Summary %>% left_join(QCS_MP_sizes, by = "Identification") %>% relocate(any_of(c("SHAPE", "SIZE_FRACTION")), .after = Identification) %>% mutate(across(where(is.numeric), ~ signif_pad(., 3)))
  } else {
    QCS_Summary <- data.frame(Message = "No QCS data found.", check.names = FALSE)
  }
  
  QCS_Validation_Report <- NULL
  if (nrow(QCS_V_Summary) > 0) {
    QCS_Validation_Report <- QCS_V_Summary %>% 
      dplyr::select(Identification, validated_concentration_cc_mL, Sample_Source) %>%
      rename(`PLASTIC_TYPE` = Identification, `VALIDATED_CONC (count/mL)` = validated_concentration_cc_mL) %>%
      left_join(QCS_V_Shape_Summary, by = c("PLASTIC_TYPE","Sample_Source")) %>%
      dplyr::select(Sample_Source, PLASTIC_TYPE, `VALIDATED_CONC (count/mL)`, PERCENT_SHAPE_CORRECT) %>%
      rename(`CORRECT_SHAPE_ASSIGNMENT (%)` = PERCENT_SHAPE_CORRECT) %>% mutate(across(where(is.numeric), ~ signif_pad(., 3)))
  }
  
  MethodBlanks <- list()
  if (length(filtered_data_AllMPs) > 0) {
    for(id_name in names(filtered_data_AllMPs)){
      if(grepl("MB", id_name) | grepl("OAB", id_name)){ 
        if(nrow(filtered_data_AllMPs[[id_name]]) > 0) {
          new_id_name <- paste0(id_name, " (cc)")
          MethodBlanks[[new_id_name]] <- filtered_data_AllMPs[[id_name]] %>% group_by(`Identification`) %>% summarise(count = n(), .groups = 'drop')
        }
      }
    }
  }
  
  MB_Summary <- tibble(Microplastics_Count = character())
  if (length(MethodBlanks) > 0) {
    for (i in 1:length(MethodBlanks)) {
      MB_ <- MethodBlanks[[i]] %>% rename(!!paste(names(MethodBlanks[i])) := "count", "Microplastics_Count" = "Identification")
      MB_Summary <- full_join(MB_Summary, MB_, by = "Microplastics_Count")
    }
  } else {
    MB_Summary <- data.frame(Microplastics_Count = "No blanks found.", check.names = FALSE)
  }
  
  OAQ_Samples <- list()
  OAQ_IDs <- sample_setup$Container.ID[sample_setup$Medium == "OAQ"]
  
  if (length(filtered_data_AllMPs) > 0 && length(OAQ_IDs) > 0) {
    for(id_name in names(filtered_data_AllMPs)){
      if(id_name %in% OAQ_IDs){
        if(nrow(filtered_data_AllMPs[[id_name]]) > 0) {
          raw_vol <- sample_setup$Filter.Vol[sample_setup$Container.ID == id_name]
          if(length(raw_vol) > 0 && !is.na(raw_vol) && raw_vol > 0) { vol <- raw_vol; suffix <- " (cc/L)" } else { vol <- 1; suffix <- " (cc)" }
          new_id_name <- paste0(id_name, suffix)
          OAQ_Samples[[new_id_name]] <- filtered_data_AllMPs[[id_name]] %>% group_by(`Identification`) %>% summarise(count = n(), .groups = 'drop') %>% mutate(final_val = count / vol) %>% dplyr::select(Identification, final_val)
        }
      }
    }
  }
  
  OAQ_Summary <- tibble(Microplastics_Count = character())
  if (length(OAQ_Samples) > 0) {
    for (i in 1:length(OAQ_Samples)) {
      OAQ_ <- OAQ_Samples[[i]] %>% rename(!!paste(names(OAQ_Samples[i])) := "final_val", "Microplastics_Count" = "Identification")
      OAQ_Summary <- full_join(OAQ_Summary, OAQ_, by = "Microplastics_Count")
    }
    OAQ_Summary <- OAQ_Summary %>% mutate(across(-Microplastics_Count, ~ if_else(is.na(.) | . == 0, "ND", signif_pad(as.numeric(.), 3))))
  } else {
    OAQ_Summary <- data.frame(Microplastics_Count = "No OAQ found.", check.names = FALSE)
  }
  
  Processed_Samples_Initial <- list(); OAQs_Initial <- list(); MBs_Initial <- list(); samples_for_summary <- list()
  if (length(filtered_data_AllMPs) > 0) {
    for (i in 1:length(filtered_data_AllMPs)){
      data_name <- names(filtered_data_AllMPs)[i]
      df <- filtered_data_AllMPs[[i]]
      if (nrow(df) == 0) next
      df_processed <- df %>% { if ("Width..µm." %in% names(.) && "Height..µm." %in% names(.)) { mutate(., `Aspect.Ratio` = pmin(`Width..µm.`, `Height..µm.`)/pmax(`Width..µm.`, `Height..µm.`)) } else { . } } %>%
        rename_with(~new_col_names) %>% dplyr::select(c(-1, -13, -14,-16)) %>% mutate(across(c(2:10, 12), as.numeric))
      
      is_blank_or_qc <- grepl("MB|QCS|OAB", data_name)
      is_duplicate <- grepl("_dup", data_name)
      is_oaq <- data_name %in% OAQ_IDs
      is_mb <- (grepl("MB", data_name) | grepl("OAB", data_name)) & !is_oaq
      
      if (!is_blank_or_qc & !is_duplicate & !is_oaq) {
        Processed_Samples_Initial[[data_name]] <- df_processed
        samples_for_summary[[data_name]] <- df_processed
      } else if (is_oaq) {
        OAQs_Initial[[data_name]] <- df_processed
      } else if (is_mb) {
        MBs_Initial[[data_name]] <- df_processed 
      }
    }
  }
  
  Sample_Summary <- tibble()
  if (length(samples_for_summary) > 0) {
    for (i in 1:length(samples_for_summary)){
      ID <- names(samples_for_summary)[i]; df_i <- samples_for_summary[[i]]
      Sample_Vol <- sample_setup$Filter.Vol[sample_setup$Container.ID == ID]
      Sample_Vol <- if(length(Sample_Vol) == 0 || is.na(Sample_Vol) || Sample_Vol == 0) 1 else Sample_Vol
      MP_Total_Conc <- nrow(df_i) / Sample_Vol
      MPType_Summary <- df_i %>% group_by(identification) %>% summarise(count = n(), .groups = 'drop') %>% mutate(concentration = (count/Sample_Vol)) %>% dplyr::select(-count)
      calculate_conc <- function(filter_condition) { count <- nrow(df_i %>% filter(!!filter_condition)); tibble(conc = count / Sample_Vol) }
      Size_Summary <- tibble(identification = c(paste0(Smallest, "-",Fraction1, "µm"), paste0(Fraction1, "-",Fraction2, "µm"), paste0(Fraction2, "-",Fraction3, "µm"), paste0(">", Fraction3,"µm")), concentration = c(calculate_conc(quote(diameter_um >= Smallest & diameter_um < Fraction1)), calculate_conc(quote(diameter_um >= Fraction1 & diameter_um < Fraction2)), calculate_conc(quote(diameter_um >= Fraction2 & diameter_um < Fraction3)), calculate_conc(quote(diameter_um >= Fraction3)) )) %>% unnest(concentration)
      Shape_Summary <- tibble(identification = c("Fiber", "Fibrous Fragment", "Fragment", "Spherical Fragment", "Sphere"), concentration = c(calculate_conc(quote(aspect_ratio < 0.35)), calculate_conc(quote(aspect_ratio >= 0.35 & aspect_ratio < 0.5 )), calculate_conc(quote(aspect_ratio >= 0.5 & aspect_ratio < 0.8)), calculate_conc(quote(aspect_ratio >= 0.8 & aspect_ratio < 0.9)), calculate_conc(quote(aspect_ratio >= 0.9)) )) %>% unnest(concentration)
      Total_MP_Summary = tibble(identification = "Total MPs", concentration = MP_Total_Conc)
      Compiled_Summary <- rbind(Total_MP_Summary, Size_Summary, Shape_Summary, MPType_Summary) %>% pivot_wider(names_from = identification, values_from = concentration) %>% add_column(Sample_ID = ID , .before = 1)
      Sample_Summary <- bind_rows(Sample_Summary, Compiled_Summary)
    }
    Sample_Summary <- Sample_Summary %>% mutate(across(.cols = 2:ncol(.), .fns = ~ if_else(is.na(.) | . == 0, "ND", signif_pad(as.numeric(.), 3))))
  } else {
    Sample_Summary <- data.frame(Sample_ID = "No valid data.", check.names = FALSE)
  }
  
  if (nrow(MB_Summary) > 0 && !any(grepl("No blanks found", MB_Summary[[1]]))) {
    if (length(samples_for_summary) > 0) {
      for (i in 1:length(samples_for_summary)) {
        sample_name <- names(samples_for_summary)[i]
        sample_counts <- samples_for_summary[[i]] %>% group_by(identification) %>% summarise(!!paste0(sample_name, " (cc)") := n(), .groups = 'drop') %>% rename(Microplastics_Count = identification)
        MB_Summary <- MB_Summary %>% left_join(sample_counts, by = "Microplastics_Count")
      }
      MB_Summary <- MB_Summary %>% mutate(across(-Microplastics_Count, ~ if_else(is.na(.) | . == 0, "ND", as.character(.))))
    }
  }
  
  Column_IDs <- colnames(Sample_Summary)
  if (Column_IDs[[1]] == "Sample_ID" && length(Column_IDs) > 1) { 
    definitions <- character(length(Column_IDs))
    Standard_definitions <- c("Sample_ID assigned by M3 Laboratory", paste0("Concentration of all microplastics >", Smallest, "µm in count/L"), paste0("Concentration of microplastics ≥ ", Smallest, "-", Fraction1, " µm in count/L"), paste0("Concentration of microplastics ≥ ", Fraction1, "-", Fraction2, " µm in count/L"), paste0("Concentration of microplastics ≥ ", Fraction2, "-", Fraction3, " µm in count/L"), paste0("Concentration of microplastics ≥", Fraction3, " µm in count/L"), "Concentration of fiber microplastics in count/L (further defined as having aspect ratio < 0.35)", "Concentration of fibrous fragment microplastics in count/L (further defined as having aspect ratio ≥ 0.35 & <0.5)", "Concentration of fragment microplastics in count/L (further defined as having aspect ratio ≥ 0.5 & <0.8)", "Concentration of spherical fragment microplastics in count/L (further defined as having aspect ratio ≥0.8 & <0.9)", "Concentration of spherical microplastics in count/L (further defined as having aspect ratio ≥0.9)")
    definitions[1:min(length(Standard_definitions), length(Column_IDs))] <- Standard_definitions[1:min(length(Standard_definitions), length(Column_IDs))]
    if(length(Column_IDs) > 11) { for(i in 12:length(Column_IDs)) { definitions[i] <- paste0("Concentration of ", Column_IDs[i], " microplastics in count/L") } }
    data_dictionary <- tibble(Column_ID = Column_IDs, Definition = definitions)
  } else {
    data_dictionary <- tibble(Column_ID = Column_IDs, Definition = "Error or no data.")
  }
  
  return(list( 
    MB_Summary = MB_Summary, 
    OAQ_Summary = OAQ_Summary, 
    QCS_Summary = QCS_Summary, 
    QCS_Validation_Report = QCS_Validation_Report, 
    QCS_V_Shape_Summary = QCS_V_Shape_Summary,
    Sample_Summary = Sample_Summary, 
    Processed_Samples_Initial = Processed_Samples_Initial, 
    MBs_Initial = MBs_Initial, 
    OAQs_Initial = OAQs_Initial, 
    sample_setup = sample_setup, 
    data_dictionary = data_dictionary, 
    LDIR_dictionary = LDIR_dictionary, 
    Microplastic_IDs = Microplastic_IDs 
  ))
}

# === FUNCTION 2: BLANK SUBTRACTION =========================
apply_blank_subtraction <- function(Processed_Samples_Initial, MBs_Initial, sample_setup_df, Smallest, Fraction1, Fraction2, Fraction3) {
  
  # 1. Calculate MB Master Profile
  mb_profiles <- list()
  if(length(MBs_Initial) > 0) {
    for(i in seq_along(MBs_Initial)) {
      df_mb <- MBs_Initial[[i]]
      
      MP_Total_Count <- nrow(df_mb)
      MPType_Count <- df_mb %>% group_by(identification) %>% summarise(count = n(), .groups = 'drop')
      
      calc_count <- function(filter_cond) { nrow(df_mb %>% filter(!!filter_cond)) }
      
      Size_Count <- tibble(
        identification = c(paste0(Smallest, "-",Fraction1, "µm"), paste0(Fraction1, "-",Fraction2, "µm"), paste0(Fraction2, "-",Fraction3, "µm"), paste0(">", Fraction3,"µm")),
        count = c(calc_count(quote(as.numeric(diameter_um) >= Smallest & as.numeric(diameter_um) < Fraction1)), calc_count(quote(as.numeric(diameter_um) >= Fraction1 & as.numeric(diameter_um) < Fraction2)), calc_count(quote(as.numeric(diameter_um) >= Fraction2 & as.numeric(diameter_um) < Fraction3)), calc_count(quote(as.numeric(diameter_um) >= Fraction3)))
      )
      
      Shape_Count <- tibble(
        identification = c("Fiber", "Fibrous Fragment", "Fragment", "Spherical Fragment", "Sphere"),
        count = c(calc_count(quote(as.numeric(aspect_ratio) < 0.35)), calc_count(quote(as.numeric(aspect_ratio) >= 0.35 & as.numeric(aspect_ratio) < 0.5 )), calc_count(quote(as.numeric(aspect_ratio) >= 0.5 & as.numeric(aspect_ratio) < 0.8)), calc_count(quote(as.numeric(aspect_ratio) >= 0.8 & as.numeric(aspect_ratio) < 0.9)), calc_count(quote(as.numeric(aspect_ratio) >= 0.9)))
      )
      
      Total_MP_Count = tibble(identification = "Total MPs", count = MP_Total_Count)
      
      mb_profiles[[i]] <- bind_rows(Total_MP_Count, Size_Count, Shape_Count, MPType_Count) %>% rename(!!names(MBs_Initial)[i] := count)
    }
    
    MB_Master <- reduce(mb_profiles, full_join, by = "identification") %>%
      mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
      rowwise() %>%
      mutate(avg_mb_count = mean(c_across(where(is.numeric)))) %>%
      ungroup() %>%
      dplyr::select(identification, avg_mb_count)
  } else {
    MB_Master <- tibble(identification = character(), avg_mb_count = numeric())
  }
  
  # 2. Process Samples with Subtraction
  Sample_Summary_Subtracted <- tibble()
  Corrected_Counts_Subtracted <- tibble()
  
  if (length(Processed_Samples_Initial) > 0) {
    for (i in 1:length(Processed_Samples_Initial)){
      ID <- names(Processed_Samples_Initial)[i]
      df_i <- Processed_Samples_Initial[[i]]
      
      Sample_Vol <- sample_setup_df$Filter.Vol[sample_setup_df$Container.ID == ID]
      Sample_Vol <- if(length(Sample_Vol) == 0 || is.na(Sample_Vol) || Sample_Vol == 0) 1 else Sample_Vol
      
      MP_Total_Count <- nrow(df_i)
      MPType_Count <- df_i %>% group_by(identification) %>% summarise(count = n(), .groups = 'drop')
      
      calc_count <- function(filter_cond) { nrow(df_i %>% filter(!!filter_cond)) }
      
      Size_Count <- tibble(
        identification = c(paste0(Smallest, "-",Fraction1, "µm"), paste0(Fraction1, "-",Fraction2, "µm"), paste0(Fraction2, "-",Fraction3, "µm"), paste0(">", Fraction3,"µm")), 
        count = c(calc_count(quote(as.numeric(diameter_um) >= Smallest & as.numeric(diameter_um) < Fraction1)), calc_count(quote(as.numeric(diameter_um) >= Fraction1 & as.numeric(diameter_um) < Fraction2)), calc_count(quote(as.numeric(diameter_um) >= Fraction2 & as.numeric(diameter_um) < Fraction3)), calc_count(quote(as.numeric(diameter_um) >= Fraction3)))
      )
      
      Shape_Count <- tibble(
        identification = c("Fiber", "Fibrous Fragment", "Fragment", "Spherical Fragment", "Sphere"), 
        count = c(calc_count(quote(as.numeric(aspect_ratio) < 0.35)), calc_count(quote(as.numeric(aspect_ratio) >= 0.35 & as.numeric(aspect_ratio) < 0.5 )), calc_count(quote(as.numeric(aspect_ratio) >= 0.5 & as.numeric(aspect_ratio) < 0.8)), calc_count(quote(as.numeric(aspect_ratio) >= 0.8 & as.numeric(aspect_ratio) < 0.9)), calc_count(quote(as.numeric(aspect_ratio) >= 0.9)))
      )
      
      Total_MP_Count = tibble(identification = "Total MPs", count = MP_Total_Count)
      Compiled_Counts <- bind_rows(Total_MP_Count, Size_Count, Shape_Count, MPType_Count)
      
      # Perform Subtraction & Re-calculate Concentration with "ND" and "≤ blank" logic
      if(nrow(MB_Master) > 0) {
        Compiled_Counts <- Compiled_Counts %>%
          full_join(MB_Master, by = "identification") %>%
          mutate(
            count = replace_na(count, 0),
            avg_mb_count = replace_na(avg_mb_count, 0),
            raw_count = count,
            subtracted_count = raw_count - avg_mb_count,
            subtracted_conc = subtracted_count / Sample_Vol,
            formatted_conc = case_when(
              is.na(raw_count) | raw_count == 0 ~ "ND",
              subtracted_count <= 0 ~ "≤ blank",
              TRUE ~ signif_pad(subtracted_conc, 3)
            ),
            formatted_count = case_when(
              is.na(raw_count) | raw_count == 0 ~ "ND",
              subtracted_count <= 0 ~ "≤ blank",
              TRUE ~ signif_pad(subtracted_count, 3)
            )
          )
      } else {
        Compiled_Counts <- Compiled_Counts %>% 
          mutate(
            raw_count = count,
            subtracted_conc = count / Sample_Vol,
            formatted_conc = case_when(
              is.na(raw_count) | raw_count == 0 ~ "ND",
              TRUE ~ signif_pad(subtracted_conc, 3)
            ),
            formatted_count = case_when(
              is.na(raw_count) | raw_count == 0 ~ "ND",
              TRUE ~ signif_pad(raw_count, 3)
            )
          )
      }
      
      Compiled_Conc <- Compiled_Counts %>% dplyr::select(identification, concentration = formatted_conc) %>% pivot_wider(names_from = identification, values_from = concentration) %>% add_column(Sample_ID = ID , .before = 1)
      Compiled_Count <- Compiled_Counts %>% dplyr::select(identification, count = formatted_count) %>% pivot_wider(names_from = identification, values_from = count) %>% add_column(Sample_ID = ID , .before = 1)
      
      Sample_Summary_Subtracted <- bind_rows(Sample_Summary_Subtracted, Compiled_Conc)
      Corrected_Counts_Subtracted <- bind_rows(Corrected_Counts_Subtracted, Compiled_Count)
    }
    
    # Catch missing cells introduced by bind_rows when polymer types differ across samples
    Sample_Summary_Subtracted <- Sample_Summary_Subtracted %>%
      mutate(across(-Sample_ID, ~ if_else(is.na(.) | . == "", "ND", as.character(.))))
    
    Corrected_Counts_Subtracted <- Corrected_Counts_Subtracted %>%
      mutate(across(-Sample_ID, ~ if_else(is.na(.) | . == "", "ND", as.character(.))))
    
  } else {
    Sample_Summary_Subtracted <- data.frame(Sample_ID = "No data remaining.", check.names = FALSE)
    Corrected_Counts_Subtracted <- data.frame(Sample_ID = "No data remaining.", check.names = FALSE)
  }
  
  return(list(Sample_Summary = Sample_Summary_Subtracted, Corrected_Counts = Corrected_Counts_Subtracted, Processed_Samples_Filtered = Processed_Samples_Initial)) 
}

# --- UI Definition ---

ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Waters: LDIR Data Processor & QA/QC Tool 🔬"),
  
  sidebarLayout(
    sidebarPanel(
      h3("1. Data Import"),
      fileInput("raw_files", "LDIR Raw Data Files (All .xlsx)", multiple = TRUE, accept = ".xlsx"),
      fileInput("setup_file", "Sample Setup File (SETUP.xlsx)", multiple = FALSE, accept = ".xlsx"),
      fileInput("qc_tracking_file", "QCS Tracking File (20XX_QCS_STD.xlsx)", multiple = FALSE, accept = ".xlsx"),
      hr(),
      h3("2. Run Initial Processing"),
      actionButton("run_initial", "Run Initial Processing & QAQC", class = "btn-primary"),
      hr(),
      h3("3. Final Processing & Export"),
      p("Click below to subtract the average Method Blank (MB) count from all sample polymer, size, and shape bins. Resulting negative values will default to 0."),
      actionButton("run_blank_sub", "Perform Blank Subtraction", class = "btn-warning"),
      hr(),
      downloadButton("export_data", "Export Final Results", class = "btn-success"),
      hr(),
      h3("Settings"),
      numericInput("hqi_threshold", "Minimum Acceptable HQI:", value = 0.8, min = 0, max = 1, step = 0.01),
      numericInput("smallest_size", "Smallest Particle Size (µm):", value = 20),
      numericInput("fraction1", "Fraction 1 Size (µm):", value = 50),
      numericInput("fraction2", "Fraction 2 Size (µm):", value = 100),
      numericInput("fraction3", "Fraction 3 Size (µm):", value = 200)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Initial QA/QC Review",
                 h3("Raw Counts by Polymer Bin"),
                 DTOutput("mb_table"), 
                 hr(),
                 h3("Raw Counts by Size Bin"),
                 DTOutput("size_raw_table"),
                 hr(),
                 h3("Raw Counts by Shape Bin"),
                 DTOutput("shape_raw_table"),
                 hr(),
                 h3("QCS Recovery Summary"),
                 DTOutput("qcs_table"), 
                 hr()
        ),
        tabPanel("QCS Validation Report",
                 h3("QCS Validation Report (Manual Update Data)"),
                 p("This table displays the final **validated concentrations (count/mL)** calculated from the QCS validation runs (`_V` samples). Analyst must manually copy/paste this data into the master QCS tracking file."),
                 DTOutput("qcs_validation_report_table") 
        ),
        tabPanel("Blank Corrected Data",
                 h3("Blank Corrected Sample Counts"),
                 p("These represent the blank corrected microplastics counts for each sample. The counts of microplastics (binned by polymer, size, and shape) in method blank samples were averaged and subtracted from sample counts.", 
                   style = "font-size: 14px; margin-bottom: 20px; font-style: italic;"),
                 hr(),
                 h4("Blank Corrected - Polymer"),
                 DTOutput("corrected_polymer_table"),
                 hr(),
                 h4("Blank Corrected - Size"),
                 DTOutput("corrected_size_table"),
                 hr(),
                 h4("Blank Corrected - Shape"),
                 DTOutput("corrected_shape_table")
        ),
        tabPanel("Sample Summary Preview",
                 h3("Sample Summary Table (Concentrations in count/L)"),
                 DTOutput("sample_summary_table") 
        )
      )
    )
  )
)

# --- Server Definition ---

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    mb_summary = NULL, oa_summary = NULL, qcs_summary = NULL,
    qcs_validation_report = NULL, sample_summary_initial = NULL, 
    sample_summary = NULL, corrected_counts = NULL, processed_samples_initial = NULL,
    processed_samples = NULL, oa_blanks_initial = NULL,
    mbs_initial = NULL, 
    sample_setup = NULL, data_dictionary = NULL, 
    LDIR_dictionary = NULL, Microplastic_IDs = NULL, qcs_v_shape_summary = NULL
  )
  
  active_qc_tracking_path <- reactiveVal(NULL)
  
  observeEvent(input$qc_tracking_file, {
    req(input$qc_tracking_file)
    active_qc_tracking_path(input$qc_tracking_file$datapath)
    showNotification("Initial QCS Tracking File loaded.", type = "message")
  })
  
  observeEvent(input$run_initial, {
    req(input$raw_files, input$setup_file, active_qc_tracking_path())
    raw_files_list <- input$raw_files$datapath
    names(raw_files_list) <- input$raw_files$name
    showModal(modalDialog("Processing initial data and running QA/QC...", footer = NULL))
    results <- tryCatch({
      run_all_processing(raw_files_list, input$setup_file$datapath, active_qc_tracking_path(), input$hqi_threshold, input$smallest_size, input$fraction1, input$fraction2, input$fraction3)
    }, error = function(e) { removeModal(); showNotification(paste("Error:", e$message), type = "error"); return(NULL) })
    removeModal()
    if (!is.null(results)) {
      rv$mb_summary <- results$MB_Summary; rv$oa_summary <- results$OAQ_Summary; rv$qcs_summary <- results$QCS_Summary; rv$qcs_validation_report <- results$QCS_Validation_Report 
      rv$sample_summary_initial <- results$Sample_Summary; rv$sample_summary <- rv$sample_summary_initial
      rv$processed_samples_initial <- results$Processed_Samples_Initial; rv$processed_samples <- rv$processed_samples_initial 
      rv$oa_blanks_initial <- results$OAQs_Initial; rv$mbs_initial <- results$MBs_Initial
      rv$sample_setup <- results$sample_setup; rv$data_dictionary <- results$data_dictionary; rv$LDIR_dictionary <- results$LDIR_dictionary; rv$Microplastic_IDs <- results$Microplastic_IDs
      rv$qcs_v_shape_summary <- results$QCS_V_Shape_Summary
      
      enable("run_blank_sub"); disable("export_data"); showNotification("Initial processing complete.", type = "message")
    }
  })
  
  observeEvent(input$run_blank_sub, {
    req(rv$processed_samples_initial, rv$sample_setup, rv$mbs_initial)
    showModal(modalDialog("Applying Method Blank Subtraction...", footer = NULL))
    filtered_results <- tryCatch({
      apply_blank_subtraction(rv$processed_samples_initial, rv$mbs_initial, rv$sample_setup, input$smallest_size, input$fraction1, input$fraction2, input$fraction3)
    }, error = function(e) { removeModal(); showNotification(paste("Error:", e$message), type = "error"); return(NULL) })
    removeModal()
    if (!is.null(filtered_results)) {
      rv$sample_summary <- filtered_results$Sample_Summary; rv$processed_samples <- filtered_results$Processed_Samples_Filtered 
      rv$corrected_counts <- filtered_results$Corrected_Counts
      showNotification("Blank subtraction applied. Export files updated.", type = "warning")
      if (!is.data.frame(rv$sample_summary) || !"Message" %in% names(rv$sample_summary)) { enable("export_data") } else { disable("export_data") }
    }
  })
  
  render_safe_datatable <- function(data_rv, message) {
    renderDT({
      df <- if (is.null(data_rv()) || (is.data.frame(data_rv()) && (nrow(data_rv()) == 0 || ("Message" %in% names(data_rv()) && ncol(data_rv()) == 1)))) {
        data.frame(Message = message, check.names = FALSE)
      } else {
        data_rv()
      }
      
      datatable(
        df,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,           
          scrollY = "500px",        
          scrollCollapse = TRUE,    
          paging = TRUE,            
          pageLength = 25,          
          dom = 'Bfrtip'            
        ),
        class = 'cell-border stripe' 
      )
    })
  }
  
  # Combined list of all raw datasets (MBs, OABs, Samples)
  all_datasets <- reactive({
    req(rv$processed_samples_initial)
    c(rv$mbs_initial, rv$oa_blanks_initial, rv$processed_samples_initial)
  })
  
  # Polymer Table (Combines MBs, OABs, and Samples)
  combined_blanks_for_ui <- reactive({
    mb <- rv$mb_summary
    oaq <- rv$oa_summary
    
    is_real <- function(df) {
      !is.null(df) && is.data.frame(df) && nrow(df) > 0 && !any(grepl("No blanks|No OAQ", df[[1]]))
    }
    
    df_out <- if (is_real(mb) && is_real(oaq)) {
      full_join(mb, oaq, by = "Microplastics_Count")
    } else if (is_real(mb)) {
      mb
    } else if (is_real(oaq)) {
      oaq
    } else {
      NULL 
    }
    
    if(!is.null(df_out)) {
      df_out %>% mutate(across(-Microplastics_Count, ~ case_when(
        is.na(.) | . %in% c("0", "0.00", "NULL", 0) ~ "ND",
        TRUE ~ as.character(.)
      )))
    } else {
      NULL
    }
  })
  
  # Raw Size Bin Table
  raw_size_combined <- reactive({
    req(all_datasets())
    datasets <- all_datasets()
    if (length(datasets) == 0) return(NULL)
    
    Smallest <- input$smallest_size
    Fraction1 <- input$fraction1
    Fraction2 <- input$fraction2
    Fraction3 <- input$fraction3
    
    size_labels <- c(
      paste0(Smallest, "-", Fraction1, "µm"),
      paste0(Fraction1, "-", Fraction2, "µm"),
      paste0(Fraction2, "-", Fraction3, "µm"),
      paste0(">", Fraction3, "µm")
    )
    
    res <- tibble(Size_Bin = size_labels)
    
    for (id in names(datasets)) {
      df <- datasets[[id]]
      col_name <- paste0(id, " (cc)")
      
      diameters <- as.numeric(df$diameter_um)
      
      counts <- c(
        sum(diameters >= Smallest & diameters < Fraction1, na.rm = TRUE),
        sum(diameters >= Fraction1 & diameters < Fraction2, na.rm = TRUE),
        sum(diameters >= Fraction2 & diameters < Fraction3, na.rm = TRUE),
        sum(diameters >= Fraction3, na.rm = TRUE)
      )
      
      res[[col_name]] <- if_else(counts == 0, "ND", as.character(counts))
    }
    return(res)
  })
  
  # Raw Shape Bin Table
  raw_shape_combined <- reactive({
    req(all_datasets())
    datasets <- all_datasets()
    if (length(datasets) == 0) return(NULL)
    
    shape_labels <- c("Fiber", "Fibrous Fragment", "Fragment", "Spherical Fragment", "Sphere")
    
    res <- tibble(Shape_Bin = shape_labels)
    
    for (id in names(datasets)) {
      df <- datasets[[id]]
      col_name <- paste0(id, " (cc)")
      
      ratios <- as.numeric(df$aspect_ratio)
      
      counts <- c(
        sum(ratios < 0.35, na.rm = TRUE),
        sum(ratios >= 0.35 & ratios < 0.5, na.rm = TRUE),
        sum(ratios >= 0.5 & ratios < 0.8, na.rm = TRUE),
        sum(ratios >= 0.8 & ratios < 0.9, na.rm = TRUE),
        sum(ratios >= 0.9, na.rm = TRUE)
      )
      
      res[[col_name]] <- if_else(counts == 0, "ND", as.character(counts))
    }
    return(res)
  })
  
  # --- Blank Corrected Splits ---
  corrected_polymer <- reactive({
    req(rv$corrected_counts)
    cols_size <- c("Sample_ID", paste0(input$smallest_size, "-", input$fraction1, "µm"), paste0(input$fraction1, "-", input$fraction2, "µm"), paste0(input$fraction2, "-", input$fraction3, "µm"), paste0(">", input$fraction3, "µm"))
    cols_shape <- c("Sample_ID", "Fiber", "Fibrous Fragment", "Fragment", "Spherical Fragment", "Sphere")
    rv$corrected_counts %>% dplyr::select(-any_of(c(cols_size[-1], cols_shape[-1], "Total MPs")))
  })
  
  corrected_size <- reactive({
    req(rv$corrected_counts)
    cols_size <- c("Sample_ID", paste0(input$smallest_size, "-", input$fraction1, "µm"), paste0(input$fraction1, "-", input$fraction2, "µm"), paste0(input$fraction2, "-", input$fraction3, "µm"), paste0(">", input$fraction3, "µm"))
    rv$corrected_counts %>% dplyr::select(any_of(cols_size))
  })
  
  corrected_shape <- reactive({
    req(rv$corrected_counts)
    cols_shape <- c("Sample_ID", "Fiber", "Fibrous Fragment", "Fragment", "Spherical Fragment", "Sphere")
    rv$corrected_counts %>% dplyr::select(any_of(cols_shape))
  })
  
  # --- UI Outputs ---
  output$mb_table <- render_safe_datatable(combined_blanks_for_ui, "No MB or OAB blanks found.")
  output$size_raw_table <- render_safe_datatable(raw_size_combined, "No raw size data available.")
  output$shape_raw_table <- render_safe_datatable(raw_shape_combined, "No raw shape data available.")
  output$qcs_table <- render_safe_datatable(reactive(rv$qcs_summary), "No QCS samples found.")
  
  output$qcs_validation_report_table <- render_safe_datatable(reactive(rv$qcs_validation_report), "No QCS Validation report.")
  
  output$corrected_polymer_table <- render_safe_datatable(corrected_polymer, "Awaiting blank subtraction.")
  output$corrected_size_table <- render_safe_datatable(corrected_size, "Awaiting blank subtraction.")
  output$corrected_shape_table <- render_safe_datatable(corrected_shape, "Awaiting blank subtraction.")
  
  output$sample_summary_table <- render_safe_datatable(reactive(rv$sample_summary), "No Sample Summary data.")
  
  output$export_data <- downloadHandler(
    filename = function() { paste0("Microplastic_Analysis_Export_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip") },
    content = function(file) {
      req(rv$sample_summary, rv$processed_samples, rv$oa_blanks_initial, rv$mb_summary, rv$qcs_summary, rv$oa_summary, rv$sample_setup, rv$data_dictionary, rv$LDIR_dictionary)
      temp_dir <- tempfile(); dir.create(temp_dir)
      
      wb1 <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb1, "Sample_Summary"); openxlsx::writeData(wb1, "Sample_Summary", rv$sample_summary)
      
      if(!is.null(rv$corrected_counts) && nrow(rv$corrected_counts) > 0) {
        openxlsx::addWorksheet(wb1, "Blank_Corrected_Counts"); openxlsx::writeData(wb1, "Blank_Corrected_Counts", rv$corrected_counts)
      }
      
      openxlsx::addWorksheet(wb1, "Blanks_Summary"); openxlsx::writeData(wb1, "Blanks_Summary", rv$mb_summary)
      openxlsx::addWorksheet(wb1, "OAQ_Summary"); openxlsx::writeData(wb1, "OAQ_Summary", rv$oa_summary)
      openxlsx::addWorksheet(wb1, "QCS_Summary"); openxlsx::writeData(wb1, "QCS_Summary", rv$qcs_summary)
      openxlsx::addWorksheet(wb1, "Sample_Info"); openxlsx::writeData(wb1, "Sample_Info", rv$sample_setup)
      openxlsx::addWorksheet(wb1, "Data_Dictionary"); openxlsx::writeData(wb1, "Data_Dictionary", rv$data_dictionary)
      openxlsx::saveWorkbook(wb1, file.path(temp_dir, "Summary.xlsx"), overwrite = TRUE)
      
      files_to_export <- c(rv$processed_samples, rv$oa_blanks_initial) 
      lapply(names(files_to_export), function(name) {
        df <- files_to_export[[name]]; wb <- openxlsx::createWorkbook()
        if(is.data.frame(df) && !"Message" %in% names(df) && nrow(df) > 0) {
          openxlsx::addWorksheet(wb, name); openxlsx::writeData(wb, name, df)
          openxlsx::addWorksheet(wb, "Data_Dictionary"); openxlsx::writeData(wb, "Data_Dictionary", rv$LDIR_dictionary)
          openxlsx::saveWorkbook(wb, file.path(temp_dir, paste0(name, ".xlsx")), overwrite = TRUE)
        }
      })
      utils::zip(zipfile = file, files = list.files(temp_dir, full.names = TRUE), flags = "-j")
      unlink(temp_dir, recursive = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)