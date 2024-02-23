#'
#' Create data dictionary for children dataset
#'
#' @param df Data.frame object for child dataset
#' @param keep Should a CSV copy of the data dictionary be saved in the `data`
#'   directory? Default is FALSE
#' @param overwrite If keep is TRUE, should an existing saved copy of the
#'   data dictionary be overwritten? Default is FALSE.
#'
#' @return A data.frame with 3 columns: `field`, `field_type`, `field_description`
#' 
#' 

create_child_dictionary <- function(df = child, keep = FALSE, overwrite = FALSE) {
  field <- names(df)
  field_type <- lapply(df, class) |> unlist()
  field_description <- c(
    "Unique state identifier",
    "State name",
    "Unique locality identifier",
    "Locality name",
    "Primary sampling unit",
    "Unique user identifier",
    "Age of child in months",
    "Sex of child: 1 = Male; 2 = Female",
    "Has child had diarrhoea in the past 2 weeks?: 1 = Yes; 0 = No",
    "Has child had cough in the past 2 weeks?: 1 = Yes; 0 = No",
    "Has child had fever in the past 2 weeks?: 1 = Yes; 0 = No",
    "Does the child have health insurance?: 1 = Yes; 0 = No",
    "Did the child sleep under an insecticide treated net last night?: 1 = Yes; 0 = No",
    "Did the child get measured with a MUAC tape in the past month?: 1 = Yes; 0 = No",
    "Did the child get measured with a MUAC tape in the past three months?: 1 = Yes; 0 = No",
    "Did the child's height and weight get measured in the healt clinci in the past month?: 1 = Yes; 0 = No",
    "Did the child's mother attend infant and young child feeding counselling in the past month?: 1 = Yes; 0 = No",
    "Child's MUAC in millimetres",
    "Child's height-for-age z-score",
    "Child's weight-for-age z-score",
    "Child's weight-for-height z-score",
    "Does the child have bipedal oedema?: 1 = Yes; 0 = No",
    "Does the child have vaccination card/record?: 1 = Yes; 0 = No",
    "Has the child had BCG vaccination?: 1 = Yes; 0 = No",
    "Has the child had first Pentavalent vaccination?: 1 = Yes; 0 = No",
    "Has the child had third Pentavalent vaccination?: 1 = Yes; 0 = No",
    "Has the child had first oral polio vaccination?: 1 = Yes; 0 = No",
    "Has the child had third oral polio vaccination?: 1 = Yes; 0 = No",
    "Has the child had first measles vaccination?: 1 = Yes; 0 = No",
    "Has the child had third measles vaccination?: 1 = Yes; 0 = No",
    "Has the child had full set of vaccinations (except rotavirus)?: 1 = Yes; 0 = No",
    "In the past 6 months, has the child received vitamin A drops/supplementation?: 1 = Yes; 0 = No",
    "Is the child attending pre-school?: 1 = Yes; 0 = No",
    "Is the child attending basic education/primary?: 1 = Yes; 0 = No",
    "Is the child attending pre-school or basic education/primary?: 1 = Yes; 0 = No",
    "Is the child attending formal education and madrasah/Arabic school?: 1 = Yes; 0 = No",
    "Has the child ever attended school?: 1 = Yes; 0 = No",
    "Reason for not attending school: No school in village/area",
    "Reason for not attending school: No water and sanitation facilities in school",
    "Reason for not attending school: Child has illness and/or disability",
    "Reason for not attending school: School is too far",
    "Reason for not attending school: Child is working",
    "Reason for not attending school: Child has married early",
    "Reason for not attending school: Cannot pay for school fees",
    "Reason for not attending school: Child and family are currently displaced",
    "Reason for not attending school: Other reasons"
  )
  
  dict <- data.frame(field, field_type, field_description) |>
    (\(x) { row.names(x) <- NULL; x } )()
  
  if (keep) {
    if (overwrite) {
      write.csv(x = dict, file = "data/child_dictionary.csv", row.names = FALSE)
    } else {
      if (length(list.files("data", pattern = "child_dictionary")) == 0) {
        write.csv(x = dict, file = "data/child_dictionary.csv", row.names = FALSE)
      } else {
        message("An existing copy of the child data dictionary is already available and has not been overwritten.")
     }
    }
  }
  
  dict
}


#'
#' Create data dictionary for maternal dataset
#'
#' @param df Data.frame object for maternal dataset
#' @param keep Should a CSV copy of the data dictionary be saved in the `data`
#'   directory? Default is FALSE
#' @param overwrite If keep is TRUE, should an existing saved copy of the
#'   data dictionary be overwritten? Default is FALSE.
#'
#' @return A data.frame with 3 columns: `field`, `field_type`, `field_description`
#' 
#' 

create_maternal_dictionary <- function(df = maternal, keep = FALSE, overwrite = FALSE) {
  field <- names(df)
  field_type <- lapply(df, class) |> unlist()
  field_description <- c(
    "Unique state identifier",
    "State name",
    "Unique locality identifier",
    "Locality name",
    "Primary sampling unit",
    "Unique universal identifier",
    "Gestational age at first antenatal care visit for most recent pregnancy",
    "Number of antenatal visits for most recent pregnancy",
    "At least 4 antenatal visits during most recent pregnancy",
    "Took any amount of iron folic acid tablets during most recent pregnancy",
    "Took at least 90 iron folic acid tablets during most recent pregnancy",
    "Received vitamin A supplementation during most recent pregnancy",
    "Attended post-natal care after giving birth during most recent pregnancy",
    "Received tetanus doses after giving birth during most recent pregnancy",
    "Maternal MUAC in millimetres",
    "Married at 15 years old or younger",
    "First pregnancy at 15 to 19 years old",
    "Age (in years) at first pregnancy",
    "Minimum dietary diversity for women",
    "Consumed protein-rich food items in the past 24 hours",
    "Consumed vitamin A-rich food items in the past 24 hours",
    "Consumed iron-rich food items in the past 24 hours",
    "Consumed calcium-rich food items in the past 24 hours",
    "Consumed zinc-rich food items in the past 24 hours",
    "Consumed vitamin B1-rich food items in the past 24 hours",
    "Consumed vitamin B2-rich food items in the past 24 hours",
    "Consumed vitamin B3-rich food items in the past 24 hours",
    "Consumed vitamin B6-rich food items in the past 24 hours",
    "Consumed vitamin B complex-rich food items in the past 24 hours"
  )
  
  dict <- data.frame(field, field_type, field_description) |>
    (\(x) { row.names(x) <- NULL; x } )()
  
  if (keep) {
    if (overwrite) {
      write.csv(x = dict, file = "data/maternal_dictionary.csv", row.names = FALSE)
    } else {
      if (length(list.files("data", pattern = "maternal_dictionary")) == 0) {
        write.csv(x = dict, file = "data/maternal_dictionary.csv", row.names = FALSE)
      } else {
        message("An existing copy of the maternal data dictionary is already available and has not been overwritten.")
      }
    }
  }
  
  dict
}


#'
#' Create data dictionary for CMAM dataset
#'
#' @param df Data.frame object for CMAM dataset
#' @param keep Should a CSV copy of the data dictionary be saved in the `data`
#'   directory? Default is FALSE
#' @param overwrite If keep is TRUE, should an existing saved copy of the
#'   data dictionary be overwritten? Default is FALSE.
#'
#' @return A data.frame with 3 columns: `field`, `field_type`, `field_description`
#' 
#' 

create_cmam_dictionary <- function(df = cmam, keep = FALSE, overwrite = FALSE) {
  field <- names(df)
  field_type <- lapply(df, class) |> unlist()
  field_description <- c(
    "State name",
    "Locality name",
    "Number of admissions carried over from previous month",
    "Number of new admissions",
    "Number of new admissions who are male",
    "Number of new admissions who are female",
    "Number of patients discharged cured",
    "Number of patients who have died",
    "Number of patients who have defaulted",
    "Number of patients who did not respond to treatment",
    "Number of patients who have been discharged",
    "Number of ready-to-use therapeutic food that has been distributed/consumed",
    "Number of children screened",
    "Number of treatments sites",
    "Month",
    "Year"
  )
  
  dict <- data.frame(field, field_type, field_description) |>
    (\(x) { row.names(x) <- NULL; x } )()
  
  if (keep) {
    if (overwrite) {
      write.csv(x = dict, file = "data/cmam_dictionary.csv", row.names = FALSE)
    } else {
      if (length(list.files("data", pattern = "cmam_dictionary")) == 0) {
        write.csv(x = dict, file = "data/cmam_dictionary.csv", row.names = FALSE)
      } else {
        message("An existing copy of the CMAM data dictionary is already available and has not been overwritten.")
      }
    }
  }
  
  dict
}
