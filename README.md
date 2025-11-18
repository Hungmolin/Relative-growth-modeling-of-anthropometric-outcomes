# Relative-growth-modeling-of-anthropometric-outcomes
Traditionally z-scores specified from the WHO population growth curves have been used to describe a child’s growth in relation to his age- and sex-matched population distribution. We propose a new regression approach that offers a straightforward interpretation of the relative growth in terms of the original anthropometric variable. We create a hybrid data set consisting of the observations from the study of interest and counterpart pseudo-population observations imputed from the WHO population growth curves matched to each study participant. We then fit linear and quantile regres- sion models to the hybrid data incorporating demographic variables (usually age and biological sex) corresponding to the growth curves of demographically-similar individuals, a study versus population indicator, and its interactions with demographic variables. We further control for confounding variables from the study by adding their interactions with the study indicator variable. The interaction terms between the study indicator and the demographic variables age and biologic sex can be interpreted as relative growth parameters that depict the differences in means (or quantiles) between the study participants and their pseudo-population counterparts of the original anthropometric variables, rather than the associated z-scores. We use anthropometric growth data from a prospective birth cohort study conducted in Uganda for illustration.

This site contains information on the SAS program and data sets used to created the tables in the Journal of Data Science paper: "Relative growth modeling of anthropometric outcomes" 

*********************************************;
* SAS program                                ;
*********************************************;

1. Uganda Analysis.sas
	SAS program for Section 3 Uganda Birth Cohort Study analysis


*********************************************;
* Data sets for Figures                      ;
*********************************************;

1. Uganda SAS data sets used in Uganda Analysis.sas:

	. module_1_household_characteristics.sas7bdat (visit number, household information)
	. module_1_household_head.sas7bdat	(household head education and marriage status)
	. module_4_health_status.sas7bdat	(HIV status)
	. module_7_child_feeding.sas7bdat	(breast feeding status)
	. module_13_index_child_anthro.sas7bdat	(study subject's weights and lengths)
	. module_13_index_woman_anthro.sas7bdat	(mother's height)
	. module_14_woman_laboratory_test.sas7bdat (HIV)

2. WHO growth curve SAS data sets:
	. weianthro.sas7bdat (for weight)
	. lenanthro.sas7bdat (for height)

