* Uganda Analysis.sas;
* 2 Regions, 12 Districts, 16 Subdistricts;
* Should end up being 15,836 visits according to paper;
 
   
%let path=C:\Uganda;
libname xxx "&path\Data";                                                      
libname who "&path\Data";  

options orientation=portrait;
%let opt=%str(options nonotes nosource nosource2 nomprint errors=2 nosymbolgen nomlogic ps=60 ls=132;; );
%let optback=%str(options notes source source2 ps=60 ls=132;;);

%let today=%sysfunc(today());
%let format_today=%sysfunc(putn(&today, EURDFDE9.));

PROC FORMAT;
   VALUE sexfmt
         1 = 'Girl'
         2 = 'Boy'
   ;
   VALUE WHOsexfmt
         1 = 'Boy'
         2 = 'Girl'
   ;
   VALUE momage5catfmt
         1 = '< yrs'
         2 = '20-24 yrs'
		 3 = '25-29 yrs'
		 4 = '30-34 yrs'
		 5 = '>= 35 yrs'
   ;
   VALUE momage3catfmt
         1 = '< 20 yrs'
         2 = '20-29 yrs'
		 3 = '30+ yrs'
   ;
   VALUE momhtcatfmt
         1 = '< 145 cm'
		 2 = '145 - 149.9 cm'
		 3 = '150 - 154.9 cm'
		 4 = '155 - 159.9 cm'
		 5 = '160+ cm'
   ;
   VALUE pateducfmt
         0 = '0-6 years'
		 1 = '7 or more years'
   ;
   VALUE maritalfmt
         0 = 'Not married'
		 1 = 'Married'
   ;
run;

* HOUSEHOLD CHARACTERISTICS: N = 9068, two visits per HH;
* PROC CONTENTS DATA=xxx.module_1_household_characteris; run; 
DATA HOUSEHOLD;
   SET xxx.module_1_household_characteris;    
   * Subsetting on visit number;
   if (. < hvisnum_cl <= 7); 
run;
PROC SORT DATA=HOUSEHOLD; by hhid hvisnum_cl; run;
/*PROC FREQ DATA=HOUSEHOLD;
   TABLES hvisnum_cl hdist hwall hrwater htoilet;
run;*/



* HOUSEHOLD HEAD: N = 4949, one record per household;				* NEED TO CARRY DOWN BOTH VARIABLES!!!!!!!!!!!!!!!!!!!!!;
*PROC CONTENTS DATA=xxx.module_1_household_head; run; 
DATA HOUSEHOLD_HEAD;
   SET xxx.module_1_household_head;  
   FORMAT pateduc pateducfmt. married maritalfmt.;
   * Creating PATERNAL EDUCATION variable: 0 for 0 to 6 years, 1 for 7+ years;
   pateduc = .;
   if (. < hhdeduc < 7) then pateduc = 0; 
   if (7 <= hhdeduc )   then pateduc = 1; 
   * Creating MARITAL STATUS variable - married = 0,1;
   if (hcgmarts = 1)       then married = 1; 
   if (2 <= hcgmarts <= 7) then married = 0; 
   label pateduc = 'Paternal Education';
run;
PROC SORT DATA=HOUSEHOLD_HEAD; by hhid; run;

* HEALTH STATUS of MOTHER;
*PROC CONTENTS DATA=xxx.module_4_health_status; run; 
DATA HEALTH_STATUS;
   SET xxx.module_4_health_status;    
   keep hhid hvisnum_cl manyill_cl mtreat_cl mnetiwoman;
   * Subsetting on visit number;
   if (. < hvisnum_cl <= 7); * Ends up with 24,709 visits;
run;
PROC SORT DATA=HEALTH_STATUS; by hhid hvisnum_cl; run;


* ANTHROPOMETRIC DATA - MOTHER;
* Visits 1, 3, 4, 5, 6, 7
PROC CONTENTS DATA=xxx.module_13_index_woman_anthro; run; 
DATA WOMAN_ANTHRO;
   SET xxx.module_13_index_woman_anthro;      
   FORMAT acsex asex_cl sexfmt. momagecat5 momage5catfmt. momagecat3 momage3catfmt. momhtcat momhtcatfmt.;
   * Subsetting on visit number;
   if (. < hvisnum_cl <= 7); * Ends up with 22,795 visits;
   * Creating BMI variable;
   *woman_bmi = acwt_mean / ((amht_mean_cl/100)**2);
   * Creating categorical variable for MOTHER'S AGE (5 categories);
   momagecat5 = .;
   if (15 <= amyrs_cl < 20) then momagecat5 = 1;
   if (20 <= amyrs_cl < 25) then momagecat5 = 2;
   if (25 <= amyrs_cl < 30) then momagecat5 = 3;
   if (30 <= amyrs_cl < 35) then momagecat5 = 4;
   if (35 <= amyrs_cl)      then momagecat5 = 5;
   momagecat3 = .;
   if (15 <= amyrs_cl < 20) then momagecat3 = 1;
   if (20 <= amyrs_cl < 30) then momagecat3 = 2;
   if (30 <= amyrs_cl)      then momagecat3 = 3;
   * Creating categorical variable for MOTHER'S HEIGHT (5 categories);
   momhtcat = .;
   if (.   <  amht_mean_cl < 145) then momhtcat = 1;
   if (145 <= amht_mean_cl < 150) then momhtcat = 2;
   if (150 <= amht_mean_cl < 155) then momhtcat = 3;
   if (155 <= amht_mean_cl < 160) then momhtcat = 4;
   if (160 <= amht_mean_cl)       then momhtcat = 5;
   label momhtcat = 'Mothers Height' momagecat5 = 'Mothers Age' momagecat3 = 'Mothers Age';
run;
PROC SORT DATA=WOMAN_ANTHRO; by hhid hvisnum_cl; run;


* Merging WOMEN ANTHRO with HOUSEHOLD HEAD data - Ends up with 22,795 observations;
DATA WOMENANTHRO_HOUSEHEAD;
   MERGE WOMAN_ANTHRO (in=a) HOUSEHOLD_HEAD (in=b);
   by hhid; 
   if a or b;
run;

* CHILD FEEDING - N = 17,944, has breast feeding variable;
*PROC CONTENTS DATA=xxx.module_7_child_feeding; run;
DATA CHILD_FEEDING;
   SET xxx.module_7_child_feeding; 
   * Subsetting on visit number;
   if (. < hvisnum_cl <= 7); 
run;
PROC SORT DATA=CHILD_FEEDING; by hhid hvisnum_cl; run;


* Merging WOMEN ANTHRO, HOUSEHOLD HEAD, and FEEDING data - ends up with N = 22,906;
PROC SORT DATA=WOMENANTHRO_HOUSEHEAD; by hhid hvisnum_cl; run;
DATA WOMENANTHRO_HOUSEHEAD2;
   MERGE WOMENANTHRO_HOUSEHEAD (in=a) CHILD_FEEDING (in=b);
   by hhid hvisnum_cl; 
   if a or b;
run;

* MOTHER LAB TEST - Includes hemoglobin test - N = 14,219;
* Visit numbers 1, 3, 5, 7;
*PROC CONTENTS DATA=xxx.module_14_woman_laboratory_test; run;
DATA WOMAN_LAB;
   SET xxx.module_14_woman_laboratory_test; 
   keep hhid hvisnum_cl blheamcg blheamcgre;
   * Creating clean version of visit number variable;
   hvisnum_cl = hvisnum;
run;
PROC SORT DATA=WOMAN_LAB; by hhid hvisnum_cl; run;

* ANTHROPOMETRIC DATA - Children - N = 17,533;
*PROC CONTENTS DATA=xxx.module_13_index_child_anthro; run; 
DATA CHILD_ANTHRO;
   SET xxx.module_13_index_child_anthro;   
   * Creating clean version of visit number variable;
   hvisnum_cl = hvisnum_clean;
    * Creating BMI variable;
   chld_bmi = acwt_mean / ((acleg_mean_clean/100)**2);
   * Creating a SEX variable aligned with WHO definition;
   sex = 3 - asex_cl;
   FORMAT acsex asex_cl sexfmt. sex WHOsexfmt.;
run;
PROC SORT DATA=CHILD_ANTHRO; by hhid hvisnum_cl; run;


* Merging WOMENANTHRO_HOUSEHEAD with CHILD ANTHRO;
PROC SORT DATA=WOMENANTHRO_HOUSEHEAD2; by hhid hvisnum_cl; run;
DATA WOMEN_CHILD_ANTHRO;
   MERGE WOMENANTHRO_HOUSEHEAD2 (in=a) CHILD_ANTHRO (in=b);
   by hhid hvisnum_cl; 
   if a or b;  * Using observations where there is any data;
run;



* Sorting study data and WHO anthrometric data sets by age and sex;
* Outputting sorted data set WOMEN_CHILD_ANTHRO to the data set ALL;
PROC SORT DATA=WOMEN_CHILD_ANTHRO(rename=(agedays_clean=age)) OUT=ALL; by age sex; run;
PROC SORT DATA=who.weianthro; by age sex; run;
PROC SORT DATA=who.lenanthro; by age sex; run;
PROC SORT DATA=who.bmianthro; by age sex; run;

DATA ALL;
   MERGE ALL (in=x1 rename=(acwt_mean=chld_weight acleg_mean_clean=chld_height))
      who.weianthro (KEEP=SEX AGE L S M rename=(L=L_WEIGHT M=M_WEIGHT S=S_WEIGHT))
      who.lenanthro (KEEP=SEX AGE L S M rename=(L=L_LENGTH M=M_LENGTH S=S_LENGTH))
      who.bmianthro (KEEP=SEX AGE L S M rename=(L=L_BMI    M=M_BMI    S=S_BMI));
   by age sex;
   * Removing records with missing ID, age, or gender variables;
   if hhid ne ' ' and age ne . and sex ne .;
run;



*********************************************************************************************************;

* Creating a POPULATION data set (number of Blom points * study sample size) matched by gender and age;
* Only 1 BLOM point at the mean level;

DATA ALL2 (drop=z i);
   SET ALL;
   by age sex;
   NBlom = 1;  * Number of blom points;
   ARRAY s[*] s_weight s_length s_bmi;
   ARRAY l[*] l_weight l_length l_bmi;
   ARRAY m[*] m_weight m_length m_bmi;
   ARRAY o[*] WEIGHT   height   BMI;               * Population values;
   ARRAY a[*] chld_weight chld_height chld_bmi;    * Study values;
   ARRAY d[*] weight_diff height_diff bmi_diff;    * Study values - population values;
   * Recalculating anthropometic values, the differences between study and population;
   DO BlomID = 1 to Nblom;
      *z = quantile('normal', (BlomID - 0.375)/(NBlom + 0.25));
       *z = 0;
       z = rand('normal', 0, 1);
	  do i=1 to dim(a);
         if L[i] ne 0 then o[i] = M[i]*((1+Z*S[i]*L[i])**(1/L[i]));
         else if L[i] = 0 then o[i] = M[i]*exp(S[i]*Z);
         d[i] = a[i] - o[i];
         if d[i] = . then do; 
            o[i] = .; 
            a[i] = .; 
         end;  
      end;
      OUTPUT;
   END;
run;


/*************************************************************
** UGANDA_ALL dataset contains 1 + Nblom obs per study subject
**************************************************************/
DATA UGANDA_ALL;
   SET ALL2(in=x where=(BLOMID=1)
            drop = WEIGHT height BMI weight_diff height_diff bmi_diff 
            rename=(chld_weight=WEIGHT chld_height=HEIGHT chld_bmi=BMI))
       ALL2(drop=  chld_weight chld_height chld_bmi);
   * Create study indicator (study) and assign weight (study_wgt)=1 if study, 
     and 1/(# of Blom points) if population; 
   IF x THEN DO;
      study = 1; 
      final_wgt = 1; 
   END;
   ELSE DO; 
      study = 0; 
      final_wgt = 1 / NBlom;  * Weighting each observation as inverse of the number of Blom points = 1/Nblom;
   END;
   * Creating a binary variable for BOY;
   if sex = 2 then boy = 0;
   if sex = 1 then boy = 1;
   * Creating an AGE IN MONTHS variable for CHILD AGE;
   age_months = (age * 12) / 365;
   * Creating REGION variable: 1 for North and 0 for South Western;
   North = 1;
   if hdist in ('Kabale','Kabarole','Kamwenge','Kanungu','Rukungiri') then North = 0; 

   if amyrs_cl ne . then do;
   	momagecat_lt20 = (15 <= amyrs_cl < 20);
   	momagecat_lt30 = (20 <= amyrs_cl < 30);
   	momagecat_ge30 = (30 <= amyrs_cl);
   end;

	if amht_mean_cl ne  . then do;
   		momhtcat_lt145 = (.   <  amht_mean_cl < 145);
	   	momhtcat_lt150 = (145 <= amht_mean_cl < 150);
   		momhtcat_lt155 = (150 <= amht_mean_cl < 155);
   		momhtcat_lt160 = (155 <= amht_mean_cl < 160) ;
   		momhtcat_ge160 = (160 <= amht_mean_cl)  ;
   end;

run;

proc contents; run;

proc means data=uganda_all (where=(hvisnum_cl=4 and study=1)) noprint; 
var north amht_mean_cl momagecat_lt20 momagecat_ge30  pateduc married  fbnow;
output out=cov_mean mean=north amht_mean_cl momagecat_lt20 momagecat_ge30  pateduc married  fbnow;
run;

data _null_;
set cov_mean;
call symput('amht_mean_cl', amht_mean_cl);
call symput('momagecat_lt20', momagecat_lt20);
call symput('momagecat_ge30', momagecat_ge30);

call symput('pateduc', pateduc);
call symput('married', married);
call symput('fbnow', fbnow);
call symput('north', north);

run;

data test ;
   set uganda_all (where=(hvisnum_cl>3));
   if study=0 then do;
   		chld_weight = weight_diff + weight;
		chld_height = height_diff + height;
		chld_bmi = bmi_diff + bmi;
	end;
	if study=1 then do;
		chld_weight = weight;
		chld_height = height;
		chld_bmi = bmi;
	end;

	** Relative growth parameters;
		StudyBoy = (Study*Boy);
		StudyAge = (Study*Age_months);
		boyAge   = (Boy*Age_months);
		* StudyBoyAge   = (Study*Boy*Age_months);

    ** Centering the covariates;
        north = north-&north;
		married = married - &married;
		momage_cat_lt20 = momagecat_lt20 - &momagecat_lt20;
		momage_cat_ge30 = momagecat_ge30 - &momagecat_ge30;
        amht_mean_cl = amht_mean_cl - &amht_mean_cl;
		pateduc = pateduc - &pateduc;
		fbnow = fbnow - &fbnow;

   if amyrs_cl ne . then do;
	momagestudy_cat_lt20 = (momagecat_lt20*study);
	momagestudy_cat_lt30 = (momagecat_lt30*study);
    momagestudy_cat_ge30 = (momagecat_ge30*study);
   end;

	if amht_mean_cl ne  . then do;
		momhtstudy_cat_lt145 = (.   <  amht_mean_cl < 145)*study;
	   	momhtstudy_cat_lt150 = (145 <= amht_mean_cl < 150)*study;
   		momhtstudy_cat_lt155 = (150 <= amht_mean_cl < 155)*study;
   		momhtstudy_cat_lt160 = (155 <= amht_mean_cl < 160)*study ;
   		momhtstudy_cat_ge160 = (160 <= amht_mean_cl)*study;  
		momhtstudy           =         amht_mean_cl *study;
   end;


   if north ne . then do;
       northstudy_cat = north*study;
	   southstudy_cat = (north=0)*study;
   end;

   if pateduc ne . then do;
       pateducstudy_cat = pateduc*study;
	   patuneducstudy_cat = (pateduc=0)*study;
   end;

   if married ne . then do;
       marriedstudy_cat = married*study;
	   unmarriedstudy_cat = (married=0)*study;
   end;

   if fbnow ne . then do;
       fbnowstudy_cat = fbnow*study;
	   nofbnowstudy_cat = (fbnow=0)*study;
   end;

   if height*weight*bmi*study * north * boy * age_months * momagecat_lt30 * momagecat_ge30 * 
				momhtcat_lt150 *  momhtcat_lt155 * momhtcat_lt160 * momhtcat_ge160 *  pateduc * married  *
                  fbnow=. then incomplete=1;
   if incomplete=1 or hhid=. then delete;
   keep hhid study north asex_cl  boy age_months amht_mean_cl amyrs_cl momagecat: momhtcat: pateduc married  fbnow
        weight height bmi weight_diff height_diff bmi_diff final_wgt age sex hdist hscount hvisnum_cl incomplete
        StudyBoy StudyAge BoyAge   /* StudyBoyAge */  
		fbnowstudy_cat nofbnowstudy_cat marriedstudy_cat unmarriedstudy_cat pateducstudy_cat patuneducstudy_cat
 		northstudy_cat southstudy_cat momhtstudy_cat_lt145 momhtstudy_cat_lt150 momhtstudy_cat_lt155 
 		momhtstudy_cat_lt160 momhtstudy_cat_ge160 momhtstudy momagestudy_cat_lt20 momagestudy_cat_lt30 momagestudy_cat_ge30;
   run;
proc means data=test; run;
   
PROC SORT DATA = test;
   by hhid age sex descending study; 
run;


/*********************************************************************
  Perform bootstrap to get standard error for quantile regression
**********************************************************************/


* Fits the quantile regression model -- no stderr;
%MACRO REG(data=, y=);
ods listing close;
ods select none;

PROC SURVEYREG DATA=&data;
   CLASS hscount;
   MODEL &y =  &xvars   / clparm solution;
   CLUSTER hhid;
   WEIGHT final_wgt;
   ods output ParameterEstimates=PARAMEST;
run;

DATA PARAMEST; 
	SET PARAMEST;
	quantile=0.5;  * create this variable just for programming covenience, can be any value;
	run;
	
%MEND REG;
*ods listing;
*ods select default;



* Fits the quantile regression model -- no stderr;
%MACRO QUANTREG(data=, y=, Q=);
ods listing close;
ods select none;

PROC QUANTREG DATA=&data order=formatted ci=none algorithm=interior(kappa=0.999 tolerance=0.0001);
   CLASS hscount;
   MODEL &y = &xvars / quantile =  &Q ;
   WEIGHT final_wgt;
   ods output ParameterEstimates=PARAMEST;
run;
%MEND QUANTREG;



* Obtains parameter estimates for quantile regression parameters (betas);
%MACRO PARAMEST2; 
   PROC SORT DATA=paramest; BY Quantile; 

   DATA PARAMEST2;
      SET PARAMEST;
      by quantile;
      retain Intercept study boy age_months StudyBoy BoyAge StudyAge  /* StudyBoyAge */ 
                fbnowstudy_cat nofbnowstudy_cat marriedstudy_cat unmarriedstudy_cat pateducstudy_cat patuneducstudy_cat
 				northstudy_cat southstudy_cat momhtstudy momagestudy_cat_lt20 momagestudy_cat_lt30 momagestudy_cat_ge30;

      	if first.Quantile then do;
       	  intercept = .;
      	end;

      if (upcase(parameter) = 'INTERCEPT')  then Intercept = Estimate;
      if (upcase(parameter) = 'STUDY')      then Study = Estimate;
      if (upcase(parameter) = 'NORTH')      then North = Estimate;
      if (upcase(parameter) = 'BOY')        then Boy = Estimate;
      if (upcase(parameter) = 'AGE_MONTHS')  then Age_months = Estimate;
	  if (upcase(parameter) = 'AMHT_MEAN_CL') then amht_mean_cl = Estimate;
	  if (upcase(parameter) = 'MOMAGECAT_LT20')  then Momagecat_LT20 = eSTIMATE;
	  if (upcase(parameter) = 'MOMAGECAT_LT30')  then Momagecat_LT30 = ESTIMATE;
	  if (upcase(parameter) = 'MOMAGECAT_GE30')  then Momagecat_GE30 = ESTIMATE;
	  *if (upcase(parameter) = 'MOMHTCAT_LT145')   then Momhtcat_LT145 = ESTIMATE;
	  *if (upcase(parameter) = 'MOMHTCAT_LT150')   then Momhtcat_LT150 = ESTIMATE;
	  *if (upcase(parameter) = 'MOMHTCAT_LT155')   then Momhtcat_LT155 = ESTIMATE;
	  *if (upcase(parameter) = 'MOMHTCAT_LT160')   then Momhtcat_LT160 = ESTIMATE;
	  *if (upcase(parameter) = 'MOMHTCAT_GE160')   then Momhtcat_GE160 = ESTIMATE;
      if (upcase(parameter) = 'PATEDUC')      then pateduc= Estimate;
      if (upcase(parameter) = 'MARRIED')      then married = Estimate;
      if (upcase(parameter) = 'FBNOW')        then fbnow = Estimate;
      if (upcase(parameter) = 'STUDYBOY')     then StudyBoy   = Estimate;
      if (upcase(parameter) = 'STUDYAGE')     then StudyAge   = Estimate;
	  if (upcase(parameter) = 'BOYAGE')       then boyAge   = Estimate;
	  *if (upcase(parameter) = 'STUDYBOYAGE')  then StudyBoyAge   = Estimate;
      *if (upcase(parameter) = 'STUDY*NORTH')       then StudyNorth = Estimate;

	  ** create indicator for study, population categories - reference is population;
      if (lowcase(parameter) ='fbnowstudy_cat') then fbnowstudy_cat = Estimate;
      if (lowcase(parameter) ='nofbnowstudy_cat') then nofbnowstudy_cat = Estimate;
	  if (lowcase(parameter) ='marriedstudy_cat') then marriedstudy_cat = Estimate;
	  if (lowcase(parameter) ='unmarriedstudy_cat') then unmarriedstudy_cat = Estimate;
	  if (lowcase(parameter) ='pateducstudy_cat') then pateducstudy_cat = Estimate;
	  if (lowcase(parameter) ='patuneducstudy_cat') then patuneducstudy_cat = Estimate;
	  if (lowcase(parameter) ='northstudy_cat') then northstudy_cat = Estimate;
	  if (lowcase(parameter) ='southstudy_cat') then southstudy_cat = Estimate;
	  if (lowcase(parameter) ='momhtstudy_cat_lt145') then momhtstudy_cat_lt145 = Estimate;
	  if (lowcase(parameter) ='momhtstudy_cat_lt150') then momhtstudy_cat_lt150 = Estimate;
	  if (lowcase(parameter) ='momhtstudy_cat_lt155') then momhtstudy_cat_lt155 = Estimate;
	  if (lowcase(parameter) ='momhtstudy_cat_lt160') then momhtstudy_cat_lt160 = Estimate;
	  if (lowcase(parameter) ='momhtstudy_cat_ge160') then momhtstudy_cat_ge160 = Estimate;
      if (lowcase(parameter) ='momhtstudy'          ) then momhtstudy           = Estimate;
	  if (lowcase(parameter) ='momagestudy_cat_lt20') then momagestudy_cat_lt20 = Estimate;
	  if (lowcase(parameter) ='momagestudy_cat_lt30') then momagestudy_cat_lt30 = Estimate;
	  if (lowcase(parameter) ='momagestudy_cat_ge30') then momagestudy_cat_ge30= Estimate;

      if last.quantile;
      keep 	quantile Intercept study north boy age_months  momagecat_lt30 momagecat_ge30 amht_mean_cl
	       	pateduc married fbnow   StudyBoy StudyAge BoyAge /* StudyBoyAge */
  		   	fbnowstudy_cat nofbnowstudy_cat marriedstudy_cat unmarriedstudy_cat pateducstudy_cat patuneducstudy_cat
 			northstudy_cat southstudy_cat momhtstudy_cat_lt145 momhtstudy_cat_lt150 momhtstudy_cat_lt155 
 			momhtstudy_cat_lt160 momhtstudy_cat_ge160 momhtstudy
			momagestudy_cat_lt20 momagestudy_cat_lt30 momagestudy_cat_ge30;
   run;
%MEND PARAMEST2;

* To calculate the 95% CIs from the bootstrap samples;
%macro BootCI(in=);

   %let betavars=  intercept study boy age_months StudyBoy StudyAge BoyAge /* StudyBoyAge */
                northstudy_cat  fbnowstudy_cat marriedstudy_cat pateducstudy_cat 
 				momhtstudy momagestudy_cat_lt30 momagestudy_cat_ge30;

   PROC SORT DATA=&in; by quantile; run;
   PROC UNIVARIATE DATA=&in noprint;
      VAR &betavars;
      OUTPUT out=PctBETA 	pctlpts  = 2.5  97.5
    						pctlpre  = &betavars
			    			pctlname = pct25  pct975;
      by quantile;
   run;


   PROC SORT data=PCTBETA     (where=(quantile ne .)); by quantile; run;

%mend BootCI;

* Conducting the bootstrap approach;
%macro BOOTSE(Data=, Y=, Boot=, Q=, seed=);

   * This is the dataset that contains final beta estimates by quantile;
   DATA Bootbetas; 
      SET _NULL_;
   run;

   *&opt;
   %do b=1 %to &Boot; *number of bootstrap samples;
 
      * find the number of subjects in the study and make it a macro variable;

      proc sort data=test; by hhid age_months;
      data subj; set test; by hhid; if first.hhid and study=1; run;

	  proc means data=subj noprint;
				var study;
				output out=N  N=N;

	  DATA _NULL_;SET N;CALL SYMPUT('N', N);RUN; *N=4002;
	  
      * Bootstrap sampling with replacement by pairID with size equal to study cohort;
      %let seed1=%sysevalf(&seed   + &b*10000); 
	  /*%put &seed1; */
      PROC SURVEYSELECT DATA=TEST(where=(STUDY=1)) out=sample0 method=urs sampsize=&N
                  seed=&seed1 outhits noprint;;
                 samplingunit hhid;
        run;

	   data sample0;
			set sample0;
			hhid_old=hhid;
			pair_ID=_n_;
			drop StudyBoy StudyAge BoyAge /* StudyBoyAge */ weight_diff height_diff bmi_diff incomplete
				fbnowstudy_cat nofbnowstudy_cat marriedstudy_cat unmarriedstudy_cat pateducstudy_cat patuneducstudy_cat
 				northstudy_cat southstudy_cat momhtstudy_cat_lt145 momhtstudy_cat_lt150 momhtstudy_cat_lt155 
 				momhtstudy_cat_lt160 momhtstudy_cat_ge160 momhtstudy momagestudy_cat_lt20 momagestudy_cat_lt30 momagestudy_cat_ge30;
         
		proc sort; by age sex;
		run;

	* merge bootstrap study sample with WHO data;
	DATA ALL_BOOT;
   	MERGE sample0 (in=x1 )
      	who.weianthro (KEEP=SEX AGE L S M rename=(L=L_WEIGHT M=M_WEIGHT S=S_WEIGHT))
      	who.lenanthro (KEEP=SEX AGE L S M rename=(L=L_LENGTH M=M_LENGTH S=S_LENGTH))
      	who.bmianthro (KEEP=SEX AGE L S M rename=(L=L_BMI    M=M_BMI    S=S_BMI));
   		by age sex;
		if x1;
	run;

	* Creating a POPULATION data set (number of Blom points * study sample size) matched by gender and age;
	DATA ALL2_Boot (drop= i);
   		SET ALL_Boot (rename=(weight=chld_WEIGHT height=chld_height BMI=chld_BMI));
   		by age sex;
   
   		ARRAY s[*] s_weight s_length s_bmi;
   		ARRAY l[*] l_weight l_length l_bmi;
   		ARRAY m[*] m_weight m_length m_bmi;
   		ARRAY o[*] WEIGHT   height   BMI;               * Population values;
   		ARRAY a[*] chld_weight chld_height chld_bmi;    * Study values;
   		ARRAY d[*] weight_diff height_diff bmi_diff;    * Study values - population values;
   		* Recalculating anthropometic values, the differences between study and population;
		%let seed2=%sysevalf(&seed   + &b*10000 + 7654321);  
		/*%put &seed2;*/
		
      		z = rannorm(&seed2); 
	  		do i=1 to dim(a);
         		if L[i] ne 0 then o[i] = M[i]*((1+Z*S[i]*L[i])**(1/L[i]));
         		else if L[i] = 0 then o[i] = M[i]*exp(S[i]*Z);
         		d[i] = a[i] - o[i];
         		if d[i] = . then do; 
            	o[i] = .; 
            	a[i] = .; 
         		end;  
      		end;
      	OUTPUT;
   	
		run;

	* final bth bootstrap dataset;
	DATA UGANDA_ALL_Boot;
   		SET ALL2_boot(in=x 
            drop = WEIGHT height BMI weight_diff height_diff bmi_diff 
            rename=(chld_weight=WEIGHT chld_height=HEIGHT chld_bmi=BMI))
       	ALL2_Boot(drop=  chld_weight chld_height chld_bmi);
   		* Create study indicator (study) and assign weight (study_wgt)=1 if study, 
     		and 1/(# of Blom points) if population; 
   		IF x THEN DO;
      		study = 1; 
      		final_wgt = 1; 
   		END;
   		ELSE DO; 
      		study = 0; 
      		final_wgt = 1 ;  * Weighting each observation as inverse of the number of Blom points = 1/Nblom;
   		END;
   		* Creating a binary variable for BOY;
   		if sex = 2 then boy = 0;
   		if sex = 1 then boy = 1;
   		* Creating an AGE IN MONTHS variable for CHILD AGE;
   		age_months = (age * 12) / 365;
   		* Creating REGION variable: 1 for North and 0 for South Western;
		hhisOld=hhid;
   		hhid = pair_ID;
		if hhid=. then delete;
		drop L_W: S_L: M_: L_: ;
		run;

	
		proc means data=uganda_all_boot (where=(hvisnum_cl=4 and study=1)) noprint; 
			var north amht_mean_cl momagecat_lt20 momagecat_ge30  pateduc married  fbnow;
			output out=cov_mean_boot mean=north amht_mean_cl momagecat_lt20 momagecat_ge30  pateduc married  fbnow;
		run;

		data _null_;
			set cov_mean_boot;
			call symput('amht_mean_cl', amht_mean_cl);
			call symput('momagecat_lt20', momagecat_lt20);
			call symput('momagecat_ge30', momagecat_ge30);
			call symput('pateduc', pateduc);
			call symput('married', married);
			call symput('fbnow', fbnow);
			call symput('north', north);
	run;

	data uganda_all_boot ;
   		set uganda_all_boot ;
   		if study=0 then do;
   				chld_weight = weight_diff + weight;
				chld_height = height_diff + height;
				chld_bmi = bmi_diff + bmi;
		end;
		if study=1 then do;
			chld_weight = weight;
			chld_height = height;
			chld_bmi = bmi;
		end;
		StudyBoy = (Study*Boy);
		StudyAge = (Study*Age_months);
		BoyAge   = (Boy*Age_months);
	    *StudyBoyAge  = (Study*Boy*Age_months);

		
       ** Centering the covariates;
        north = north-&north;
		married = married - &married;
		momage_cat_lt20 = momagecat_lt20 - &momagecat_lt20;
		momage_cat_ge30 = momagecat_ge30 - &momagecat_ge30;
        amht_mean_cl = amht_mean_cl - &amht_mean_cl;
		pateduc = pateduc - &pateduc;
		fbnow = fbnow - &fbnow;

   		if amyrs_cl ne . then do;
			momagestudy_cat_lt20 = (15 <= amyrs_cl < 20) *study;
			momagestudy_cat_lt30 = (20 <= amyrs_cl < 30) *study;
    		momagestudy_cat_ge30 = (30 <= amyrs_cl)*study;
   		end;

		if amht_mean_cl ne  . then do;
			momhtstudy_cat_lt145 = (.   <  amht_mean_cl < 145)*study;
	   		momhtstudy_cat_lt150 = (145 <= amht_mean_cl < 150)*study;
   			momhtstudy_cat_lt155 = (150 <= amht_mean_cl < 155)*study;
   			momhtstudy_cat_lt160 = (155 <= amht_mean_cl < 160)*study ;
   			momhtstudy_cat_ge160 = (160 <= amht_mean_cl)*study;  
			momhtstudy           =         amht_mean_cl *study;
   		end;

   		if north ne . then do;
       		northstudy_cat = north*study;
	   		southstudy_cat = (north=0)*study;
   		end;

   		if pateduc ne . then do;
       		pateducstudy_cat = pateduc*study;
	   		patuneducstudy_cat = (pateduc=0)*study;
   		end;

   		if married ne . then do;
       		marriedstudy_cat = married*study;
	   		unmarriedstudy_cat = (married=0)*study;
   		end;

   		if fbnow ne . then do;
       		fbnowstudy_cat = fbnow*study;
	   		nofbnowstudy_cat = (fbnow=0)*study;
   		end;

   keep  hhid study north asex_cl  boy age_months amht_mean_cl amyrs_cl momagecat: momhtcat: pateduc married  fbnow
         weight height bmi weight_diff height_diff bmi_diff final_wgt age sex hdist hscount hvisnum_cl 
         StudyBoy StudyAge BoyAge /* StudyBoyAge */
		fbnowstudy_cat nofbnowstudy_cat marriedstudy_cat unmarriedstudy_cat pateducstudy_cat patuneducstudy_cat
 		northstudy_cat southstudy_cat momhtstudy_cat_lt145 momhtstudy_cat_lt150 momhtstudy_cat_lt155 
 		momhtstudy_cat_lt160 momhtstudy_cat_ge160 momhtstudy momagestudy_cat_lt20 momagestudy_cat_lt30 momagestudy_cat_ge30;
   run;


		%if &method=mean %then %do; 
   				%REG(DATA=UGANDA_ALL_Boot, y=&Y);
		%end;

		%if &method=quant %then %do; 
   				%QUANTREG(DATA=UGANDA_ALL_Boot, y=&Y, Q=&Q);
		%end;


   

      PROC SORT DATA=PARAMEST;               by quantile parameter; run;
      PROC TRANSPOSE DATA=PARAMEST OUT=BETA; by quantile parameter; run;
      DATA BETA_B;
         SET BETA (where=(_name_='Estimate') rename=( col1=beta));
         B = &b;
         drop _name_;  
      run;
      PROC TRANSPOSE DATA=BETA_B OUT=EST(drop=_NAME_) ; 
				ID parameter; by B quantile; 
				var beta; run;
 

      *** Append beta estimates to final dataset;
      DATA Bootbetas; 
         SET Bootbetas est; 
      run;
   %end;  * End of bootstrap DO loop;

   
   ods listing;
   ods select default;


   %BOOTCI(in=BootBetas);

%mend BOOTSE;

	
%macro BootModel(method=, Y=);

   %let xvars=  study boy age_months StudyBoy StudyAge BoyAge /* StudyBoyAge */
                northstudy_cat  fbnowstudy_cat marriedstudy_cat pateducstudy_cat 
 				momhtstudy momagestudy_cat_lt30 momagestudy_cat_ge30;


   %let seed1=12345;
   
   %if &method=mean %then %do; 
   		%REG(DATA=test, y=&Y);
	%end;
	%if &method=quant %then %do; 
		%QUANTREG (data=test, y=&Y, Q=0.025 0.05 0.1 0.2 0.5);
	%end;


   %PARAMEST2 ; * Paramter estimate output;

ods listing;
ods select default;

    DATA _ACTUAL_BETAS;     SET PARAMEST2; 
	
    run; 


&opt;
    %BootSE(data=test, y=&Y, Boot=1000, Q=0.025 0.05 0.1 0.2 0.5, seed=54321);
&optback;

proc means data=_ACTUAL_BETAS; class quantile; title "&y &method RESULTS FROM ORIGINAL DATA"; 
var intercept &xvars; run;

proc means data=bootbetas n mean stddev min p25 p50 p75 max maxdec=5;
%if &method=quant %then %do; class quantile; %end;
title "&y &method RESULTS FROM BOOTSTRAP SAMPLES";
run;

proc print data=pctbeta; 
title "&y &method 95% CI FROM BOOTSTRAP SAMPLES";
run;

data xxx.&method._&y._rand_2wayinter_boot1000;
set bootbetas;
run;

%mend;
*%BootModel(method=mean, y=weight);
*%BootModel(method=quant, y=weight);




   options ls=132 ps=80;

** Use regular regression estimating mean differences;
ods rtf file="&path\sas\results\meanmodel_rand_2wayinter_boot1000 &format_today..rtf";
%BootModel(method=mean, Y=weight);
%BootModel(method=mean, Y=height);
ods rtf close;



** Use quantile regression estimating quantile differences;
ods rtf file="&path\sas\results\quantmodel_rand_2wayinter_boot1000 &format_today..rtf";
%Bootmodel(method=quant, Y=Weight);
%Bootmodel(method=quant, Y=Height);
ods rtf close;





