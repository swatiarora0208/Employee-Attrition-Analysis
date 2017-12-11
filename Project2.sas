*Define Permanent Library;
LIBNAME Lib "P:\Semester 3\Survival Analytics\Project 2";
DATA Lib.employees;
INFILE "P:\Semester 3\Survival Analytics\Project 2\fermalogis_event_type.csv" truncover firstobs=2 dlm="," dsd;
input X Obsv Age Turnover$:3. Type :2. BusinessTravel $:17. DailyRate :4. Department $:22. DistanceFromHome :2. Education 
EducationField $ :16.
EmployeeCount 	EmployeeNumber :4. EnvironmentSatisfaction Gender $:6. HourlyRate :3. JobInvolvement 
JobLevel JobRole $:25. JobSatisfaction MaritalStatus $:9. MonthlyIncome :5. MonthlyRate :5. 
NumCompaniesWorked  Over18 $
OverTime $:3. PercentSalaryHike PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel	
TotalWorkingYears :2. TrainingTimesLastYear WorkLifeBalance YearsAtCompany :2. YearsInCurrentRole 
YearsSinceLastPromotion :2. YearsWithCurrManager :2.	
(bonus_1 bonus_2 bonus_3 bonus_4 bonus_5 bonus_6 bonus_7 bonus_8 bonus_9 bonus_10 bonus_11 bonus_12 
bonus_13 bonus_14 bonus_15 bonus_16 bonus_17 bonus_18 bonus_19 bonus_20	bonus_21 bonus_22 bonus_23 
bonus_24 bonus_25 bonus_26 bonus_27 bonus_28 bonus_29 bonus_30 bonus_31	bonus_32 bonus_33 bonus_34
bonus_35 bonus_36 bonus_37 bonus_38 bonus_39 bonus_40) ($) ;
*change Turnover from character type to numeric type;
if Turnover="Yes" then Turnover_b = 1;
if Turnover= "No" then Turnover_b = 0;
*Map the turnover type;
if type = 0 then type_label = "No turnover";
if type = 1 then type_label = "Retirement";
if type = 2 then type_label = "Voluntary";
if type = 3 then type_label = "Involuntary";
if type = 4 then type_label = "Fired";
array bonus bonus_1 - bonus_40;
do over bonus ;
if bonus = "NA" then bonus = .;
end;
run;
PROC  CONTENTS DATA = LIB.EMPLOYEES;
RUN;

*Exploratory Data Analysis;

*type of turnovers;
PROC SGPLOT DATA = LIB.employees;
VBAR type_label;
where type_label<>"No turnover";
TITLE "Type of Resignation vs Attrition";
RUN;

*Splitting dataset into volunteer and others;

DATA lib.voluntary1;  /*create voluntary1 data*/
  SET lib.employees;
 where  type_label = "Voluntary";
 run;

PROC SGPLOT DATA = LIB.voluntary1;
VBAR JobSatisfaction ;
where type_label= "Voluntary";
RUN;

PROC SGPLOT DATA = LIB.voluntary1;
VBAR Department;
where type_label= "Voluntary";
TITLE "Departments of people resingning";
RUN;

PROC SGPLOT DATA = LIB.voluntary1;
VBAR Gender;
where type_label= "Voluntary";
TITLE "Genders of people resingning";
RUN;


PROC SGPLOT DATA = LIB.voluntary1;
VBAR MaritalStatus ;
where type_label= "Voluntary";
TITLE "Marital Status of people resingning";
RUN;


PROC SGPLOT DATA = LIB.voluntary1;
VBAR StockOptionLevel;
where type_label= "Voluntary";
TITLE "Bonus of people resingning";

RUN;


PROC SGPLOT DATA = LIB.voluntary1;
VBAR JobRole ;
where type_label= "Voluntary";
TITLE "Job Roles of people resingning";
RUN;

PROC SGPLOT DATA = LIB.voluntary1;
VBAR TrainingTimesLastYear ;
where type_label= "Voluntary";

RUN;

PROC SGPLOT DATA = LIB.voluntary1;
VBAR YearsSinceLastPromotion ;
where type_label= "Voluntary";
TITLE "Time Since Last Promotion of people resingning";
RUN;

*Find whether the hazard rates are same for the event types;
PROC FREQ data=Lib.employees;
	WHERE type ne 0;
	TABLES type /chisq;
RUN;
DATA Lib.Retirement;  /*create Retirement data*/
  SET lib.employees;
  event=(type =1);
  turnover_type = "Retirement";
DATA lib.Voluntary;  /*create Voluntary data*/
  SET lib.employees;
  event=(type =2);
  turnover_type = "Voluntary";
RUN;
DATA lib.Involuntary;  /*create Involuntary data*/
  SET lib.employees;
  event=(type =3);
  turnover_type = "Involuntary";
DATA lib.Fired;  /*create Fired data*/
  SET lib.employees;
  event=(type =4);
  turnover_type = "Fired";
DATA lib.all_turnover; /*we combined the datasets to use them as strata in the graphical analysis*/
  SET lib.Retirement lib.Voluntary lib.Involuntary lib.Fired;
PROC LIFETEST DATA=lib.all_turnover PLOTS=LLS;  /*LLS plot is requested*/
  TIME YearsAtCompany*event(0);
  STRATA turnover_type/diff=all;
RUN;
*Test whether coefficients found for each event type is equal to
coefficients found for the model created for combined event types;
*Nested Model;
PROC PHREG DATA=lib.employees;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
   MODEL YearsAtCompany*type(0)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager /TIES=EFRON;

*FIRED;
PROC PHREG DATA=lib.employees;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
   MODEL YearsAtCompany*type(0,1,2,3)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsAtCompany YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager / TIES=EFRON;

*INVOLUNTARY;
PROC PHREG DATA=lib.employees;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
MODEL YearsAtCompany*type(0,1,2,4)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager / TIES=EFRON;

*VOLUNTARY;
PROC PHREG DATA=lib.employees;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
MODEL YearsAtCompany*type(0,1,3,4)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager / TIES=EFRON;

*RETIREMENT;
PROC PHREG DATA=lib.employees;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
   MODEL YearsAtCompany*type(0,2,3,4)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager / TIES=EFRON;

DATA LogRatioTest_All_types;
	Nested = 2140.114;
	Fiered= 271.792;
	Involuntary = 485.688;
	Voluntary = 843.820;
	Retirement = 85.787;
	
	Total = Fiered+ Involuntary + Voluntary + Retirement;
	Diff = Nested - Total;

	P_value = 1 - probchi(Diff,99); *33x4 coef. in 4 models - 33 coef. in nested;
RUN;

PROC PRINT DATA = LogRatioTest_All_types;
	FORMAT P_Value 5.3;
RUN;

*Time dependent co-variate - Bonus;
/*this estimates the effect of the previous year bonus on turnover*/
PROC PHREG DATA=lib.Voluntary;
   WHERE YearsAtCompany>1;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
   MODEL YearsAtCompany*event(0)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager bonus_emp_lag1 /TIES=EFRON;
ARRAY bonus_lag(*) bonus_1-bonus_40;
	bonus_lag1=bonus_lag[YearsAtCompany-1];
	bonus_emp_lag1 = input(bonus_lag1,3.0);
RUN;
/*this estimates the effect of the previous 2 years bonus on turnover*/
PROC PHREG DATA=lib.Voluntary;
   WHERE YearsAtCompany>2;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
   MODEL YearsAtCompany*event(0)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager bonus_emp_lag2 /TIES=EFRON;
ARRAY bonus_lag(*) bonus_1-bonus_40;
	bonus_lag2=bonus_lag[YearsAtCompany-2];
	bonus_emp_lag2 = input(bonus_lag2,3.0);
RUN;
/*this is the effect of employment cumulatively*/
DATA lib.voluntarycum;
   SET lib.voluntary;
   ARRAY bonus(*) bonus_1 - bonus_40;
   ARRAY cum(*) cum1-cum40;
   cum1=bonus_1;
   DO i=2 TO 40;
      cum(i)=(cum(i-1)*(i-1) + bonus(i))/i;
   END;
PROC PHREG DATA=lib.Voluntarycum;
WHERE YearsAtCompany>1;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
MODEL YearsAtCompany*event(0)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager bonus_cum/ TIES=EFRON;
   ARRAY cumbonus(*) cum1-cum40;
   bonus_cum=cumbonus[YearsAtCompany-1];
   
*Define Permanent Library;
LIBNAME Lib "P:\OPIM5894\Library";
/**ASSESS Statement**/
ODS GRAPHICS ON;
PROC PHREG DATA=lib.voluntary;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
MODEL YearsAtCompany*event(0)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EnvironmentSatisfaction Gender HourlyRate JobInvolvement EducationField
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager /TIES=EFRON;
ASSESS PH / RESAMPLE;
RUN;
/**shoenfeld residuals**/
PROC PHREG DATA=lib.voluntary;
MODEL YearsAtCompany*event(0)= Age DailyRate DistanceFromHome HourlyRate MonthlyIncome MonthlyRate NumCompaniesWorked
PercentSalaryHike TotalWorkingYears TrainingTimesLastYear 
YearsInCurrentRole YearsSinceLastPromotion YearsWithCurrManager
Education EnvironmentSatisfaction JobInvolvement JobLevel JobSatisfaction /TIES=EFRON;
OUTPUT OUT=sch_out RESSCH=schAge schDailyRate schDistanceFromHome schHourlyRate schMonthlyIncome schMonthlyRate schNumCompaniesWorked
schPercentSalaryHike schTotalWorkingYears schTrainingTimesLastYear 
schYearsInCurrentRole schYearsSinceLastPromotion schYearsWithCurrManager
schEducation schEnvironmentSatisfaction  schJobInvolvement schJobLevel
 schJobSatisfaction 
RUN;
DATA sch_out;
	SET sch_out;
	id= _n_;
RUN;
proc sgplot data=sch_out;
	scatter x=YearsAtCompany y=schage / datalabel=id;
run;
/*find the correlations with yearsatcompany and functions of yearsatcompany*/
DATA corryearsatcompany;
  SET sch_out;
  where yearsatcompany > 1;
  lyearsatcompany =log(yearsatcompany);
  yearsatcompany2=yearsatcompany **2;
PROC CORR data = corryearsatcompany;
VAR yearsatcompany lyearsatcompany yearsatcompany2;
WITH schAge schDailyRate schDistanceFromHome schHourlyRate schMonthlyIncome schMonthlyRate 
schNumCompaniesWorked schPercentSalaryHike schTotalWorkingYears schTrainingTimesLastYear 
schYearsInCurrentRole schYearsSinceLastPromotion schYearsWithCurrManager
schEducation schEnvironmentSatisfaction  schJobInvolvement schJobLevel
schJobSatisfaction ;
RUN;
*Find significant NP covariates as simple function of time;
PROC PHREG DATA=lib.Voluntary;
   WHERE YearsAtCompany>1;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
   MODEL YearsAtCompany*event(0)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager ageyr 
CurrMgryr Curroleyr dlyrtyr ttlwrkgyr /TIES=EFRON;
ageyr = age*yearsatcompany;
CurrMgryr = YearsWithCurrManager *yearsatcompany;
Curroleyr = YearsInCurrentRole *yearsatcompany;
dlyrtyr = DailyRate *yearsatcompany;
ttlwrkgyr = TotalWorkingYears *yearsatcompany;
run;
*Find significant NP covariates as log function of time;
DATA lib.voluntary;
     SET lib.voluntary;
     where yearsatcompany > 1;
     lyearsatcompany =log(yearsatcompany);
RUN;
PROC PHREG DATA=lib.Voluntary;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
MODEL lYearsAtCompany*event(0)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager ageyr 
CurrMgryr Curroleyr dlyrtyr ttlwrkgyr /TIES=EFRON;
ageyr = age*lyearsatcompany;
CurrMgryr = YearsWithCurrManager *lyearsatcompany;
Curroleyr = YearsInCurrentRole *lyearsatcompany;
dlyrtyr = DailyRate *lyearsatcompany;
ttlwrkgyr = TotalWorkingYears *lyearsatcompany;
run;
*Final Model built(Step-wise) with NP Covariates and bonus_lag1;
PROC PHREG DATA=lib.Voluntary;
WHERE YearsAtCompany>1;
CLASS BusinessTravel Department EducationField Gender JobRole MaritalStatus OverTime JobLevel Education JobSatisfaction WorkLifeBalance RelationshipSatisfaction StockOptionLevel;
MODEL YearsAtCompany*event(0)= Age BusinessTravel DailyRate Department DistanceFromHome Education 
EducationField  EnvironmentSatisfaction Gender HourlyRate JobInvolvement 
JobLevel JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate NumCompaniesWorked
OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction  StockOptionLevel	
TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole 
YearsSinceLastPromotion YearsWithCurrManager ageyr 
CurrMgryr Curroleyr dlyrtyr ttlwrkgyr bonus_emp_lag1 /TIES=EFRON selection=stepwise;
ageyr = age*yearsatcompany;
CurrMgryr = YearsWithCurrManager *yearsatcompany;
Curroleyr = YearsInCurrentRole *yearsatcompany;
dlyrtyr = DailyRate *yearsatcompany;
ttlwrkgyr = TotalWorkingYears *yearsatcompany;
ARRAY bonus_lag(*) bonus_1-bonus_40;
	bonus_lag1=bonus_lag[YearsAtCompany-1];
	bonus_emp_lag1 = input(bonus_lag1,3.0);
run;