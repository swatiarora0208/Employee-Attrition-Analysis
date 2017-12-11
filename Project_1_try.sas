/*
* Project 1: Survival Analysis
* Author: Swati Arora
*/

libname Project'/folders/myfolders/Project1';

proc import datafile= '/folders/myfolders/Project1/FermaLogis1.csv' out= Project.test DBMS=csv;
run;
proc print data= Project.Ferma;
run;


/* 
 * Setting the categorical variables to numeric.
 * And Creating new dataset.
 */
data Project.Ferma1;
set Project.Ferma;
if Attrition = 'Yes'
then AttritionN= 1;
else AttritionN= 0;
If Gender= 'Female' Then Gender_num= 1;
Else Gender_num =0;
If Maritalstatus = 'Single' Then mar_Status = 0;
Else if Maritalstatus = 'Married' Then mar_Status = 1;
Else if Maritalstatus = 'Divorced' Then mar_Status = 2;
IF YearsAtCompany > 40 and AttritionN = 0 THEN YearsAtCompany = 41;
IF Age < 30 Then Experience ='Fresher';
Else IF Age < 45 and Age > 30 Then Experience ='Moderate';
Else Experience = 'Highly experienced';
run;
proc print data= Project.Ferma1;
run;

Data Project.Ferma1;
Merge Project.Ferma1 project.test;
By employeenumber;
RUN;

/*
 * Goodness of fit test on all the variables excluding Bonus
 */
PROC LIFEREG DATA= Project.Ferma1;
class BusinessTravel Department  EducationField  JobRole OverTime ; 

 Model YearsAtCompany * AttritionN(0)= 	Age  DailyRate DistanceFromHome
   EnvironmentSatisfaction HourlyRate JobInvolvement JobLevel
		JobSatisfaction	MonthlyIncome MonthlyRate NumCompaniesWorked	
		PercentSalaryHike PerformanceRating RelationshipSatisfaction
	 StockOptionLevel TotalWorkingYears TrainingTimesLastYear	WorkLifeBalance
		YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager Education Gender_num
		Mar_Status
	/ DISTRIBUTION=LNORMAL; /*Run the code for all distributions*/
		 	PROBPLOT;
RUN;

PROC LIFEREG DATA= Project.Ferma1;
class BusinessTravel Department  EducationField  JobRole OverTime Bonus_1 - Bonus_40; 

 Model YearsAtCompany * AttritionN(0)= 	Age  DailyRate DistanceFromHome
   EnvironmentSatisfaction JobInvolvement	JobLevel
		JobSatisfaction	MonthlyIncome MonthlyRate NumCompaniesWorked	
		PercentSalaryHike PerformanceRating RelationshipSatisfaction
	StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear	WorkLifeBalance
		YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager Education Gender_num
		Mar_Status Bonus_1 - Bonus_40 BusinessTravel Department  EducationField  JobRole OverTime
		/ DISTRIBUTION=LNORMAL; /*Run the code for all distributions*/
		PROBPLOT;
RUN;

PROC LIFEREG DATA= Project.Ferma1;
class BusinessTravel Department  EducationField  JobRole OverTime ; 

 Model YearsAtCompany * AttritionN(0)= 	Age  DailyRate DistanceFromHome
 EmployeeCount EmployeeNumber EnvironmentSatisfaction HourlyRate JobInvolvement	JobLevel
		JobSatisfaction	MonthlyIncome MonthlyRate NumCompaniesWorked	
		PercentSalaryHike PerformanceRating RelationshipSatisfaction
	StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear	WorkLifeBalance
		YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager Education Gender_num
		Mar_Status
	/ DISTRIBUTION=LLOGISTIC; /*Run the code for all distributions*/
		 	PROBPLOT;
RUN;

PROC LIFEREG DATA= Project.Ferma1;
class BusinessTravel Department  EducationField  JobRole OverTime ; 

 Model YearsAtCompany * AttritionN(0)= 	Age  DailyRate DistanceFromHome
 EmployeeCount EmployeeNumber EnvironmentSatisfaction HourlyRate JobInvolvement	JobLevel
		JobSatisfaction	MonthlyIncome MonthlyRate NumCompaniesWorked	
		PercentSalaryHike PerformanceRating RelationshipSatisfaction
	StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear	WorkLifeBalance
		YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager Education Gender_num
		Mar_Status
	/ DISTRIBUTION=WEIBULL; /*Run the code for all distributions*/
		 	PROBPLOT;
RUN;

PROC LIFEREG DATA= Project.Ferma1;
class BusinessTravel Department  EducationField  JobRole OverTime ; 

 Model YearsAtCompany * AttritionN(0)= 	Age  DailyRate DistanceFromHome
 EmployeeCount EmployeeNumber EnvironmentSatisfaction HourlyRate JobInvolvement	JobLevel
		JobSatisfaction	MonthlyIncome MonthlyRate NumCompaniesWorked	
		PercentSalaryHike PerformanceRating RelationshipSatisfaction
	StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear	WorkLifeBalance
		YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager Education Gender_num
		Mar_Status
	/ DISTRIBUTION=GAMMA; /*Run the code for all distributions*/
		 	PROBPLOT;
RUN;

/*
 * Compare Models
 */
DATA CompareModels;
	L_exponential = -578.0670523;
	L_weibull = -484.3183826;
	L_lognormal = -480.4612996;
	L_gamma = -414.3730069;
	
	LRTEG = -2*(L_exponential - L_gamma);
	LRTEW = -2*(L_exponential - L_weibull);
	LRTWG = -2*(L_weibull - L_gamma);
	LRTLG = -2*(L_lognormal - L_gamma);

	p_valueEG = 1 - probchi(LRTEG,2);
	p_valueEW = 1 - probchi(LRTEW,1);
	p_valueWG = 1 - probchi(LRTWG,1);
	p_valueLG = 1 - probchi(LRTLG,1);
RUN;
PROC PRINT DATA=CompareModels;
RUN;

/*
 * After Comapring the models it is clear that LNormal is the best fit for our dataset. 
 */
/*
 * Imputing the covariates (using TEST in LIFETEST) to have an idea of which variable is significant and which are 
 * non-significant.
 */
PROC LIFETEST DATA=project.ferma1 METHOD=LIFE INTERVALS=5 10 15 20 25 30 35 40 41 PLOTS=(S,H);;
	TIME YearsAtCompany * AttritionN(0);
	TEST DailyRate DistanceFromHome
 EmployeeCount EmployeeNumber EnvironmentSatisfaction HourlyRate JobInvolvement	JobLevel
		JobSatisfaction	MonthlyIncome MonthlyRate NumCompaniesWorked	
		PercentSalaryHike PerformanceRating RelationshipSatisfaction
	StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear	WorkLifeBalance
		YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager Education Gender_num
		Mar_Status ;
RUN;

PROC CO
/*
 * Imputing Further using LIFEREG
 */

proc lifereg data=  Project.Ferma1;
class BusinessTravel Department Education EducationField JobRole OverTime ;

 Model YearsAtCompany * AttritionN(0)= 	Age  DailyRate DistanceFromHome
 EmployeeCount EmployeeNumber EnvironmentSatisfaction HourlyRate JobInvolvement	JobLevel
		JobSatisfaction	MonthlyIncome MonthlyRate NumCompaniesWorked	
		PercentSalaryHike PerformanceRating RelationshipSatisfaction
	StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear	WorkLifeBalance
		YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager mar_status gender_num
	 /D=LNORMAL;
 run;
/* 
 * Removing insignificant ones: DailyRate, Employeecount, EmployeeNumber, HourlyRate, ,
 * , MonthlyRate, PresentSalaryHike, PerformanceRating, RelationshipSatisfaction,
 *  Standard Hours, StockOptionLevel, WorkLifeBalance, Gender , JobRole, Education,
 * EducationField, JobLevel, StockOptionLevel
 */
 
proc lifereg data=  Project.Ferma1;
class BusinessTravel Department OverTime ;

 Model YearsAtCompany * AttritionN(0)= 	Age   DistanceFromHome EnvironmentSatisfaction 
    JobInvolvement JobSatisfaction NumCompaniesWorked TotalWorkingYears MonthlyIncome
    TrainingTimesLastYear YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager 
    mar_status  WorkLifeBalance BusinessTravel Department bonus_total
     OverTime 
	 /D=LNORMAL CLF;
	 PROBPLOT;
 run;
 
proc lifereg data=  Project.Ferma1;
class BusinessTravel Department  OverTime ;

 Model YearsAtCompany * AttritionN(0)= 	Age   DistanceFromHome EnvironmentSatisfaction 
    JobInvolvement JobSatisfaction NumCompaniesWorked TotalWorkingYears MonthlyIncome
    TrainingTimesLastYear YearsInCurrentRole	YearsSinceLastPromotion	YearsWithCurrManager 
    mar_status StockOptionLevel WorkLifeBalance BusinessTravel Department bonus_total
     OverTime 
	 /D=Weibull ;
	 PROBPLOT;
 run;

/*
 * Visualization
 */
 
 PROC SGPLOT data= project.ferma1;
 VBOX Age/ Category= AttritionN;
  label Age = 'Age' AttritionN = 'Attrition';
 title ' Distribution of Attrition by Age';
 run;
 
 Proc SGPLOT data= Project.Ferma1;
 VBAR AttritionN / GROUP= Overtime;
  label Overtime = 'Overtime' AttritionN = 'Attrition';
 title ' Distribution of Attrition by Overtime';
 run;

/*
 * Attrition Comparision using Strata
 */


PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA Overtime businessTravel / ADJUST=TUKEY;
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA Overtime MaritalStatus / ADJUST=TUKEY;
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA Overtime JobSatisfaction / ADJUST=TUKEY;
RUN;
PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA JobSatisfaction ;
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA WOrkLifeBalance ;
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA Joblevel ;
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA Department ;
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA Department ;
RUN;
PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA Experience;
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA Overtime ;	
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA EnvironmentSatisfaction ;	
RUN;

PROC LIFETEST DATA=PROJECT.FERMA1 method=life plots=(S,H);
	TIME YearsAtCompany*AttritionN(0);
	STRATA bonus_total ;	
RUN;



 PROC SGPLOT data= project.ferma1;
 VBOX MonthlyIncome/ Category= AttritionN group=Overtime;
  label Age = 'Age' AttritionN = 'Attrition' ;
 title ' Distribution of Attrition by Income and Overtime';
 run;
 
  PROC SGPLOT data= project.ferma1;
 VBOX MonthlyIncome/ Category= AttritionN group=Department;
  label Age = 'Age' AttritionN = 'Attrition' ;
 title ' Distribution of Attrition by Income and Overtime';
 run;
 
 
              ODS GRAPHICS ON;
              PROC FREQ DATA = project.ferma1 (where=(yearsatcompany <5));
              TABLES   Attrition *Trainingtimeslastyear/ PLOTS=FREQPLOT(TWOWAY=GROUPVERTICAL);
              *TABLES BusType * OnTimeOrLate / PLOTS=FREQPLOT(TWOWAY=GROUPHORIZONTAL);
              *TITLE;
              RUN;
               ODS GRAPHICS ON;
              PROC FREQ DATA = project.ferma1 (where=(yearsatcompany >=5 and department ='Research & Development'));
              TABLES   Attrition *Trainingtimeslastyear/ PLOTS=FREQPLOT(TWOWAY=GROUPVERTICAL);
              *TABLES BusType * OnTimeOrLate / PLOTS=FREQPLOT(TWOWAY=GROUPHORIZONTAL);
              *TITLE;
              RUN;
              PROC FREQ DATA = project.ferma1 (where=(yearsatcompany >=5 and department ='Sales'));
              TABLES   Attrition *Trainingtimeslastyear/ PLOTS=FREQPLOT(TWOWAY=GROUPVERTICAL);
              *TABLES BusType * OnTimeOrLate / PLOTS=FREQPLOT(TWOWAY=GROUPHORIZONTAL);
              *TITLE;
              RUN;
              PROC FREQ DATA = project.ferma1 (where=(yearsatcompany >=5 and department ='Human Resources'));
              TABLES   Attrition *Trainingtimeslastyear/ PLOTS=FREQPLOT(TWOWAY=GROUPVERTICAL);
              *TABLES BusType * OnTimeOrLate / PLOTS=FREQPLOT(TWOWAY=GROUPHORIZONTAL);
              *TITLE;
              RUN;
              
 
 
  PROC SGPLOT data= project.ferma1;
 VBOX Distancefromhome/ Category= AttritionN Group=Overtime;
  label Age = 'Age' AttritionN = 'Attrition' ;
 title ' Distribution of Attrition by Income and Overtime';
 run;
 
   PROC SGPLOT data= project.ferma1;
 VBOX TotalworkingYears/ Category= AttritionN ;
  label Age = 'Age' AttritionN = 'Attrition' ;
 title ' Distribution of Attrition by Income and Overtime';
 run;
 
 
   PROC SGPLOT data= project.ferma1;
 VBOX Bonus_total/ Category= AttritionN ;
  label Age = 'Age' AttritionN = 'Attrition' ;
 title ' Distribution of Attrition by Income and Overtime';
 run;
/*
 * Transpose Bonus variables
 */
proc transpose data=project.ferma1 out= project.Bonus (rename=( _name_ =Type col1=bonus));
Var bonus_1 - bonus_40;
by employeenumber;
RUN;
Data project.formatBonus;
Set project.Bonus;
Format bonus_updated;
IF Bonus ='NA' Then bonus_updated =0;
Else if bonus ='0' Then bonus_updated = 0;
Else bonus_updated =1;
RUN;
Proc freq data=project.formatBonus(where= (bonus_updated=1));
by employeenumber ;
RUN;
