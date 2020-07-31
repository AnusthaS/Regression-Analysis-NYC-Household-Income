PROC IMPORT OUT= WORK.HHIncome 
            DATAFILE= "\\tsclient\Anustha Shrestha\Documents\Baruch College\Fall 2018\9700 Regression Analysis\Project round 2\SASinputsmall.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="SASinput$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

PROC IMPORT OUT= WORK.matrixhw 
            DATAFILE= "\\tsclient\Anustha Shrestha\Documents\Baruch College\Fall 2018\9700 Regression Analysis\Project round 2\matrixhw.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/*Simple Regression Model*/
PROC PRINT DATA=HHIncome;
RUN;

PROC SGPLOT DATA=HHIncome;
SCATTER x=UnempShare y=Income;
XAXIS LABEL = '% of Civilian Labor Force who are Unemployed';
YAXIS LABEL = 'Median Household Income';
TITLE 'Median Household Income of New York City Neighborhoods against Percentage of Civilian Force who are Unemployed';
RUN; 

/*Playing around with transformation*/
DATA HHIncome1;
SET HHIncome;
logUnemp = LOG (UnempShare);
logIncome = LOG (Income);
sqrtUnemp = SQRT (UnempShare);
sqrtIncome = SQRT (Income);
sqUnemp = UnempShare **2;
sqIncome = Income **2;
oneoverUnemp = 1/UnempShare;
RUN;

PROC SGPLOT DATA=HHIncome1;
SCATTER x=oneoverUnemp y=Income;
RUN;

PROC SGPLOT DATA=HHIncome1;
SCATTER x=logUnemp y=logIncome;
RUN;

PROC SGPLOT DATA=HHIncome1;
SCATTER x=logUnemp y=Income;
RUN;

PROC SGPLOT DATA=HHIncome1;
SCATTER x=UnempShare y=logIncome;
RUN;

PROC SGPLOT DATA=HHIncome1;
SCATTER x=sqrtUnemp y=sqrtIncome;
RUN;

PROC SGPLOT DATA=HHIncome1;
SCATTER x=UnempShare y=sqIncome;
RUN;


PROC SGPLOT DATA=HHIncome1;
SCATTER x=sqUnemp y=sqIncome;
RUN;


PROC REG DATA=HHIncome1; /*best*/
Model logIncome = logUnemp;
RUN;

PROC REG DATA=HHIncome1;
Model logIncome = UnempShare;
RUN;


PROC REG DATA=HHIncome1; /*not good*/
Model Income = logUnemp;
RUN;

PROC REG DATA=HHIncome1; /*bad*/
Model Income = oneoverunemp;
RUN;



/*Anustha Shrestha - Matrix approach to regression*/
PROC REG data=matrixhw;
Model LogIncome = LogUnemp;
RUN;


PROC IML; /*Opens the Matrix Language procedure*/
use matrixhw; /*Identifies the SAS file to import*/
read all; /*imports all components of SAS dataset with their names*/

one_vec = j(8,1); /*j(n,1) creates a vector of lenght n filled with 1s*/

X = one_vec || LogUnemp || AvgHHSize; /*Sandwiches columns into a matrix*/
print X;

Y= LogIncome;

mat1 = t(X) * X; /* shows steps*/
mat2 = inv(t (X) * X);
mat3 = inv (t(X) * X) * t(X);
print mat1 mat2 mat3;

b = inv(t(X) *X) * (t(X)*Y); /*compustes b in one go*/
print b;

/*exporting IML results to SAS file*/
create NewIncome var {b_vec}; /*Creating SAS file named NewIncome and SAS variable b-vec*/
append from b; /*Sends IML variable "b" to SAS variable b_vec*/

quit;

PROC PRINT data=NewIncome;
RUN;
 
/*For comparison of slope and coefficient*/
PROC REG data=matrixhw;
Model LogIncome = LogUnemp AvgHHSize;
RUN;

/*Chapter 4 Model Selection*/
PROC PRINT data=HHIncome (obs =4);
RUN;

/*Avoided*/PROC SGSCATTER data=HHIncome;
matrix Income AvgHHSzE UnempShare WomeninLF	LessthanHS	BachelorandH Foreignborn FinanceServices;
RUN;

PROC SGSCATTER data=HHIncome;
matrix Income AgriMfgConst Trade TransandUtilities IT FinanceServices scientificandmgmt EducationHealth ArtsEntertainment PublicAdm Other;
RUN;

DATA HHIncTrans2;
SET HHIncome;
logInc = log(Income);
logUnemp = log (UnempShare);
logHHSz = log (AvgHHSzE);
logWomen = log (WomeninLF);
logOccuMSA = log (MgmtSciArt);
sqOccuMSA = OccuMgmtSciArt**2;
logOccuSer = log(OccuService);
sqOccuSer = OccuService **2;
sqrtOccuSer = sqrt(OccuService);
logOccuSal = log(OccuSales);
logOccCons = log(OccuConstrctin);
logOccuProd = log(OccuProduction);
logHS = log (LessthanHS);
logBS = log (BachelorandH);
logForeign = log (ForeignBorn);
logAMC = log(AgriMfgConst);
logTrade = log(Trade);
logTU = log(TransandUtilities);
logIT = log (IT);
logFS = log (FinanceServices);
logSM = log (scientificandmgmt);
logEH = log (EducationHealth);
logArt = log (ArtsEntertainment);
logPAdm = log (PublicAdm);
LogOthr = log (Other);
logInc = log(Income);
logUnemp = log (UnempShare);
logWomen = log (WomeninLF);
logOccuMSA = log(OccuMgmtSciArt);
logOccuSer = log(OccuService);
logOccuSal = log(OccuSales);
logOccCons = log(OccuConstrctin);
logOccuProd = log(OccuProduction);
logHS = log (LessthanHS);
logBS = log (BachelorandH);
sqBS = BachelorandH **2;
logForeign = log (ForeignBorn);
logAMC = log(AgriMfgConst);
logTrade = log(Trade);
logTU = log(TransandUtilities);
logIT = log (IT);
logFS = log (FinanceServices);
logSM = log (scientificandmgmt);
logEH = log (EducationHealth);
logArt = log (ArtsEntertainment);
logPAdm = log (PublicAdm);
LogOthr = log (Other);
RUN;

PROC PRINT DATA=HHIncTrans2 (obs =4);
RUN;

/*log transformed*/
PROC SGSCATTER data=HHIncTrans2;
matrix LogInc AvgHHSzE logUnemp logWomen logHS logBS logForeign LogFS; 
RUN;

/*Avoided*/PROC SGSCATTER data=HHIncTrans2; /*Selected*/
matrix logInc logAMC logTrade logTU logIT logFS logSM logEH logArt logPAdm LogOthr;
RUN;

/* trying other transformation1*/

PROC SGSCATTER data=HHIncTrans2; /*Selected*/
matrix LogInc AvgHHSzE logUnemp logWomen LessthanHS	logBS logForeign logFS; 
RUN;


/*Avoided*/PROC SGSCATTER data=HHIncTrans2;
matrix LogInc logAMC logTrade TransandUtilities logIT logFS logSM logEH logArt logPAdm LogOthr;
RUN;

/*Avoided*/PROC SGSCATTER data=HHIncTrans1;
matrix logInc logUnemp logWomen logOccuMSA logOccuSer logOccuSal logOccCons logOccuProd logHS logBS logForeign; 
RUN;

PROC SGSCATTER data=HHIncTrans1;
matrix logInc logAMC logTrade logTU logIT logFS logSM logEH logArt logPAdm LogOthr;
RUN;

/*Best Model Selection*/
/*Criteria plots and all possible models*/
ods graphics on;
PROC REG PLOTS (label) = criteria;
model logInc = AvgHHSzE logUnemp logWomen LessthanHS logBS logForeign logFS/ selection = adjrsq cp aic sbc;
run;
ods graphics off; quit;

/*Summary and plots for the model selected #4 with 5x-variables */
ods graphics on;
PROC REG data=HHIncTrans2;
model logInc = AvgHHSzE logUnemp LessthanHS logBS logFS;
RUN;
ods graphics off;
run; quit;

/*Diagnostic plots*/
ods graphics on;
proc reg plots(label) = (CooksD RStudentByLeverage);
id GeoId;
Model logInc = AvgHHSzE logUnemp LessthanHS logBS logFS;
Run;
ods graphics off;quit;


/*default forward stepwise selection*/
proc reg data=HHIncTrans2;
model logInc = AvgHHSzE logUnemp logWomen LessthanHS logBS logForeign logFS/ selection = stepwise details=summary;
run;

/*Model Stepwise selection
ods graphics on;
PROC REG data=HHIncTrans1;
model Income = logHS sqrtOccuSer logUnemp logOccuSal logWomen logOccCons ;
RUN;
ods graphics off;
run; quit;*/

/*VIF*/
proc reg data=HHIncTrans2;
model logInc = AvgHHSzE logUnemp LessthanHS logBS logFS/ VIF;
run;quit;

/*Cook's D*/
proc reg data=HHIncTrans2;
model Income = AvgHHSzE logUnemp LessthanHS logBS logFS/ VIF;
output out = diagnostics
P = pred
R = res
STDP = stdp
STDR = stdr
STUDENT = stud
COOKD = cook
H = lev
PRESS = press
RSTUDENT = rstud
DFFITS = dffits
COVRATIO = covratio;
run;

data diagnostics1;
Set diagnostics 
(keep = GeoID GeogName pred res stdp stdr stud cook lev press rstud dffits covratio);
row_num +1;
proc print data= diagnostics1;

proc sgplot data=diagnostics1;
scatter x=Pred y=rstud;
run; ods graphics off; quit;
run;quit;



/*Ridge Regression*/
ods graphics on;
title 'Regression of Household Income of New York City Neighborhoods';
proc reg data= HHIncTrans2 outvif outest = b ridge = 0 to 0.02 by 0.005; *request ridge plots;
model logInc = AvgHHSzE logUnemp LessthanHS logBS logFS;
run;
proc print;

proc reg noprint data= Building outvif
	outest = ridgeout outstb ridge = 0.003 ; *bias value from ridgge plot analysis;
	model logInc =AvgHHSzE logUnemp LessthanHS logBS logFS;
	proc print;
	*the ridge slope coefficients are printed here;
	run;
	ods graphics off;
	quit;



ods graphics on;
title 'Regression of Household Income of New York City Neighborhoods';
proc reg data= HHIncTrans2 outvif outest = b ridge = 0 to 0.02 by 0.005; *request ridge plots;
model logInc = AvgHHSzE logUnemp LessthanHS logBS logFS logWomen logForeign;
run;
proc print;

proc reg noprint data= Building outvif
	outest = ridgeout outstb ridge = 0.003 ; *bias value from ridgge plot analysis;
	model logInc =AvgHHSzE logUnemp LessthanHS logBS logFS logWomen logForeign;
	proc print;
	*the ridge slope coefficients are printed here;
	run;
	ods graphics off;
	quit;




