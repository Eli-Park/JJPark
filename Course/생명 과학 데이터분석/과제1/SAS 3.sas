/*3-1*/

DATA Depress;
	INPUT thr $ dep $ obs @@;
	CARDS;
	1 1 5
	1 2 21
	2 1 8
	2 2 82
	;
RUN;
PROC FREQ DATA = Depress;
	TABLES thr*dep/MEASURES;
	WEIGHT obs;
RUN;

/*3-4*/
DATA HIV;
	INPUT hpv $ hiv $ obs @@;
	CARDS;
	1 1 23
	1 2 4
	1 3 10
	2 1 10
	2 2 14
	2 3 35
	;
RUN;

PROC FREQ DATA=HIV;
	WEIGHT obs;
	TABLES hpv*hiv/CHISQ;
RUN;

/*3-5*/
DATA CANCER;
	INPUT treat $ result $ obs @@;
	CARDS;
	Oper Cure 21
	Oper Ncure 2
	Rad Cure 15
	Rad Ncure 3
	;
RUN;

PROC FREQ DATA = CANCER;
	WEIGHT obs;
	TABLES treat*result/EXACT;
RUN;

/*3-7*/
DATA Approval;
	INPUT first $ second $ count;
	CARDS;
	Agr Agr 794
	Agr Dis 150
	Dis Agr 86
	Dis Dis 570
	;
RUN;

ODS SELECT McNemarsTest;
PROC FREQ DATA = Approval;
	WEIGHT count;
	TABLES first*second/AGREE;
RUN;

/*3-8*/
DATA ERY;
	INPUT Severity $ Gender $ Survival $ count @@;
	CARDS;
	1 Male Dead 2
	1 Male Arrive 21
	1 Female Dead 0
	1 Female Arrive 10
	2 Male Dead 2
	2 Male Arrive 40
	2 Female Dead 0
	2 Female Arrive 18
	3 Male Dead 6
	3 Male Arrive 33
	3 Female Dead 0
	3 Female Arrive 10
	4 Male Dead 17
	4 Male Arrive 16
	4 Female Dead 0
	4 Female Arrive 4
	;
RUN;

PROC FREQ DATA = ERY;
	WEIGHT count;
	TABLES Severity*Gender*Survival/CMH NOROW NOCOL;
RUN;

PROC FREQ DATA = ERY;
	WEIGHT count;
	TABLES Gender*Survival/CHISQ;
RUN;

/*3-9*/
DATA COR;
	INPUT dignosis disease count;
	CARDS;
	0 0 302
	0 1 80
	1 0 179
	1 1 372

PROC FREQ DATA=COR;
	WEIGHT count;
    TABLES dignosis*disease;
RUN;




/*Data Export for R*/
proc export data=Depress
   outfile='C:\Users\pkmon\Desktop\Depress.csv'
   dbms=csv
   replace;
run;

proc export data=HIV
   outfile='C:\Users\pkmon\Desktop\HIV.csv'
   dbms=csv
   replace;
run;

proc export data=CANCER
   outfile='C:\Users\pkmon\Desktop\CANCER.csv'
   dbms=csv
   replace;
run;

proc export data=ERY
   outfile='C:\Users\pkmon\Desktop\ERY.csv'
   dbms=csv
   replace;
run;

proc export data=Approval
   outfile='C:\Users\pkmon\Desktop\Approval.csv'
   dbms=csv
   replace;
run;

proc export data=COR
   outfile='C:\Users\pkmon\Desktop\COR.csv'
   dbms=csv
   replace;
run;
