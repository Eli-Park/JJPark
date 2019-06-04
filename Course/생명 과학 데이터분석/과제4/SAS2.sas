/*7-2*/
DATA cancer1;
INPUT time censor freq @@;
CARDS;
	0.5 1 82 0.5 0 0 1.5 1 30 1.5 0 8 2.5 1 27 2.5 0 8 3.5 1 22 3.5 0 7
	4.5 1 26 4.5 0 7 5.5 1 25 5.5 0 28 6.5 1 20 6.5 0 31
	7.5 1 11 7.5 0 32 8.5 1 14 8.5 0 24 9.5 1 13 9.5 0 27 10.5 1 5 10.5 0 22
	11.5 1 5 11.5 0 23 12.5 1 5 12.5 0 18 13.5 1 2 13.5 0 9 14.5 1 3 14.5 0 7
	15.5 1 3 15.5 0 11
	;
RUN;

PROC LIFETEST METHOD=LIFE
	INTERVALS = 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
	PLOTS = (S, H)
	GRAPHICS;
	TIME time*censor(0);
	FREQ freq;
RUN;




/*7-3*/

DATA cancer2;
	INPUT time censor @@;
	CARDS;
	2 1 4 1 5 1 10 1 10 0 12 1 12 0 14 1 14 1 15 1 16 1 18 1 19 1 23 1 25 1 26 0 27 1 30 0
	31 1 34 1 35 1 37 0 38 1 39 1 42 0 43 0 46 1 47 0 49 1 50 1 53 0 54 0
;
RUN;

PROC LIFETEST METHOD=KM PLOTS=(S) GRAPHICS OUTSURV=a;
	TIME time*censor(0);
PROC PRINT DATA=a;
RUN;

/*7-4*/
DATA Hod;
	INPUT time censor group @@;
	CARDS;
	1 1 1 2 1 1 5 1 1 5 1 1 5 1 1 7 1 1 9 1 1 11 1 1 11 1 1 13 1 1 13 1 1 16 1 1 20 1 1 21 1 1 22 0 1 22 1 1 31 0 1 33 0 1 37 0 1 43 1 1
	1 1 2 3 1 2 4 1 2 4 1 2 5 1 2 7 1 2 7 1 2 9 1 2 9 1 2 14 0 2 17 1 2 19 0 2 27 0 2 30 0 2 41 0 2
	;
RUN;

proc lifetest data = Hod plots = (s) graphics;
	time time*censor(0);
	strata group;
	symbol1 v = none color=b line=1;
	symbol2 v = none color=b line=2;
run;

/*7-5*/
DATA dia;
	INPUT censor time age BMI dage smoke @@;
	CARDS;
	1 3.6 63 25.1 46 1 1 15.4 71 26.0 59 0 1 11.3 51 32.0 49 1 1 10.3 59 28.1 57 1
	1 5.8 50 26.1 49 1 0 8.0 66 45.3 49 0 1 14.6 42 30.0 41 1 1 11.4 40 35.7 36 1
	1 7.2 67 28.1 61 0 1 5.5 86 32.9 61 0 1 11.1 52 37.6 46 1 1 16.5 42 43.4 37 0
	1 10.9 60 25.4 60 0 1 2.5 75 49.7 57 1 0 10.8 81 35.2 81 0 1 4.7 60 37.3 39 0
	0 5.5 60 26.0 42 0 1 4.5 63 21.8 60 1 1 9.0 62 18.2 43 0 1 6.8 57 34.1 41 1
	0 3.6 71 25.6 54 1 1 12.1 58 35.1 45 0 1 8.1 42 32.5 28 1 1 11.1 45 44.1 40 0
	0 7.0 66 29.7 59 1 1 1.5 61 29.2 54 0 1 11.7 48 25.2 30 1 1 0.3 82 25.3 50 0
	;
RUN;

PROC LIFEREG DATA=dia;
	CLASS smoke;
	MODEL time*censor(0) = age BMI dage smoke/DIST=WEIBULL;
RUN;


PROC LIFEREG DATA=dia;
	CLASS smoke;
	MODEL time*censor(0) = /DIST=WEIBULL;
RUN;

PROC LIFETEST DATA=dia PLOTS=(LS, LLS) GRAPHICS;
	TIME time*censor(0);
RUN;

PROC LIFETEST DATA=dia OUTSURV = a;
	TIME time*censor(0);
RUN;


DATA b; SET a;
	S = SURVIVAL;
	LOGIT = LOG((1-S)/S);
	LNORM = PROBIT(1-S);
	LTIME=LOG(time);

PROC GPLOT DATA=b;
	SYMBOL1 VALUE=NONE I=JOIN;
	PLOT LOGIT*LTIME LNORM*LTIME;
RUN;


/*7-6*/
DATA Leukemia;
	INPUT time censor age x @@;
	CARDS;
	18 1 0 0 9 1 0 1 28 0 0 0 31 1 0 1 39 0 0 1 19 0 0 1 45 0 0 1 6 1 0 1 8 1 0 1 15 1 0 1 23 1 0 0 28 0 0 0 7 1 0 1 12 1 1 0 9 1 1 0
	8 1 1 0 2 1 1 1 26 0 1 0 10 1 1 1 4 1 1 0 3 1 1 0 4 1 1 0 18 1 1 1 8 1 1 1 3 1 1 1 14 1 1 1 3 1 1 0 13 1 1 1 13 1 1 1 35 0 1 0
	;
RUN;

proc phreg data = Leukemia;
model time*censor(0) = age x;
run;

proc export data=cancer1
   outfile='C:\Users\pkmon\Desktop\cancer1.csv'
   dbms=csv
   replace;
run;


proc export data=cancer2
   outfile='C:\Users\pkmon\Desktop\cancer2.csv'
   dbms=csv
   replace;
run;


proc export data=Hod
   outfile='C:\Users\pkmon\Desktop\Hod.csv'
   dbms=csv
   replace;
run;


proc export data=dia
   outfile='C:\Users\pkmon\Desktop\dia.csv'
   dbms=csv
   replace;
run;

proc export data=Leukemia
   outfile='C:\Users\pkmon\Desktop\Leukemia.csv'
   dbms=csv
   replace;
run;

