/*4-1*/
DATA Guilt;
	INPUT reside $ arrest $ imprison $ obs @@;
	CARDS;
	N Y Y 42
	N Y N 109
	N N Y 17
	N N N 75
	Y Y Y 33
	Y Y N 175
	Y N Y 53
	Y N N 359
	;

PROC LOGISTIC DATA= Guilt DESCENDING;
	FREQ obs;
	CLASS reside arrest imprison;
	MODEL imprison = reside arrest/SCALE = NONE AGGREGATE;
RUN;

/*4-5*/
DATA psycho;
	INPUT degree class incident @@;
	CARDS;
	4 1 1 4 1 9 4 1 4 4 1 3 4 0 2
	4 1 0 4 0 1 4 1 3 4 1 3 4 1 7
	4 0 1 4 0 2 3 1 5 3 0 6 3 1 3
	3 0 1 3 1 8 3 1 2 3 0 5 3 1 5
	3 1 9 3 0 3 3 1 3 3 1 1 2 0 0
	2 1 4 2 0 3 2 0 9 2 1 6 2 0 4
	2 0 3 1 1 8 1 1 2 1 1 7 1 0 5
	1 0 4 1 0 4 1 1 8 1 0 8 1 0 9
	;

PROC LOGISTIC DATA=psycho ORDER=DATA;
	CLASS class;
	MODEL degree = class | incident/SCALE=NONE AGGREGATE;
	OUTPUT OUT=prob pred=p;
RUN;

PROC LOGISTIC DATA=psycho ORDER=DATA;
	CLASS class;
	MODEL degree = class incident/SCALE=NONE AGGREGATE;
	OUTPUT OUT=prob pred=p;
RUN;

PROC PRINT data=prob(obs=10);
RUN;

/*4-7*/
DATA safety;
	INPUT seatbelt $ bounce $ death $ obs @@;
	CARDS;
	Y Y Y 659 Y Y N 270 Y N Y 532 Y N N 347
	N Y Y 432 N Y N 532 N N Y 269 N N N 552
	;
PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN seatbelt | bounce | death;
RUN;

PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN seatbelt|bounce seatbelt|death bounce|death;
RUN;

PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN seatbelt|bounce seatbelt|death;
RUN;

PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN seatbelt|bounce bounce|death;
RUN;

PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN  seatbelt|death bounce|death;
RUN;

PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN death seatbelt|bounce;
RUN;

PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN seatbelt|death bounce;
RUN;

PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN bounce|death seatbelt;
RUN;

PROC CATMOD DATA = safety;
	WEIGHT obs;
	MODEL seatbelt*bounce*death = _response_/ NORESPONSE NOITER;
	LOGLIN seatbelt bounce death;
RUN;

PROC LOGISTIC DATA=safety;
	FREQ obs;
	CLASS seatbelt bounce death;
	MODEL death = seatbelt | bounce/SCALE=NONE AGGREGATE;
RUN;

PROC LOGISTIC DATA=safety DESCENDING;
	FREQ obs;
	CLASS seatbelt bounce death;
	MODEL death = seatbelt bounce/SCALE =NONE AGGREGATE;
RUN;

/*4-9*/
DATA reading;
	INPUT time $ gender $ type $ obs @@;
	CARDS;
		<0m m arts 34 <0m m work 64 <0m w arts 49 <0m w work 135
		<30m m arts 29 <30m m work 61 <30m w arts 31 <30m w work 118
		<60m m arts 40 <60m m work 81 <60m w arts 37 <60m w work 142
		<120m m arts 37 <120m m work 65 <120m w arts 32 <120m w work 64
		<240m m arts 24 <240m m work 40 <240m w arts 11 <240m w work 37
		<360m m arts 6 <360m m work 15 <360m w arts 0 <360m w work 3
		>360m m arts 3 >360m m work 7 >360m w arts 4 >360m w work 2
	;

PROC CATMOD DATA = reading;
	WEIGHT obs;
	MODEL time*gender*type = _response_/ NORESPONSE NOITER;
	LOGLIN time | gender | type;
RUN;

PROC CATMOD DATA = reading;
	WEIGHT obs;
	MODEL time*gender*type = _response_/NOITER PRED=FREQ;
	LOGLIN time|gender time|type gender|type;
RUN;
 
PROC CATMOD DATA = reading;
	WEIGHT obs;
	MODEL time*gender*type = _response_/NOITER PRED=FREQ;
	LOGLIN time|gender gender|type;
RUN;
 
PROC CATMOD DATA = reading;
	WEIGHT obs;
	MODEL time*gender*type = _response_/NOITER PRED=FREQ;
	LOGLIN time|gender time|type;
RUN;
PROC CATMOD DATA = reading;
	WEIGHT obs;
	MODEL time*gender*type = _response_/NOITER PRED=FREQ;
	LOGLIN time|type gender|type;
RUN;

PROC CATMOD DATA = reading;
	WEIGHT obs;
	MODEL time*gender*type = _response_/NOITER PRED=FREQ;
	LOGLIN time|gender type;
RUN;
PROC CATMOD DATA = reading;
	WEIGHT obs;
	MODEL time*gender*type = _response_/NOITER PRED=FREQ;
	LOGLIN gender|type time;
RUN;

PROC CATMOD DATA = reading;
	WEIGHT obs;
	MODEL time*gender*type = _response_/NOITER PRED=FREQ;
	LOGLIN time gender|type;
RUN;

/*4-9-2*/
DATA reading2;
	INPUT time $ gender $ type $ obs @@;
	CARDS;
	<0.5h m arts 63 <0.5h m work 125 <0.5h w arts 80 <0.5h w work 253 
	<2h m arts 77 <2h m work 146 <2h w arts 69 <2h w work 206 
	>2h m arts 33 >2h m work 62 >2h w arts 15 >2h w work 42 
;
PROC CATMOD DATA = reading2;
	WEIGHT obs;
	MODEL time*gender*type = _response_/ NORESPONSE NOITER;
	LOGLIN time | gender | type;
RUN;

PROC CATMOD DATA = reading2;
	WEIGHT obs;
	MODEL time*gender*type = _response_/ NORESPONSE NOITER;
	LOGLIN time|gender time|type gender|type;
RUN;


PROC CATMOD DATA = reading2;
	WEIGHT obs;
	MODEL time*gender*type = _response_/ NORESPONSE NOITER;
	LOGLIN time|gender gender|type;
RUN;

PROC CATMOD DATA = reading2;
	WEIGHT obs;
	MODEL time*gender*type = _response_/ NORESPONSE NOITER;
	LOGLIN time|gender time|type;
RUN;

PROC CATMOD DATA = reading2;
	WEIGHT obs;
	MODEL time*gender*type = _response_/ NORESPONSE NOITER;
	LOGLIN time|type gender|type;
RUN;


PROC CATMOD DATA = reading2;
	WEIGHT obs;
	MODEL time*gender*type = _response_/ NORESPONSE NOITER;
	LOGLIN time|gender type;
RUN;


PROC CATMOD DATA = reading2;
	WEIGHT obs;
	MODEL time*gender*type = _response_/ NORESPONSE NOITER;
	LOGLIN time gender|type;
RUN;
/*DATA EXPORT FOR R*/
proc export data=Guilt
   outfile='C:\Users\pkmon\Desktop\Guilt.csv'
   dbms=csv
   replace;
run;

proc export data=psycho
   outfile='C:\Users\pkmon\Desktop\psycho.csv'
   dbms=csv
   replace;
run;

proc export data=safety
   outfile='C:\Users\pkmon\Desktop\safety.csv'
   dbms=csv
   replace;
run;

proc export data=reading
   outfile='C:\Users\pkmon\Desktop\reading.csv'
   dbms=csv
   replace;
run;

proc export data=reading2
   outfile='C:\Users\pkmon\Desktop\reading2.csv'
   dbms=csv
   replace;
run;
