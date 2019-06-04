FILENAME golfsrs 'C:\Users\pkmon\Desktop\Sampling Theory\golfsrs.dat';
DATA golf;
   INFILE golfsrs;
   INPUT rn state $ course $ holes type $ yearblt wkday18
	wkday9 wkend18 wkend9 backtee rating par cart18
	cart9 caddy $ pro $ ;

RUN;

PROC PRINT DATA=golf;
RUN;

PROC SURVEYMEANS MEAN SUM DATA=golf TOTAL=16883;
	VAR wkday9;
	STRATA pro;
	DOMAIN pro;
	TITLE 'Estimate Mean and SD of wkday9';
RUN;
