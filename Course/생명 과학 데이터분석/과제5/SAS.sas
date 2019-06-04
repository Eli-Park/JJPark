/*8-1*/

DATA A;
	INPUT p @@;
	CARDS;
	0.025 0.31 0.009 0.28 0.345 0.42 0.06
	;

	/*ORDER STATISTICS*/
PROC RANK DATA=A OUT=ord; VAR p; RANKS ORDER;
RUN;
DATA order; SET ord;
	k=7; alpha=0.05;
	if ORDER ^= 1 then delete;
	signi = 1-(1-alpha)**(1/k);
	keep alpha order p signi;
	label signi='Critical Value'
		p ='P(1)';
	title "Method Based on Order Statistics";
PROC PRINT label noobs; RUN;

/*Inverse Chi-squre Method*/
DATA B;
	INPUT p @@;
	logp=-2*log(p);
	label logp='-2LOG(P)';
	CARDS;
	0.025 0.31 0.009 0.28 0.345 0.42 0.06
	;

PROC MEANS DATA=B noprint;
	OUTPUT OUT=i_chi SUM=sum;
	VAR logp;

DATA inv_chi; set i_chi;
	chi=sum;
	chi_p=1-probchi(chi, 2*_freq_, 0);
	keep chi chi_p;
	label chi_p='ONE-SIDED P-VALUE'
		chi='CHI-SQUARE';
title 'INVERSE CHI-SQUARE METHOD';
PROC PRINT label noobs DATA=inv_chi;
RUN;

/*INVERSE NORMAL METHOD*/
DATA C;
	INPUT p @@;
	z_i=probit(p);
	CARDS;
	0.025 0.31 0.009 0.28 0.345 0.42 0.06
	;
title;
PROC PRINT label; RUN;

PROC MEANS DATA=C noprint;
	OUTPUT OUT=i_nor SUM=sum;
	VAR z_i;
DATA inv_nor; SET i_nor;
	nor=sum/sqrt(_freq_);
	if nor>0 then nor_p=1-probnorm(nor);
	else nor_p=probnorm(nor);
	nor_p2=2*nor_p;
	keep nor nor_p nor_p2;
	label nor_p='ONE-SIDED P-VALUE'
		nor_p2='TWO-SIDED P-VALUE'
		nor='NORMAL';
	title 'INVERSE NORMAL METHOD';
PROC PRINT label noobs; RUN;

/*LOGIT METHOD*/
DATA D;
	INPUT p @@;
	logit=log(p/(1-p));
	CARDS;
	0.025 0.31 0.009 0.28 0.345 0.42 0.06
	;
PROC MEANS DATA=D noprint;
	OUTPUT OUT=prelogit SUM=sum;
	VAR logit;
DATA logit; SET prelogit;
	pi=3.141592154;
	l=sum*sqrt((3*(5*_freq_+4))/((pi**2)*_freq_*(5*_freq_+2)));
	if l>0 then log_p=1-probt(l, 5*_freq_+4, 0);
	else log_p=probt(l, 5*_freq_+4,0);
	log_p2=2*log_p;
	keep l log_p log_p2;
	label log_p = 'ONE-SIDED P-VALUE'
		log_p2 = 'TWO-SIDED P-VALUE'
		l='L*';
	title 'LOGIT METHOD';
	PROC PRINT label noobs; RUN;

proc means data=C; 
var z_i; 
run; 
 
data homo; 
set C; 
st_z=(z_i+1.0799709)**2; 
run; 
 
proc means data=homo; 
var st_z; 
output out=homo1 sum=sum; 
run; 
 
data homo_test; 
set homo1; 
val=1-probchi(sum, _freq_-1,0); 
keep sum val; 
label sum='chi-square' val='p-value'; 
title 'homogeneity test for p-values'; 
run; 
 
proc print data=homo_test noobs label; 
run;

/*8-5*/
DATA mix;
	INPUT nie nic d @@;
	h_d=sqrt(2)*arsinh(d/(2*sqrt(2)));
	twonih_d=2*nie*h_d;
	CARDS;
	90 90 -0.581 40 40 0.263 36 36 0.381 20 20 0.505 22 22 0.275
	10 10 0.147 10 10 0.039 10 10 0.284 39 39 -0.088 50 50 -0.116
	;
RUN;

PROC PRINT DATA=mix;
RUN;

PROC MEANS DATA=mix SUM; 
	OUTPUT OUT=mmean SUM=sum1 sum2;
	VAR twonih_d nie;
RUN;

DATA mix1; set mmean;
	alpha=0.05;
	hplus=sum1/(2*sum2);
	delta_hat=2*sqrt(2)*sinh(hplus/sqrt(2));
	sig2_hplus=1/(2*sum2);
	h_l=hplus-probit(1-alpha/2)*sqrt(sig2_hplus);
	h_u=hplus+probit(1-alpha/2)*sqrt(sig2_hplus);
	delta_l=2*sqrt(2)*sinh(h_l/sqrt(2));
	delta_u=2*sqrt(2)*sinh(h_u/sqrt(2));
RUN;

PROC PRINT DATA=mix1;
RUN;

/*8-6*/
DATA mix2;
	SET mix;
	q=2*nie*((h_d+0.024766)**2);
RUN;

PROC PRINT DATA=mix2;
RUN;

PROC MEANS DATA=mix2 SUM;
	VAR q;
RUN;

