#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAXLINE 200
#define SKIP 0

int main(argc, argv)
     int argc;
     char **argv;
{
  
  char line[1000];
  double x, y, flux, flux_max, area, elong;
  double mag, flux_min;
  double i;

  if(argc != 1) {
    printf("args: \n");
    exit(-1);
  }

  for(i=0; i<SKIP; i++){
    if ( fgets(line, MAXLINE, stdin) == NULL ) { 
	    exit (1);
    }
  }

  flux_min = 1.0;

  while(scanf("%lf %lf %lf %lf %lf %lf", &x, &y, &flux, &area, &flux_max, &elong) != EOF){
    if(flux > 0.0){
      mag = -2.5*log10(flux);
    }else{
      mag = -2.5*log10(flux_min);
    }
    printf("%8.2lf%8.2lf%13.2lf%9.1lf%10.2lf%6.2lf%10.2lf\n",
	   x, y, flux, area, flux_max, elong, mag);
  }
  exit(0);
}

