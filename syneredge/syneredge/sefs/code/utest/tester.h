
#define SHOW(rc,s) if (rc) {printf("%s PASSED\n", s) ;}else{ printf("%s FAILED\n", s) ; exit(1);}

#define S(rc,s) if (rc) {printf("%s PASSED\n", s) ;}else{ printf("%s FAILED\n", s) ; exit(1);}

#define P(s) printf("TESTING %s\n", s) ;
#define PREAMBLE(s) printf("TESTING %s\n", s) ;

