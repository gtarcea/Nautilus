
#ifndef PolicyUtil_INCLUDED
#define PolicyUtil_INCLUDED

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <iostream>

typedef long int32;
typedef long long int64;

using namespace std;

// debug macros
#define TRC (printf("%s:%d\n", __FILE__, __LINE__))
#if false
#define IS_PRINTABLE(X) (((X)>=' ')&&((X) <='z'))
#define TRC0 cout << strrchr (__FILE__,'/')+1 << ":" << __LINE__ << " " <<
#define TRCasc(A,L) {int Ts=0;for (int Ti=0; Ti<(L);Ti++) { char Tb[5]; int Tv=((char*)(A))[Ti]&255; if ((Ti%20)==0) cout << "\n"; sprintf(Tb,(IS_PRINTABLE(Tv)?"  %c":" %02x"),Tv); cout << Tb; Ts+=Tv;} cout<< "   Sum:"<<Ts<<"\n";}
#define TRChex(A,L) {int Ts=0;for (int Ti=0; Ti<(L);Ti++) { char Tb[5]; int Tv=((char*)(A))[Ti]&255; if ((Ti%10)==0) cout << "\n"; sprintf(Tb," %02x",Tv); cout << Tb; Ts+=Tv;} cout<< "   Sum:"<<Ts<<"\n";}
#define TRC TRC0 "\n";
#define TRC1(P1) TRC0 #P1 << ":" << (P1) << "\n";

#define TRC2(P1,P2) TRC0 #P1 << ":" << (P1) << "   " << #P2 << ":" << (P2) << "\n";
#define TRC3(P1,P2,P3) TRC0 #P1 << ":" << (P1) << "   " << #P2 << ":" << (P2) << "   " << #P3 << ":" << (P3) << "\n";
#define TRCM(M) TRC0 (M) << "\n";
#define TRCB(B) TRC0 #B << ((B) ? " True" : " False") << "\n";
#define TRCHEX(A,L) TRC0 #A << ":" << #L <<"="<<(L) ; TRChex((A),(L))
#define TRCKEY(K) TRC0 "Key: " << (K); if ((K)!=NULL){TRChex ((K)->KeyData(),(K)->KeyLength())}
#endif

#endif
