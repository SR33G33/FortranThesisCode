    SUBROUTINE PROMAT (A,B,C,M,L,N)
    DIMENSION A(M,L),B (L,N),C(M,N)
    DO 1 I=1,M
    DO 1 J=1,N
    C(1,J)=0
    DO 1 K=1,L
  1 C(I,J)=C(I,J)+A(I,K)*B(K,J)
    RETURN
    END

    SUBROUTINE UNIT(B,N)
    DIMENSION B(N,N)
    DO 1 I=1,N
    DO 1 J=1,N
  1 B(I,J)=0
    DO 2 I=1,N
  2 B(I,I)=1
    RETURN
    END

    SUBROUTINE TRAM (A,B,M,N)
    DIMENSION A(M,N),B(N,M)
    DO 1 I=1,M
    DO 1 J=1,N
  1 B(J,I)=A(I,J)
    RETURN
    END

    SUBROUTINE HAZ(S,N,Z)
    DIMENSION Z(N)
    Z(1)=S/N
    DO 1 I=2,N
  1 Z(I)=(Z(I-1)+INT(Z(I-1)*N))/N+SIN(I+1.)
    RETURN
    END

    SUBROUTINE ZADEH (A,N,ALFA)
    DIMENSION A(N,N),AK(9,9),E(9,9),UN(9,9),ALFA(N)
    DO 3 I=1,N
    DO 3 J=1,N
    IF(I-J)10,11,10
 10 UN(I,J)=0
    AK(I,J)=0
    GO TO 3
 11 UN (I,J)=1
    AK(I,J)=1
 3  CONTINUE
    DO 1 M=1,N
    M1=N-M+1
    DO 5 I=1,N
    DO 5 J=1,N
    E(I,J)=0
    DO 5 L=1,N
  5 E(I,J)=E(I,J)+A(I,L)*AK(L,J)
    TRAC=O
    DO 6 I=1,N
  6 TRAC=TRAC+E(I,I)
    ALFA(M1)=-(1./M)*TRAC
    ALFA1=ALFA(M1)
    IF(M-N) 2,50,50
  2 DO 1 I=1,N
    DO 1 J=1,N
  1 AK(I,J)=ALFA1*UN(I,J)+E(I,J)
 50 RETURN
    END

     SUBROUTINE DIAG(A,B,N,RO,I1,I2)
     INTEGER RO
     DIMENSION A(N,N),B(N,N),R(9),D(9)
     CALL UNIT(B,N)
     DO 1 I=1,N
     R(I)=O
   1 D(I)=A(I,I)
     DO 12 K=1,N
     EDM=D(1)
     L=1
     DO 3 I=2,N
     IF(EDM-D(I)) 4,3,3
   4 EDM=D(I)
     L=I
   3 CONTINUE
     IF(EDM-0.00001) 5,6,6
   6 DO 7 J=1,N
     A(L,J)=A(L,J)/SQRT(EDM)
     A(J,L)=A(L,J)
   7 B(L,J)=B(L,J)/SQRT(EDM)
     A(L,L)=1
     DO 8 I=1,N
     IF(I-L) 9,8,9
   9 DO 8 J=1,N
     A(I,J)=A(I,J)-A(L,I)*A(L,J)
     B(I,J)=B(I,J)-B(L,J)*A(L,I)
   8 CONTINUE
     DO 10 I=1,N
     IF(I-L) 11,10,11
  11 A(L,I)=0
  10 CONTINUE
     R(L)=-1
     DO 12 I=1,N
  12 D(I)=A(I,I)+R(I)
   5 RO=O
     DO 14 I=1,N
  14 RO=RO-R(I)
     I=1
  17 IF(R(I)) 15,16,15
  16 I=I+1
     GO TO 17
  15 I1=I
     I=N
  20 IF (R(I)) 18,19,18
  19 I=I-1
     GO TO 20
  18 I2=I
     RETURN
     END




     SUBROUTINE ESALON(A,N,M)
     DIMENSION A(N,M)
     DATA EPS/1.E-6/
     K=1
     DO 8 J=1,M
     PIV=0
     DO 2 I=K,N
     IF(ABS(A(I,J))-PIV) 2,2,1
   1 PIV=ABS(A(I,J))
     LOC=I
   2 CONTINUE
     IF(FIV-EPS) 8,8,3
   3 DO 4 L=1,N
     S=A(K,L)
     A(K,L)=A(LOC,L)
   4 A(LOC,L)=S
     S=A(K,J)
     DO 5 L=K,M
   5 A(K,L)=A(K,L)/5
     K1=K+1
     DO 6 I=K1,N
     DO 6 L=J ,M
   6 A(I,L)=A(I,L)-A(K,L)*A(I,J)
     IF(K-N+1)7,9,9
   7 K=K+1
   8 CONTINUE
   9 RETURN
     END

    SUBROUTINE CONOBS(IV,V,W,AA,N)
    DIMENSION W(5,15),V(N,IV),AA(N,N)
    DO 1 I=1,N
    DO 1 J=1,IV
  1 W(I,J)=V(I,J)
    N2=N-1
    DO 2 IQ=1,N2
    DO 2 I=1,N
    DO 2 J=1,IV
    S=O
    DO 3 K=1,N
  3 S=S+AA(I,K)*W(K,J+(IQ-1)*IV)
  2 W(I,J+IQ*IV)=S
    RETURN
    END

    SUBROUTINE PRIM(R,Q,N,NM,IP)
    DIMENSION Q(6,5),R(NM,NM)
    M=N-IP
    DO 1 I=1,NM
    DO 1 J=1,NM
  1 R(I,J)=0
    DO 2 J=1,N
    MJ=M+J
    DO 2 I=J,MJ
  2 R(I,M+N+1-J)=Q(M+1+J-I,IP+1)
    DO 3 J=1,M
    NJ=N+J
    DO 3 I=J,NJ
  3 R(I,J)=Q(N+1+J-I,1)
    RETURN
    END

    SUBROUTINE PS (L, N, R, LPI)
    INTEGER R, RO, RANG1, RANG2, P, Q, Q1
    REAL L(6,7), LT(7,6), LPI(7,6), A(6,6), B(6,6), IM(6,6), IMT(6,6), C(6,6), U(6,6), UT(6,6), D(6,6), API(6,6), C1(6,6)
    IF (N-R) 2,2,1
    1	M=R
    CALL TRAM(L, LT, N, M)
    CALL PROMAT(L, LT, A, M, N, M)
    GO TO 3
    2	M=N
    CALL TRAM(L, LT, M, R)
    CALL PROMAT(L, LT, A, M, R, M)
    3	DO 4 I=1,M
    DO 4 J=1,M
    4	B(I,J)=A(I,J)
    CALL DIAG (B, IM, M, RANG1, I11, I12)
    IF (RANG1-M) 6, 5, 6
    5	CALL TRAM(IM,IMT,M,M)
    CALL PROMAT(IMT, IM, API, M, M, M)
    GO TO 50
    6	DO 7 I=1,M
    DO 7 J=1,M
    IF (I-J) 9, 8, 9
    8	IF (B(I,I)-1) 11,10,11
    10	U(I,J)=1.
    GO TO 7
    11	U(I,J)=0.
    9	IF (B(I,I)-1) 12,11,12
    12	U(I,J)=-IM(I,J)
    7	CONTINUE
    CALL TRAM (U, UT, M, M)
    CALL PROMAT (UT, A, C1, M, M, M)
    CALL PROMAT (C1, U, C, M, M, M)
    RO=1
             22   P=0
                     DO 14 I=I11, I12
                     IF (B (I,I)-1) 14,15,14
              15   P=P+1
                     Q=1
                     DO 14 J=I11, I12
                     IF (B (J,J)-1) 14,16,14
              16   IF (RO-1) 17, 18, 17
                17     C(I,J)=D(P,Q)
                          GO TO 19
                18      D(P,Q)=C(I,J)
    19	   Q=Q+1
                14       CONTINUE
           IF (RO-1) 21, 20, 21
    20	Q1=Q-1
    CALL DIAG (D, IM, P, RANG2, I21,I22)
    CALL TRAM (IM, IMT, P,P)
    CALL PROMAT (IMT, IM, D, P,P, P)
    RO=2
    GO TO 22
    21	CALL PROMAT (U,C,C1, M,M,M)
    CALL PROMAT (C1, UT, API, M,M, M)
    50	 IF (M-N) 23,24,23
    23     CALL PROMAT (API, LT, LPI, M, M, N)
      GO TO 25
                  24   CALL PROMAT (LT, API, LPI, R, M, M)
                   25   RETURN
                           END


    SUBROUTINE CQ(Q,N,AL)
    DIMENSION Q(6, 5), AL (N)
    Q(N+1,1)=1.
    DO 1 I=1,N
  1 Q(I,1)=AL(I)
    DO 2 K=2,N
    I1=N+2-K
    DO 2 I=1,I1
  2 Q(I,K)=I*Q(I+1, K-1)
    RETURN
    END

DIMENSION AE(6,6), BE(6,4), CE(3,6), BE1(6,3), CE1(6,6)
DIMENSION A(5,5), A1T(5,5), Q(6,5), AKC(3,5), BAKC(5,5), Y(5,5), QC(5,15)
DIMENSION B(5,3), C(2,5), CT(5,2), QO(5,15), T(5), R(5,5), F(6,5), PSF(5,6), AL(5), OK1(4,3), OK2(4,3), OK(4,3)
DIMENSION H(3), G(5), CL(5,3), P(5,5), A1(5,5), D(6), AL1(5), BETA(6), E(6,7), PSE(7,6), X(7), CK1(4,3), CK2(4,3)
DIMENSION CK(4,3), ETA(2), OL(5,2), A2(6,6), AK1(3,2), ALFA2(6), AT(5,5), RR(9,9), AS(6), AK(3,2)
EQUIVALENCE (QC(1,1), QO(1,1), RR(1,1), E(1,1), CE(1,1), F(1,1))
EQUIVALENCE ( PSE(1,1), PSF(1,1),BE(1,1), CE(1,1))
EQUIVALENCE (Q(1,1), CK1(1,1), OK1(1,1),BE1(1,1))
EQUIVALENCE (CK2(1,1), OK2(1,1), AE(1,1), P(1,1), BAKC(1,1))
EQUIVALENCE (CK(1,1), QK(1,1), A2(1,1), CL (1,1), OL (1,1))
EQUIVALENCE (Y(1,1), A1T(1,1))
EQUIVALENCE (X(1), R(1,1), ALFA2 (1))
EQUIVALENCE (AT(1,1),AK(1,1))
EQUIVALENCE (AKC(1,1), AS(1), D(1))
READ (105, 98) N,M,IR
98 FORMAT (3I2)
READ (105,58) ((A(I,J), I=1,N), J=1,N)
READ (105,58) ((B(I,J), I=1,N), J=1,IR)
READ (105,58) ((C(I,J), J=1,N), I=1,M)
58 FORMAT (40 F2.1)
READ (105,68) (BETA(I), I=1,6)
68 FORMAT (6 F8.2)


CALL CONOBS (IR, B, QC, A, N)
NR=N*IR
CALL ESALON (QC, N, NR)

DO 19 J=1, NR
IF (QC(N,J)) 29,19,29
19 CONTINUE
801 FORMAT (//,5X,’THE SYSTEM IS NOT COMPLETELY CONTROLLABLE’)
WRITE (108, 801)
GO TO 1500
29 IC=(J-1)/IR
70 FORMAT (20X,‘MATRIX QC’ // 15 (1X,F5.2))
WRITE (108, 70) ((QC (I,J), J=1,NR), I=1,N)
CALL TRAM(A,AT,N,N)
CALL TRAM(C,CT,M,N)
CALL CONOBS (M,CT, QO, AT, N)
NM=N*M
CALL ESALON (QO, N, NM)


DO 39 J=1,NM
IF (QO(N,J)) 49,39,49
39 CONTINUE
901 FORMAT (//, 5X,‘SYSTEM IS NOT COMPLETELY OBSERVABLE’)
WRITE (108, 901)
GO TO 1500
49 IO=(J-1)/M
90 FORMAT (20X,‘MATRIX QO’/10(3X, E 9.2))
WRITE (108, 90) ((QO(I,J), J=1, NM), I=1,N)
DO 310 I=1,IR
DO 310 J=1,M
310 AK1 (I,J)=0
CALL ZADEH (A, N, AL)
CALL CQ (Q, N, AL)
K=1
NM=2*N-K
CALL PRIM (RR, Q, N, NM, K)
CALL ESALON (RR, NM, NM)
IF (RR(NM, NM)) 311, 314, 311
312 FORMAT (//, 10X,‘MATRIX A IS CYCLICAL’)
311 WRITE (108, 312)
DO 313 I=1,N
DO 313 J=1,N
313 A1(I,J)=A(I,J)
GO TO 399
315 FORMAT (//,‘MATRIX A IS NOT CYCLICAL, WE APPLY THE DAVISON ALGORITHM’)
314 WRITE (108, 315)
K=2
320 NM=2*N-K
CALL PRIM (RR, Q, N, NM, K)
CALL ESALON (RR, NM, NM)
IF (RR(NM, NM)) 316, 317, 316
317 IF (K-N+1) 318, 319, 319
318 K=K+1
GO TO 320
319 ID1=N
GO TO 321
316 ID1=K
321 MR=IR*M
S=1.3
336 CALL HAZ (S, MR, AS)
DO 322 I=1,IR
DO 322 J=1,M
322 AK (I,J)=AS(J+(I-1)*M)
CALL PROMAT (AK, C, AKC, IR, M, N)
CALL PROMAT (B, AKC, BAKC, N, IR, N)
DO 323 I=1,N
DO 323 J=1,N
323 Y(I,J)=A(I,J)+BAKC(I,J)
CALL ZADEH (Y,N, AL)
CALL CQ (Q, N, AL)
K=1
NM=2*N-K
CALL PRIM (RR, Q, N, NM, K)
CALL ESALON (RR, NM, NM)
IF (RR(NM, NM)) 324, 325, 324
326 FORMAT (//, 10X,‘MATRIX Y IS NOT CYCLICAL’)
325 WRITE (108, 326)
K=2
331 NM=2*N-K
CALL PRIM (RR, Q, N, NM, K)
CALL ESALON (RR, NM, NM)
IF (RR(NM, NM)) 327, 328, 327
328 IF (K-N+1) 329, 330, 330
329 K=K+1
GO TO 331
330 ID2=N
GO TO 332
327 ID2=K
332 IF (ID2-ID1) 333, 338, 338
333 ID1=ID2
DO 335 I=1,N
DO 335 J=1,N
335 A (I,J)=Y (I,J)
DO 337 I=1, IR
DO 337 J=1,M
337 AK1 (I,J)=AK1 (I,J)+AK (I,J)
GO TO 334
338 DO 339 I=1,N
DO 339 J=1,N
339 A (I,J)=Y (I,J)-BAKC (I,J)
334 S=S+1.
GO TO 336
340 FORMAT (//,10X,‘Y HAS BECOME CYCLICAL’)
324 WRITE (108, 340)
DO 341 I=1,N
DO 341 J=1, N
341 A1 (I,J)=Y(I,J)
DO 342 I=1, IR
DO 342 J=1, M
342 AK1 (I,J)=AK1 (I,J)+AK (I,J)
399 CALL ZADEH (A1, N, AL1)
K=1
S=1.7
405 CALL HAZ (S, M, ETA)
CALL TRAM (A1, A1T, N, N)
CALL PROMAT (CT, ETA, G, N, M, 1)
CALL CONOBS (1, G, QO, A1T, N)
CALL ESALON (QO, N, N)
IF (QO(N,N)) 401, 402, 401
402 IF (K-10) 403, 404, 404
403 K=K+1
S=S+1.
GO TO 405
800 FORMAT (//, 5X,‘G COULD NOT BE FOUND’)
404 WRITE (108, 800)
GO TO 1500
401 K=1
S=S+1.3
410 CALL HAZ (S, IR, H)

CALL PROMAT (B, H, T, N, IR, 1)
CALL CONOBS (1, T, QC, A1, N)
CALL ESALON (QC, N, N)
IF (QC (N,N)) 406, 407, 406
407 IF (K-10) 408,409, 409
408 K=K+1
S=S+1.
GO TO 410
900 FORMAT (//, 5X, ‘T COULD NOT BE FOUND’)
409 WRITE (108,900)
GO TO 1500
406 IF (IC-IO) 11, 13, 12
13 IF (IR-M) 11, 11, 12
11 L=IC
60 FORMAT (20X,‘THE NUMBER OF INTEGRATORS’, I2, 10X, ‘L=IC’)
WRITE (108, 60) L
GO TO 1
12 L=IO
80 FORMAT (20X,‘THE NUMBER OF INTEGRATORS’, I2, 10X, ‘L=IO’)
WRITE (108, 80) L
GO TO 2
1 DO 100 J=1,N
100  P(1,J)=G (J)
DO 200 I=2,N
DO 200 J=1,N
S=0.
DO 300 K=1, N
300 S=S+P(I-1,K)* A1 (K, J)
200 P (I, J) = S+AL1 (N-I+2) * G (J)
CALL PROMAT (P, B, CL, N, N, IR)

IL=N+L
JC=L+(L+1)*IR
DO 101 I=1,IL
DO 101 J=1,JC
101 E(I,J)=0
DO 201 J=1,L
E (J,J)=1
DO 201 K=1,N
201 E (J+K, J) = AL1 (N-K+1)
DO 301 I=1,N
DO 301 J=1,IR
E (L+I, L+J) = CL (I,J)
DO 301 K=1,L
301 E(L-K+I, L+K*IR+J) = CL (I, J)
DO 102 I=1,N
102 D(I)= BETA (L+N-I+1) - AL1 (N-I+1)
DO 103 I=1,L
103 D (N+I) = BETA (L-I+1)
CALL PS (E, IL, JC, PSE)
CALL PROMAT (PSE, D, X, JC, IL, 1)
I1=IR+L
J1=M+L
DO 41 I=1,I1
DO 41 J=1,J1
CK2 (I,J)=0
41 CK1 (I,J)=0
DO 42 I=1,IR
DO 42 J=1,M
CK2(I,J)=-ETA(J)* X(L+I+L*IR)
42 CK1 (I,J)=AK1 (I,J)
DO 43 I=1,IR
DO 43 J=1,L
43 CK2 (I, M+J)=-X(L+I+(L-J)*IR)+X(J)*X(L+L*IR+I)
DO 44 J=1,M
44 CK2 (IR+1,J)=ETA (J)
DO 45 J=1,L
45 CK2 (IR+1, M+J)=-X(J)
L1=L-1
IF (L1) 51, 51, 50
50 DO 46 J=1, L1
46 CK2 (J+IR+1, M+J)=1.
51 DO 47 I=1,I1
DO 47 J=1,J1
47 CK (I,J)=CK1 (I, J)+CK2 (I, J)
59 FORMAT (//20X,‘MATRIX K’// (3E13.6))
PRINT 59, CK
GO TO 500
2 DO 111 J=1,N
111 R (1,J)=T (J)
DO 112 I=2,N
DO 112 J=1,N
S=0.
DO 113 K=1,N
113 S=S+R(I-1,K)*A1 (J,K)
112 R(I, J)=S+AL1(N-I+2)*T(J)
CALL PROMAT (R, CT, OL, N, N, M)
IL=N*L
JC=L+(L+1)*M
DO 121 I=1,IL
DO 121 J=1,JC
121 F(I,J)=0.
DO 122 J=1,L
F(I,J)=1.
DO 122 K=1,N
122 F(J+K,J)=AL1 (N-K+1)
DO 123 I=1,N
DO 123 J=1,M
F(L+I, L+J)=OL(I, J)
DO 123 K=1,L
123 F(L-K+I, L+K*M+J)=OL (I,J)
DO 124 I=1,N
124 D(I)=BETA(L+N-I+1)-AL1(N-I+1)
DO 125 I=1,L
125 D (N+I)=BETA (L-I+1)
CALL PS(F, IL, JC, PSF)
CALL PROMAT (PSF, D, X, JC, IL,1)
I1=IR+L
J1=M+L
DO 141 I=1,I1
DO 141 J=1,J1
OK2 (I,J)=0
141 OK1 (I,J)=0
DO 142 I=1,IR
DO 142 J=1,M
OK1(I,J)=AK1(I,J)
142 OK2(I, J)=-H(I)*X(L+L*M+J)
DO 143 I=1,L
DO 143 J=1,M
143 OK2 (I, J)=-X(L+J+(L-I)*M)+X(I)*X(L+L*M+J)
DO 144 I=1,IR
144 OK2(I, M+1)=H(I)
DO 145 J=1,L
145 OK2 (IR+1, M+J)=-X(J)
L1=L-1
IF (L1) 61, 61, 62
62 DO 146 J=1,L1
146 OK2 (J+IR+1, J+M)=1.
61 DO 147 I=1,I1
DO 147 J=1,J1
147 OK (I,J)=OK1(I,J)+OK2(I,J)
PRINT 59,OK
500 I2=N+L
READ (105, 58) ((A(I,J),I=1,N),J=1,N)
N1=N+1
IR1=IR+1
M1=M+1
DO 130 I=1,I2
DO 130 J=1,I2
130 AE (I,J)=0
DO 131 I=1,N
DO 131 J=1,N
131 AE (I,J)=A(I,J)
DO 132 I=1,I2
DO 132 J=1,I1
132 BE (I,J)=0
DO 133 I=1,N
DO 133 J=1,IR
133 BE (I,J)=B(I,J)
DO 134 I=1,L
134 BE (N+I, IR+I)=1
DO 135 I=1,J1
DO 135 J=1,I2
135 CE (I, J)=0
DO 136 I=1,M
DO 136 J=1,N
136 CE (I,J)= C(I,J)
DO 137 I=1,L
137 CE (M+I, N+I)=1.
IF (IC-IO) 139, 160, 140
160 IF (IR-M) 139, 139, 140
139 CALL PROMAT (BE, CK, BE1, I2, I1, J1)
GO TO 150
140  CALL PROMAT (BE, OK, BE1, I2, I1, J1)
150  CALL PROMAT (BE1, CE, CE1, I2, J1, J2)
DO 138 I=1,I2
DO 138 J=1,I2
138 A2(I,J)= AE(I,J) +CE1(I,J)
CALL ZADEH (A2, I2, ALFA2)
WRITE (108, 600)
600 FORMAT (20X,‘ALGORITHM CHECK’)
WRITE (108, 600)
180 FORMAT (20X,‘BETA (‘I2’)=’,F8.2, 10X,‘ALFA2(‘I2’)=’,F8.2)
WRITE (108,180) (I, BETA (I),          I, ALFA2 (I), I=1,I2)
1500 STOP
END
