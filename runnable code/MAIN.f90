    DIMENSION AE(6,6), BE(6,4), CE(3,6), BE1(6,3), CE1(6,6), BE1(6,3), CE1(6,6)
    DIMENSION A(5,5), A1T(5,5), Q(6,5), AKC(3,5), BAKC(5,5), Y(5,5), QC(5,15)
    DIMENSION B(5,3), C(2,5), CT(5,2), QO(5,15), T(5), R(5,5), F(6,5), PSF(5,6), AL(5), OK1(4,3), OK2(4,3), OK(4,3)
    DIMENSION H(3), G(5), CL(5,3), P(5,5), A1(5,5), D(6), AL1(5), BETA(6), E(6,7), PSE(7,6), X(7), CK1(4,3), CK2(4,3)
    DIMENSION CK(4,3), ETA(2), OL(5,2), A2(6,6), AK1(3,2), ALFA2(6), AT(5,5), RR(9,9), AS(6), AK(3,2)
    EQUIVALENCE (QC(1,1), QO(1,1), RR(1,1), E(1,1), CE(1,1), F(1,1))
    EQUIVALENCE ( PSE(1,1), PSF(1,1),BE(1,1), CE(1,1))
    EQUIVALENCE (Q(1,1), CK1(1,1), OK1(1,1),BE1(1,1))
    EQUIVALENCE (CK2(1,1), OK2(1,1), AE(1,1), P(1,1), BAKC(1,1))
    EQUIVALENCE (CK(1,1), QK(1,1), A2(1,1), AK(1,1))
    EQUIVALENCE (AKC(1,1), AS(1), D(1))
    READ (105, 98) N,M,IR
98  FORMAT (3I2)
    READ (105,58) ((A(I,J), I=1,N), J=1,N)
    READ (105,58) ((B(I,J), I=1,N), J=1,IR)
    READ (105,58) ((C(I,J), J=1,N), I=1,M)
58  FORMAT (40 F2.1)
    READ (105,68) (BETA(I), I=1,6)
68  FORMAT (6 F8.2)


    CALL CONOBS (IR, B, QC, A, N)
    NR=N*IR
    CALL ESALON (QC, N, NR)

    DO 19 J=1, NR
    IF (QC(N,J)) 29,19,29
19  CONTINUE
    WRITE (108, 801)
801 FORMAT (//, 5X, THE SYSTEM IS NOT COMPLETELY CONTROLLABLE)
    GO TO 1500
29  IC=(J-1)/IR
    WRITE (108, 70) ((QC (I,J), J=1,NR), I=1,N)
70  FORMAT (20X, ‘MATRIX QC’ // 15 (1X,F 5.2))

    CAL TRAM (A, AT, N, N)
    CALL TRAM (C, CT, M, N)
    CALL CONOBS (M,CT, QO, AT, N)
    NM=N*M
    CALL ESALON (QO, N, NM)


    DO 39 J=1,NM
    IF (QO(N,J)) 49,39,49
39  CONTINUE
    WRITE (108, 901)
901 FORMAT (//, 5X, ‘SYSTEM IS NOT COMPLETELY OBSERVABLE’)
    GO TO 1500
49  IO=(J-1)/M
    WRITE (108, 90) ((QO(I,J), J=1, NM), I=1,N)
90  FORMAT (20X, ‘MATRIX QO’/10(3X, E 9.2))
    DO 310 I=1,IR
    DO 310 J=1,M
310 AK1 (I,J)=0.
    CALL ZADEH (A, N, AL)
    CALL CQ (Q, N, AL)
    K=1
    NM=2*N-K
    CALL PRIM (RR, Q, N, NM, K)
    CALL ESALON (RR, NM, NM)
    IF (RR(NM, NM)) 311, 314, 311
311 WRITE (108, 312)
312 FORMAT (//, 10X, ‘MATRIX A IS CYCLICAL’)
    DO 313 I=1,N
    DO 313 J=1,N
313 A1(I,J)=A(I,J)
    GO TO 399
314 WRITE (108, 315)
315 FORMAT (//, ‘MATRIX A IS NOT CYCLICAL, WE APPLY THE DAVISON ALGORITHM’)
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
    IF (RR(NM, NM) 324, 325, 324
325 WRITE (108, 326)
326 FORMAT (//, 10X, ‘MATRIX Y IS NOT CYCLICAL’)
    K=2
331 NM=2*N-K
    CALL PRIM (RR, Q, N, NM, K)
    CALL ESALON (RR, NM, NM)
    IF (RR(NM, NM) 327, 328, 327
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
324 WRITE (108, 340)
340 FORMAT (//,10X, ‘Y HAS BECOME CYCLICAL’)
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
    IF (QO(,N,N)) 401, 402, 401
402 IF (K-10) 403, 404, 404
403 K=K+1
    S=S+1.
    GO TO 405
404 WRITE (108, 800)
800 FORMAT (//, 5X, ‘G COULD NOT BE FOUND’)
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
409 WRITE (108,900)
900 FORMAT (//, 5X, ‘T COULD NOT BE FOUND’)
    GO TO 1500
406 IF (IC-IO) 11, 13, 12
13  IF (IR-M) 11, 11, 12
11  L=IC


    WRITE (108, 60) L
60  FORMAT (20X, ‘THE NUMBER OF INTEGRATORS’, I2, 10X, ‘L=IC’)
    GO TO 1
12  L=IO
    WRITE (108, 80) L
80  FORMAT (20X, ‘THE NUMBER OF INTEGRATORS’, I2, 10X, ‘L=IO’)
    GO TO 2


1   DO 100 J=1,N
100 P (1,J)=G (J)
    DO 200 I=2, N
    DO 200 J=1, N
    S=0.
    DO 300 K=1, N
300 S=S+P (I-1,K)* A1 (K, J)
200 P (I, J) = S+AL1 (N-I+2) * G (J)
    CALL PROMAT (P, B, CL, N, N, IR)

    IL=N+L
    JC=L+(L+1)*IR
    DO 101 I=1, IL
    DO 101 J=1, JC
101 E(I,J)=0.
    DO 201 J=1, L
    E (J,J)=1.
    DO 201 K=1,N
201 E (J+K, J) = AL1 (N-K+1)
    DO 301 I=1, N
    DO 301 J=1, IR
    E (L+I, L+J) = CL (I,J)
    DO 301 K=1,L
301 E(L-K+I, L+K*IR+J) = CL (I, J)
    DO 102 I=1, N
102 D (I)= BETA (L+N-I+1) – AL1 (N-I+1)
    DO 103 I=1, L
103 D (N+I) = BETA (L-I+1)
    CALL PS (E, IL, JC, PSE)
    CALL PROMAT (PSE, D, X, JC, IL, 1)
    I1=IR+L
    J1=M+L
    DO 41 I=1, I1
    DO 41 J=1, J1
    CK2 (I,J)=0.
41  CK1 (I,J)=0.
    DO 42 I=1, IR
    DO 42 J=1, M
    CK2(I, J)=-ETA(J)* X(L+I+L*IR)
42  CK1 (I,J)=AK1 (I,J)
    DO 43 I=1, IR
    DO 43 J=1, L
43  CK2 (I, M+J)=-X(L+I+(L-J)*IR)+X(J)*X(L+L*IR+I)
    DO 44 J=1,M
44  CK2 (IR+1,J)=ETA (J)
    DO 45 J=1, L
45  CK2 (IR+1, M+J)=-X(J)
    L1=L-1
    IF (L1) 51, 51, 50
50  DO 46 J=1, L1
46  CK2 (J+IR+1, M+J)=1.
51  DO 47 I=1,I1
    DO 47 J=1, J1
47  CK (I,J)=CK1 (I, J)+CK2 (I, J)
    PRINT 59, CK
59  FORMAT (//20X, ‘MATRIX K’// (3E13.6))
    GO TO 500


2   DO 111 J=1,N
111 R (1,J)=T (J)
    DO 112 I=2,N
    DO 112 J=1, N
    S=0.
    DO 113 K=1,N
113 S=S+R(I-1,K)*A1 (J,K)
112 R(I, J)=S+AL1(N-I+2)*T(J)
    CALL PROMAT (R, CT, OL, N, N, M)

    IL=N*L
    JC=L+(L+1)*M
    DO 121 I=1, IL
    DO 121 J=1, JC
121 F(I,J)=0.
    DO 122 J=1, L
    F(I,J)=1.
    DO 122 K=1, N
122 F(J+K,J)=AL1 (N-K+1)
    DO 123 I=1,N
    DO 123 J=1, M
    F(L+I, L+J)=OL (I, J)
    DO 123 K=1,L
123 F(L-K+I, L+K*M+J)=OL (I,J)
    DO 124 I=1,N
124 D(I)=BETA (L+N-I+1)-AL1 (N-I+1)
    DO 125 I=1,L
125 D (N+I)=BETA (L-I+1)

    CAL PS(F, IL, JC, PSF)

    CALL PROMAT (PSF, D, X, JC, IL,1)

    I1=IR+L
    J1=M+L
    DO 141 I=1, I1
    DO 141 J=1, J1
    OK2 (I,J)=0.
141 OK1 (I,J)=0.
    DO 142 I=1, IR
    DO 142 J=1, M
    OK1 (I,J)=AK1 (I,J)
142 OK2 (I, J)=-H(I)*X(L+L*M+J)
    DO 143 I=1, L
    DO 143 J=1, M
143 OK2 (I, J)=-X(L+J+(L-I)*M)+X(I)*X(L+L*M+J)
    DO 144 I=1, IR
144 OK2(I, M+1)=H(I)
    DO 145 J=1, L
145 OK2 (IR+1, M+J)=-X(J)
    L1=L-1
    IF (L1) 61, 61, 62
62  DO 146 J=1, L1
146 OK2 (J+IR+1, J+M)=1.
61  DO 147 I=1, I1
    DO 147 J=1, J1
147 OK (I,J)=OK1 (I,J)+OK2 (I,J)
    PRINT 59, OK

500 I2=N+L
    READ (105, 58) ((A(I,J), I=1,N) , J=1,N)
    N1=N+1
    IR1=IR+1
    M1=M+1
    DO 130 I=1, I2
    DO 130 J=1, I2
130 AE (I,J)=0.
    DO 131 I=1, N
    DO 131 J=1, N
131 AE (I,J)=A(I,J)
    DO 132 I=1,I2
    DO 132 J=1, I1
132 BE (I,J)=0.
    DO 133 I=1,N
    DO 133 J=1, IR
133 BE (I,J)=B(I,J)
    DO 134 I=1, L
134 BE (N+I, IR+I)=1.
    DO 135 I=1, J1
    DO 135 J=1, I2
135 CE (I, J)=0.
    DO 136 I=1,M
    DO 136 J=1, N
136 CE (I,J)= C(I,J)
    DO 137 I=1,L
137 CE (M+I, N+I)=1.
    IF (IC-IO) 139, 160, 140
160 IF (IR-M) 139, 139, 140
139 CALL PROMAT (BE, CK, BE1, I2, I1, J1)
    GO TO 150
140 CALL PROMAT (BE, OK, BE1, I2, I1, J1)
150 CALL PROMAT (BE1, CE, CE1, I2, J1, J2)
    DO 138 I=1, I2
    DO 138 J=1, I2
138 A2 (I,J)= AE (I,J) +CE1 (I,J)
    CALL ZADEH (A2, I2, ALFA2)
    WRITE (108, 600)
600 FORMAT (20X, 'ALGORITHM CHECK’)
    WRITE (108,180) (I, BETA (I),          I, ALFA2 (I), I=1, I2)
180 FORMAT (20X, ‘BETA (‘I2’)=’ ,F8.2, 10X, ‘ALFA2(‘I2’)=’, F8.2)
1500 STOP
     END
