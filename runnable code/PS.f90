    SUBROUTINE PS (L, N, R, LPI)
    INTEGER R, RO, RANG1, RANG2, P, Q, Q1
    REAL L(6,7), LT(7,6), LPI(7,6), A(6,6), B(6,6), IM(6,6), IMT(6,6), C(6,6), U(6,6), UT(6,6), D(6,6), API(6,6), C1(6,6)
    IF (N-R) 2,2,1
1	  M=R
    CALL TRAM (L, LT, N, M)
    CALL PROMAT (L, LT, A, M, N, M)
    GO TO 3
2	  M=N
    CALL TRAM (L, LT, M, R)
    CALL PROMAT (L, LT, A, M, R, M)
3	  DO 4 I=1,M
    DO 4 J=1, M
4	  B(I,J)=A(I,J)
    CALL DIAG (B, IM, M, RANG1, I11, I12)
    IF (RANG1-M) 6, 5, 6
5	  CAL TRAM ( IM,IMT,M,M)
    CAL PROMAT (IMT, IM, API, M, M, M)
    GO TO 50
6	  DO 7 I=1,M
    DO 7 J=1, M
    IF (I-J) 9, 8, 9
8	  IF (B(I,I)-1) 11,10,11
10	U(I,J)=1.
    GO TO 7
11	U(I,J)=0.
9	  IF (B(I,I)-1) 12,11,12
12	U(I,J)=-IM(I,J)
7	  CONTINUE
    CALL TRAM (U, UT, M, M)
    CALL PROMAT (UT, A, C1, M, M, M)
    CALL PROMAT (C1, U, C, M, M, M)
    RO=1
22  P=0
    DO 14 I=I11, I12
    IF (B (I,I)-1) 14,15,14
15  P=P+1
    Q=1
    DO 14 J=I11, I12
    IF (B (J,J)-1) 14,16,14
16  IF (RO-1) 17, 18, 17
17  C (I,J)=D(P,Q)
    GO TO 19
18  D(P,Q)=C(I,J)
19	Q=Q+1
14  CONTINUE
    IF (RO-1) 21, 20, 21
20	Q1=Q-1
    CALL DIAG (D, IM, P, RANG2, I21,I22)
    CALL TRAM (IM, IMT, P,P)
    CALL PROMAT (IMT, IM, D, P,P, P)
    RO=2
    GO TO 22
21	CALL PROMAT (U,C,C1, M,M,M)
    CALL PROMAT (C1, UT, API, M,M, M)
50	IF (M-N) 23,24,23
23  CALL PROMAT (API, LT, LPI, M, M, N)
    GO TO 25
24  CALL PROMAT (LT, API, LPI, R, M, M)
25  RETURN
    END
