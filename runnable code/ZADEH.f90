    SUBROUTINE ZADEH (A,N,ALFA)
    DIMENSION A(N,N),AK(9,9),E(9,9),UN(9,9),ALFA(N)
    DO 3 I=1,N
    DO 3 J=1,N
    IF(I-J)10,11,10
10  UN(I,J)=0.
    AK(I,J)=0.
    GO TO 3
11  UN (I,J)=1.
    AK(I,J)=1.
3   CONTINUE
    DO 1 M=1,N
    M1=N-M+1
    DO 5 I=1,N
    DO 5 J=1,N
    E(I,J)=0.
    DO 5 L=1,N
5   E(I,J)=E(I,J)+A(I,L)*AK(L,J)
    TRAC=O.
    DO 6 I=1,N
6   TRAC=TRAC+E(I,I)
    ALFA(M1)=-(1./M)*TRAC
    ALFA1=ALFA(M1)
    IF(M-N) 2,50,50
2   DO 1 I=1,N
    DO 1 J=1,N
1   AK(I,J)=ALFA1*UN(I,J)+E(I,J)
50  RETURN
    END
