    SUBROUTINE DIAG(A,B,N,RO,I1,I2)
    INTEGER RO
    DIMENSION A(N,N) ,B(N,N),R(9),D(9)
    CALL UNIT(B,N)
    DO 1 I=1, N
    R(I)=O,
1   D(I)=A(I,I)
    DO 12 K=1,N
    EDM=D(1)
    L=1
    DO 3 I=2,N
    IF(EDM-D(I)) 4,3,3
4   EDM=D(I)
    L=I
3   CONTINUE
    IF(EDM-O.1E-04) 5,6,6
6   DO 7 J=1,N
    A(L,J)=A(L,J)/SQRT(EDM)
    A(J,L)=A(L,J)
7   B(L,J)=B(L,J)/SQRT(EDM)
    A(L,L)=1.
    DO 8 I=1,N
    IF(I-L) 9,8,9
9   DO 8 J=1,N
    A(I,J)=A(I,J)-A(L,I)*A(L,J)
    B(I,J)=B(I,J)-B(L,J)*A(L,I)
8   CONTINUE
    DO 1O I=1,N
    IF(I-L) 11,1O,11
11  A(L,I)=0.
10  CONTINUE
    R(L)=-1.
    DO 12 I=1,N
12  D(I)=A(I,I)+R(I)
5   RO=O.
    DO 14 I=1,N
14  RO=RO-R(I)
    I=1
17  IF(R(I)) 15,16,15
16  I=I+1
    GO TO 17
15  I1=I
    I=N
20  IF (R(I)) 18,19,18
19  I=I-1
    GO TO 20
18  I2=I
    RETURN
    END
