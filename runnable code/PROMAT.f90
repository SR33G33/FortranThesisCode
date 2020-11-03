    SUBROUTINE PROMAT (A,B,C,M,L,N)
    DIMESION A(M,L),B (L,N),C(M,N)
    DO 1 I=1,M
    DO 1 J=1,N
    C(1,J)=0,
    DO 1 K=1,L
1   C(1,J)=C(I.J)+A(I,K)*B(K,J)
    RETURN
    END
