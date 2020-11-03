    SUBROUTINE TRAN (A,B,M,N)
    DIMENSION A(M,N),B(N,M)
    DO 1 I=1,M
    DO 1 J=1,N
1   B(J,I)=A(I,J)
    RETURN
    END
